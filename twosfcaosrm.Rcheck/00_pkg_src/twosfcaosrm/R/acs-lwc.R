#' Join walk / drive isochrone membership for ACS-weighted access simulation
#'
#' @param data Tract-level table: `tract_id_col`, `pop_col`, and four ACS columns
#'   (`acs_num_est`, `acs_num_moe`, `acs_den_est`, `acs_den_moe`).
#' @param pnts_c,pnts_w `sf` point layers with `supply_id_col` and `within_isochrone`.
#' @param iso_c,iso_w Isochrone `sf` objects with `tract_id_col` aligned to row order
#'   of `within_isochrone` indices.
#' @param supply_id_col Facility identifier (default `"query"`).
#' @param tract_id_col Small-area id in `data` and on isochrones (default `"GEOID"`).
#' @param pop_col Population column on `data` (default `"population"`).
#' @param acs_num_est,acs_num_moe ACS estimate and MOE for the **numerator** count
#'   (e.g. vehicle commuters).
#' @param acs_den_est,acs_den_moe ACS estimate and MOE for the **denominator** count
#'   (e.g. workers traveling to work).
#'
#' @return Long table with `supply_id_col`, `tract_id_col`, `support` (`"K"` = car row,
#'   `"L"` = walk row), and ACS columns.
#' @export
join_iso <- function(data,
                     pnts_c,
                     pnts_w,
                     iso_c,
                     iso_w,
                     supply_id_col = "query",
                     tract_id_col = "GEOID",
                     pop_col = "population",
                     acs_num_est = "estimate_car_truck_van",
                     acs_num_moe = "moe_car_truck_van",
                     acs_den_est = "estimate_total_trans",
                     acs_den_moe = "moe_total_trans") {
  sid <- supply_id_col
  tid <- tract_id_col
  acs_cols <- c(acs_num_est, acs_num_moe, acs_den_est, acs_den_moe)
  .require_cols <- function(tbl, nms) {
    miss <- setdiff(nms, names(tbl))
    if (length(miss)) {
      rlang::abort(paste0("Missing column(s): ", paste(miss, collapse = ", ")))
    }
  }
  .require_cols(pnts_w, c(sid, "within_isochrone"))
  .require_cols(pnts_c, c(sid, "within_isochrone"))
  .require_cols(sf::st_drop_geometry(iso_w), tid)
  .require_cols(sf::st_drop_geometry(iso_c), tid)
  .require_cols(data, c(tid, pop_col, acs_cols))

  pnts_w %>%
    dplyr::select(dplyr::all_of(c(sid, "within_isochrone"))) %>%
    tidyr::unnest(.data$within_isochrone) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(
      iso_w %>%
        sf::st_drop_geometry() %>%
        dplyr::select(dplyr::all_of(tid)) %>%
        dplyr::mutate(within_isochrone = dplyr::row_number()),
      by = "within_isochrone"
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(within_iso_w = 1L) %>%
    dplyr::rename(within_isochrone_w = .data$within_isochrone) %>%
    dplyr::full_join(
      pnts_c %>%
        dplyr::select(dplyr::all_of(c(sid, "within_isochrone"))) %>%
        tidyr::unnest(.data$within_isochrone) %>%
        sf::st_drop_geometry() %>%
        dplyr::left_join(
          iso_c %>%
            sf::st_drop_geometry() %>%
            dplyr::select(dplyr::all_of(tid)) %>%
            dplyr::mutate(within_isochrone = dplyr::row_number()),
          by = "within_isochrone"
        ) %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(within_iso_c = 1L) %>%
        dplyr::rename(within_isochrone_c = .data$within_isochrone) %>%
        dplyr::select(dplyr::all_of(c(sid, "within_iso_c", "within_isochrone_c"))),
      by = c(sid, "within_isochrone_w" = "within_isochrone_c")
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("within_iso"), ~ tidyr::replace_na(.x, 0L))
    ) %>%
    dplyr::left_join(data, by = tid) %>%
    dplyr::select(
      dplyr::all_of(c(sid, tid)),
      .data$within_iso_c,
      .data$within_iso_w,
      dplyr::all_of(c(pop_col, acs_cols))
    ) %>%
    tidyr::pivot_longer(
      c(.data$within_iso_c, .data$within_iso_w),
      names_to = "support",
      values_to = "support_boolean"
    ) %>%
    dplyr::filter(.data$support_boolean == 1L) %>%
    dplyr::mutate(
      support = dplyr::if_else(.data$support == "within_iso_c", "K", "L")
    ) %>%
    dplyr::select(-.data$support_boolean) %>%
    dplyr::distinct(.data[[sid]], .data[[tid]], .data$support, .keep_all = TRUE)
}

#' @rdname join_iso
#' @export
join_iso2 <- function(data,
                      pnts_c,
                      pnts_w,
                      iso_c,
                      iso_w,
                      supply_id_col = "query",
                      tract_id_col = "GEOID",
                      pop_col = "population",
                      acs_num_est = "estimate_car_truck_van",
                      acs_num_moe = "moe_car_truck_van",
                      acs_den_est = "estimate_total_trans",
                      acs_den_moe = "moe_total_trans") {
  join_iso(
    data, pnts_c, pnts_w, iso_c, iso_w,
    supply_id_col = supply_id_col,
    tract_id_col = tract_id_col,
    pop_col = pop_col,
    acs_num_est = acs_num_est,
    acs_num_moe = acs_num_moe,
    acs_den_est = acs_den_est,
    acs_den_moe = acs_den_moe
  )
}

#' Draw one truncated-normal realization of ACS counts and mode share \eqn{p}
#'
#' For each tract, draws denominator and nested numerator from truncated
#' normals with mean = published estimate and SD = MOE/1.645 (ACS 5-year MOEs
#' are approximate 90% margins). Then computes \eqn{p =} numerator/denominator
#' and attaches `support_prop` for car (`K`) vs walk (`L`) rows.
#'
#' @param data Output of [join_iso()].
#' @inheritParams join_iso
#'
#' @export
acs_gen <- function(data,
                    tract_id_col = "GEOID",
                    pop_col = "population",
                    acs_num_est = "estimate_car_truck_van",
                    acs_num_moe = "moe_car_truck_van",
                    acs_den_est = "estimate_total_trans",
                    acs_den_moe = "moe_total_trans") {
  tid <- tract_id_col
  draw_cols <- c(tid, pop_col, acs_num_est, acs_num_moe, acs_den_est, acs_den_moe)
  data %>%
    dplyr::select(dplyr::all_of(draw_cols)) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data[[tid]]) %>%
    dplyr::mutate(
      .den_draw = truncnorm::rtruncnorm(
        n = 1L,
        mean = .data[[acs_den_est]],
        sd = .data[[acs_den_moe]] / 1.645,
        a = 1
      ),
      .num_draw = truncnorm::rtruncnorm(
        n = 1L,
        mean = .data[[acs_num_est]],
        sd = .data[[acs_num_moe]] / 1.645,
        a = 0,
        b = .data$.den_draw
      ),
      p = .data$.num_draw / .data$.den_draw
    ) %>%
    dplyr::filter(is.finite(.data$p)) %>%
    stats::na.omit() %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c(tid, "p"))) %>%
    dplyr::right_join(data, by = tid) %>%
    dplyr::mutate(
      support_prop = dplyr::if_else(.data$support == "K", .data$p, 1 - .data$p)
    )
}

#' Step-1 ratio \eqn{R} from `support_prop`
#'
#' @param data Long table with `supply_id_col`, `tract_id_col`, `population`, `support_prop`.
#' @inheritParams join_iso
#'
#' @export
R_func <- function(data,
                   supply_id_col = "query",
                   tract_id_col = "GEOID",
                   pop_col = "population") {
  sid <- supply_id_col
  data %>%
    dplyr::group_by(.data[[sid]]) %>%
    dplyr::mutate(R = sum(.data[[pop_col]] * .data$support_prop, na.rm = TRUE)^(-1)) %>%
    stats::na.omit() %>%
    dplyr::ungroup()
}

#' Step-2 accessibility \eqn{A} (per 10k)
#'
#' @param data Table with `R`, `tract_id_col`, and `p`.
#' @inheritParams join_iso
#'
#' @export
A_func <- function(data, tract_id_col = "GEOID") {
  tid <- tract_id_col
  data %>%
    dplyr::group_by(.data[[tid]]) %>%
    dplyr::summarize(
      A = sum(.data$R, na.rm = TRUE) * 10000,
      p = dplyr::first(.data$p),
      .groups = "drop"
    ) %>%
    stats::na.omit()
}

#' Monte Carlo accessibility with propagated ACS uncertainty (LWC)
#'
#' Repeats: build mode-specific support from car/walk isochrones ([join_iso()]),
#' draw ACS-consistent stochastic counts ([acs_gen()]), recompute \eqn{R} ([R_func()])
#' and tract accessibility \eqn{A} ([A_func()]). Summarizes the \eqn{n_{\mathrm{sims}}}
#' realizations per tract: mean/median \eqn{A}, 2.5% and 97.5% quantiles, and the same
#' for the realized mode-share \eqn{p}. Use this when accessibility depends on decomposing
#' population demand using published ACS estimates with MOEs (e.g. commute mode shares).
#'
#' @param data Tract table (see [join_iso()]).
#' @inheritParams join_iso
#' @param n_sims Number of Monte Carlo draws.
#' @param keep Character vector of `tract_id_col` values to force into each simulation
#'   summary (tracts missing from a draw get \eqn{A = 0} and \eqn{p = 0} after
#'   `left_join`). If `NULL`, uses `unique(data[[tract_id_col]])`.
#'
#' @return Tibble with columns `tract_id_col`, `mean_A`, `median_A`, `q0.025`, `q0.975`,
#'   `mean_p`, `median_p`, `q0.025_p`, `q0.975_p`. Rename as needed (e.g. `median_A` → `lwc_A5`).
#'
#' @export
lwc_access <- function(data,
                       pnts_c,
                       pnts_w,
                       iso_c,
                       iso_w,
                       n_sims,
                       keep = NULL,
                       supply_id_col = "query",
                       tract_id_col = "GEOID",
                       pop_col = "population",
                       acs_num_est = "estimate_car_truck_van",
                       acs_num_moe = "moe_car_truck_van",
                       acs_den_est = "estimate_total_trans",
                       acs_den_moe = "moe_total_trans") {
  tid <- tract_id_col
  if (is.null(keep)) {
    keep <- unique(data[[tid]])
  }
  keep_tbl <- dplyr::distinct(tibble::tibble(!!tid := keep), .data[[tid]])
  sim_long <- purrr::map_dfr(seq_len(n_sims), function(sim_id) {
    A_out <- join_iso(
      data = data,
      pnts_c = pnts_c,
      pnts_w = pnts_w,
      iso_c = iso_c,
      iso_w = iso_w,
      supply_id_col = supply_id_col,
      tract_id_col = tract_id_col,
      pop_col = pop_col,
      acs_num_est = acs_num_est,
      acs_num_moe = acs_num_moe,
      acs_den_est = acs_den_est,
      acs_den_moe = acs_den_moe
    ) %>%
      acs_gen(
        tract_id_col = tract_id_col,
        pop_col = pop_col,
        acs_num_est = acs_num_est,
        acs_num_moe = acs_num_moe,
        acs_den_est = acs_den_est,
        acs_den_moe = acs_den_moe
      ) %>%
      R_func(
        supply_id_col = supply_id_col,
        tract_id_col = tract_id_col,
        pop_col = pop_col
      ) %>%
      A_func(tract_id_col = tract_id_col) %>%
      dplyr::select(dplyr::all_of(c(tid, "A", "p"))) %>%
      dplyr::distinct(.data[[tid]], .keep_all = TRUE)
    keep_tbl %>%
      dplyr::left_join(A_out, by = tid) %>%
      dplyr::mutate(
        sim = sim_id,
        A = tidyr::replace_na(.data$A, 0),
        p = tidyr::replace_na(.data$p, 0)
      )
  })
  sim_long %>%
    dplyr::group_by(.data[[tid]]) %>%
    dplyr::summarize(
      mean_A = mean(.data$A),
      median_A = stats::median(.data$A),
      q0.025 = stats::quantile(.data$A, 0.025),
      q0.975 = stats::quantile(.data$A, 0.975),
      mean_p = mean(.data$p),
      median_p = stats::median(.data$p),
      q0.025_p = stats::quantile(.data$p, 0.025),
      q0.975_p = stats::quantile(.data$p, 0.975),
      .groups = "drop"
    ) %>%
    dplyr::select(
      dplyr::all_of(tid),
      .data$mean_A,
      .data$median_A,
      .data$q0.025,
      .data$q0.975,
      .data$mean_p,
      .data$median_p,
      .data$q0.025_p,
      .data$q0.975_p
    )
}

#' City-level median access per simulation draw
#'
#' @inheritParams lwc_access
#'
#' @export
lwc_access_global <- function(data,
                              pnts_c,
                              pnts_w,
                              iso_c,
                              iso_w,
                              n_sims,
                              keep = NULL,
                              supply_id_col = "query",
                              tract_id_col = "GEOID",
                              pop_col = "population",
                              acs_num_est = "estimate_car_truck_van",
                              acs_num_moe = "moe_car_truck_van",
                              acs_den_est = "estimate_total_trans",
                              acs_den_moe = "moe_total_trans") {
  tid <- tract_id_col
  if (is.null(keep)) {
    keep <- unique(data[[tid]])
  }
  keep_tbl <- dplyr::distinct(tibble::tibble(!!tid := keep), .data[[tid]])
  purrr::map_dfr(seq_len(n_sims), function(sim_id) {
    A_out <- join_iso(
      data = data,
      pnts_c = pnts_c,
      pnts_w = pnts_w,
      iso_c = iso_c,
      iso_w = iso_w,
      supply_id_col = supply_id_col,
      tract_id_col = tract_id_col,
      pop_col = pop_col,
      acs_num_est = acs_num_est,
      acs_num_moe = acs_num_moe,
      acs_den_est = acs_den_est,
      acs_den_moe = acs_den_moe
    ) %>%
      acs_gen(
        tract_id_col = tract_id_col,
        pop_col = pop_col,
        acs_num_est = acs_num_est,
        acs_num_moe = acs_num_moe,
        acs_den_est = acs_den_est,
        acs_den_moe = acs_den_moe
      ) %>%
      R_func(
        supply_id_col = supply_id_col,
        tract_id_col = tract_id_col,
        pop_col = pop_col
      ) %>%
      A_func(tract_id_col = tract_id_col) %>%
      dplyr::select(dplyr::all_of(c(tid, "A", "p"))) %>%
      dplyr::distinct(.data[[tid]], .keep_all = TRUE)
    keep_tbl %>%
      dplyr::left_join(A_out, by = tid) %>%
      dplyr::mutate(
        sim = sim_id,
        A = tidyr::replace_na(.data$A, 0),
        p = tidyr::replace_na(.data$p, 0)
      )
  }) %>%
    dplyr::group_by(.data$sim) %>%
    dplyr::summarize(
      n = dplyr::n(),
      median_A_city = stats::median(.data$A),
      median_p_city = stats::median(.data$p),
      .groups = "drop"
    )
}

#' Point-estimate LWC (no MOE draws): ACS means only
#'
#' @inheritParams join_iso
#'
#' @export
lwc_access_fixed <- function(data,
                             pnts_c,
                             pnts_w,
                             iso_c,
                             iso_w,
                             supply_id_col = "query",
                             tract_id_col = "GEOID",
                             pop_col = "population",
                             acs_num_est = "estimate_car_truck_van",
                             acs_num_moe = "moe_car_truck_van",
                             acs_den_est = "estimate_total_trans",
                             acs_den_moe = "moe_total_trans") {
  tid <- tract_id_col
  join_iso(
    data = data,
    pnts_c = pnts_c,
    pnts_w = pnts_w,
    iso_c = iso_c,
    iso_w = iso_w,
    supply_id_col = supply_id_col,
    tract_id_col = tract_id_col,
    pop_col = pop_col,
    acs_num_est = acs_num_est,
    acs_num_moe = acs_num_moe,
    acs_den_est = acs_den_est,
    acs_den_moe = acs_den_moe
  ) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data[[tid]]) %>%
    dplyr::mutate(p = .data[[acs_num_est]] / .data[[acs_den_est]]) %>%
    dplyr::filter(is.finite(.data$p)) %>%
    stats::na.omit() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      support_prop = dplyr::if_else(.data$support == "K", .data$p, 1 - .data$p)
    ) %>%
    R_func(
      supply_id_col = supply_id_col,
      tract_id_col = tract_id_col,
      pop_col = pop_col
    ) %>%
    A_func(tract_id_col = tract_id_col) %>%
    dplyr::mutate(A = tidyr::replace_na(.data$A, 0)) %>%
    dplyr::select(dplyr::all_of(c(tid, "A", "p"))) %>%
    dplyr::distinct()
}
