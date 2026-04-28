#' Join walk / drive isochrone membership for ACS-weighted access simulation
#'
#' @param data Tract-level table with `GEOID`, population, ACS commute fields.
#' @param pnts_c,pnts_w Car / walk pharmacy `sf` with `query` and `within_isochrone`.
#' @param iso_c,iso_w Matching isochrone `sf` layers (`GEOID` column).
#'
#' @return Long table keyed by `query`, `GEOID`, `support` (`"K"` drive, `"L"` walk).
#' @export
join_iso <- function(data, pnts_c, pnts_w, iso_c, iso_w) {
  pnts_w %>%
    dplyr::select(.data$query, .data$within_isochrone) %>%
    tidyr::unnest(.data$within_isochrone) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(
      iso_w %>%
        sf::st_drop_geometry() %>%
        dplyr::select(.data$GEOID) %>%
        dplyr::mutate(within_isochrone = dplyr::row_number()),
      by = "within_isochrone"
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(within_iso_w = 1L) %>%
    dplyr::rename(within_isochrone_w = .data$within_isochrone) %>%
    dplyr::full_join(
      pnts_c %>%
        dplyr::select(.data$query, .data$within_isochrone) %>%
        tidyr::unnest(.data$within_isochrone) %>%
        sf::st_drop_geometry() %>%
        dplyr::left_join(
          iso_c %>%
            sf::st_drop_geometry() %>%
            dplyr::select(.data$GEOID) %>%
            dplyr::mutate(within_isochrone = dplyr::row_number()),
          by = "within_isochrone"
        ) %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(within_iso_c = 1L) %>%
        dplyr::rename(within_isochrone_c = .data$within_isochrone) %>%
        dplyr::select(.data$query, .data$within_iso_c, .data$within_isochrone_c),
      by = c("query", "within_isochrone_w" = "within_isochrone_c")
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("within_iso"), ~ tidyr::replace_na(.x, 0L))
    ) %>%
    dplyr::left_join(data, by = "GEOID") %>%
    dplyr::select(
      .data$query, .data$GEOID,
      .data$within_iso_c, .data$within_iso_w,
      .data$population,
      .data$estimate_car_truck_van, .data$moe_car_truck_van,
      .data$estimate_total_trans, .data$moe_total_trans
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
    dplyr::distinct(.data$query, .data$GEOID, .data$support, .keep_all = TRUE)
}

#' @rdname join_iso
#' @export
join_iso2 <- function(data, pnts_c, pnts_w, iso_c, iso_w) {
  pnts_w %>%
    dplyr::select(.data$query, .data$within_isochrone) %>%
    tidyr::unnest(.data$within_isochrone) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(
      iso_w %>%
        sf::st_drop_geometry() %>%
        dplyr::select(.data$GEOID) %>%
        dplyr::mutate(within_isochrone = dplyr::row_number()),
      by = "within_isochrone"
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(within_iso_w = 1L) %>%
    dplyr::rename(within_isochrone_w = .data$within_isochrone) %>%
    dplyr::full_join(
      pnts_c %>%
        dplyr::select(.data$query, .data$within_isochrone) %>%
        tidyr::unnest(.data$within_isochrone) %>%
        sf::st_drop_geometry() %>%
        dplyr::left_join(
          iso_c %>%
            sf::st_drop_geometry() %>%
            dplyr::select(.data$GEOID) %>%
            dplyr::mutate(within_isochrone = dplyr::row_number()),
          by = "within_isochrone"
        ) %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(within_iso_c = 1L) %>%
        dplyr::rename(within_isochrone_c = .data$within_isochrone) %>%
        dplyr::select(.data$query, .data$within_iso_c, .data$within_isochrone_c),
      by = c("query", "within_isochrone_w" = "within_isochrone_c")
    ) %>%
    dplyr::mutate(
      dplyr::across(dplyr::starts_with("within_iso"), ~ tidyr::replace_na(.x, 0L))
    ) %>%
    dplyr::left_join(data, by = "GEOID") %>%
    dplyr::select(
      .data$query, .data$GEOID,
      .data$within_iso_c, .data$within_iso_w,
      .data$population,
      .data$estimate_car_truck_van, .data$moe_car_truck_van,
      .data$estimate_total_trans, .data$moe_total_trans
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
    dplyr::distinct(.data$query, .data$GEOID, .data$support, .keep_all = TRUE)
}

#' Draw one truncated-normal ACS realization of drive / commute counts
#'
#' @param data Output of [join_iso()] (wide columns required).
#'
#' @export
acs_gen <- function(data) {
  data %>%
    dplyr::select(
      .data$GEOID, .data$population,
      .data$estimate_car_truck_van, .data$moe_car_truck_van,
      .data$estimate_total_trans, .data$moe_total_trans
    ) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$GEOID) %>%
    dplyr::mutate(
      go_to_work = truncnorm::rtruncnorm(
        n = 1L,
        mean = .data$estimate_total_trans,
        sd = .data$moe_total_trans / 1.645,
        a = 1
      ),
      drive_to_work = truncnorm::rtruncnorm(
        n = 1L,
        mean = .data$estimate_car_truck_van,
        sd = .data$moe_car_truck_van / 1.645,
        a = 0,
        b = .data$go_to_work
      ),
      p = .data$drive_to_work / .data$go_to_work
    ) %>%
    dplyr::filter(is.finite(.data$p)) %>%
    stats::na.omit() %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$GEOID, .data$p) %>%
    dplyr::right_join(data, by = "GEOID") %>%
    dplyr::mutate(
      support_prop = dplyr::if_else(.data$support == "K", .data$p, 1 - .data$p)
    )
}

#' Step-1 ratio \eqn{R} from `support_prop`
#'
#' @export
R_func <- function(data) {
  data %>%
    dplyr::group_by(.data$query) %>%
    dplyr::mutate(R = sum(.data$population * .data$support_prop, na.rm = TRUE)^(-1)) %>%
    stats::na.omit() %>%
    dplyr::ungroup()
}

#' Step-2 accessibility \eqn{A} (per 10k)
#'
#' @export
A_func <- function(data) {
  data %>%
    dplyr::group_by(.data$GEOID) %>%
    dplyr::summarize(
      A = sum(.data$R, na.rm = TRUE) * 10000,
      p = dplyr::first(.data$p),
      .groups = "drop"
    ) %>%
    stats::na.omit()
}

#' Linearly weighted combination access: Monte Carlo over ACS MOE (\code{lwc_access})
#'
#' @param data Tract attributes (see [join_iso()]).
#' @inheritParams join_iso
#' @param n_sims Number of simulation draws.
#' @param keep Character vector of `GEOID` values to retain in summaries.
#'
#' @export
lwc_access <- function(data, pnts_c, pnts_w, iso_c, iso_w, n_sims, keep) {
  keep_tbl <- dplyr::distinct(tibble::tibble(GEOID = keep), .data$GEOID)
  sim_long <- purrr::map_dfr(seq_len(n_sims), function(sim_id) {
    A_out <- join_iso(data = data,
                      pnts_c = pnts_c,
                      pnts_w = pnts_w,
                      iso_c = iso_c,
                      iso_w = iso_w) %>%
      acs_gen() %>%
      R_func() %>%
      A_func() %>%
      dplyr::select(.data$GEOID, .data$A, .data$p) %>%
      dplyr::distinct(.data$GEOID, .keep_all = TRUE)
    keep_tbl %>%
      dplyr::left_join(A_out, by = "GEOID") %>%
      dplyr::mutate(
        sim = sim_id,
        A = tidyr::replace_na(.data$A, 0),
        p = tidyr::replace_na(.data$p, 0)
      )
  })
  sim_long %>%
    dplyr::group_by(.data$GEOID) %>%
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
      .data$GEOID, .data$median_A,
      .data$q0.025, .data$q0.975,
      .data$mean_p, .data$median_p, .data$q0.025_p, .data$q0.975_p
    )
}

#' City-level median access per simulation
#'
#' @inheritParams lwc_access
#'
#' @export
lwc_access_global <- function(data, pnts_c, pnts_w, iso_c, iso_w, n_sims, keep) {
  keep_tbl <- dplyr::distinct(tibble::tibble(GEOID = keep), .data$GEOID)
  purrr::map_dfr(seq_len(n_sims), function(sim_id) {
    A_out <- join_iso(data = data,
                      pnts_c = pnts_c,
                      pnts_w = pnts_w,
                      iso_c = iso_c,
                      iso_w = iso_w) %>%
      acs_gen() %>%
      R_func() %>%
      A_func() %>%
      dplyr::select(.data$GEOID, .data$A, .data$p) %>%
      dplyr::distinct(.data$GEOID, .keep_all = TRUE)
    keep_tbl %>%
      dplyr::left_join(A_out, by = "GEOID") %>%
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

#' One-shot ACS point estimates (\code{lwc_access_fixed})
#'
#' @inheritParams join_iso
#'
#' @export
lwc_access_fixed <- function(data, pnts_c, pnts_w, iso_c, iso_w) {
  join_iso2(data = data,
            pnts_c = pnts_c,
            pnts_w = pnts_w,
            iso_c = iso_c,
            iso_w = iso_w) %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$GEOID) %>%
    dplyr::mutate(p = .data$estimate_car_truck_van / .data$estimate_total_trans) %>%
    dplyr::filter(is.finite(.data$p)) %>%
    stats::na.omit() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      support_prop = dplyr::if_else(.data$support == "K", .data$p, 1 - .data$p)
    ) %>%
    R_func() %>%
    A_func() %>%
    dplyr::mutate(A = tidyr::replace_na(.data$A, 0)) %>%
    dplyr::select(.data$GEOID, .data$A, .data$p) %>%
    dplyr::distinct()
}
