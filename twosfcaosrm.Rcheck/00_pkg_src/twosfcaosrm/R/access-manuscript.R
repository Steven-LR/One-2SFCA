#' Step-ratio access scores (\eqn{R = 1/\sum P}, \eqn{A} per 10\,000)
#'
#' For each supply site, \eqn{R = 1/\sum P} over demand zones whose isochrone
#' contains that site (using `population`). For each zone, \eqn{A = \sum R \times
#' 10000}. Inputs match the usual pipeline: `pnts` with list column
#' `within_isochrone` from [sf::st_within()], `iso` with one isochrone row per zone
#' (same row order as indices in `within_isochrone`), and `supply` with
#' per-site `population` for the join. For \eqn{R_j = S_j / \sum_i P_i}{Rj = Sj/sum(Pi)} use [access()] or
#' [twosfca_scores()].
#'
#' @param pnts `sf` of supplies with `within_isochrone` (list column) and
#'   `query` (or `supply_id`).
#' @param iso `sf` or data with tract id column `GEOID` (or `geoid_col`).
#' @param supply Attribute table merged by supply id (must include `population` for demand weights).
#' @param access_name Name for the output column (e.g. `"access_5"`).
#' @param supply_id Column name for pharmacy / facility id on `pnts`.
#' @param geoid_col Tract id column on `iso` (default `"GEOID"`).
#'
#' @return A tibble with `geoid` and one column `access_name`.
#' @export
access_step_ratio <- function(pnts,
                              iso,
                              supply,
                              access_name,
                              supply_id = "query",
                              geoid_col = "GEOID") {
  pnts %>%
    dplyr::select(.data[[supply_id]], .data$within_isochrone) %>%
    tidyr::unnest(.data$within_isochrone) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(
      iso %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(
          within_isochrone = dplyr::row_number(),
          geoid_iso = .data[[geoid_col]]
        ),
      by = "within_isochrone"
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(supply, by = supply_id) %>%
    dplyr::group_by(.data[[supply_id]]) %>%
    dplyr::mutate(R = 1 / sum(.data$population, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$geoid_iso) %>%
    dplyr::summarize(
      A_sum = sum(.data$R, na.rm = TRUE) * 10000,
      .groups = "drop"
    ) %>%
    dplyr::rename(geoid = .data$geoid_iso) %>%
    dplyr::rename_with(~ access_name, .cols = .data$A_sum)
}

#' `gpt_access`: wide table of access by isochrone band (`isomax`)
#'
#' Expects `iso` to include an `isomax` column (as returned by some OSRM
#' isochrone workflows). If absent, a constant column `isomax = 1` is added.
#'
#' @inheritParams access_step_ratio
#' @param iso Isochrone `sf` with `GEOID` and `isomax`.
#'
#' @return Wide tibble: `geoid`, columns `paste0(access_name, "_", isomax)`.
#' @export
gpt_access_bands <- function(pnts,
                             iso,
                             supply,
                             access_name,
                             supply_id = "query",
                             geoid_col = "GEOID") {
  iso2 <- iso
  if (!"isomax" %in% names(iso2)) {
    iso2 <- dplyr::mutate(iso2, isomax = 1L)
  }
  result <- pnts %>%
    dplyr::select(.data[[supply_id]], .data$within_isochrone) %>%
    tidyr::unnest(.data$within_isochrone) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(
      iso2 %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(
          within_isochrone = dplyr::row_number(),
          geoid_iso = .data[[geoid_col]]
        ),
      by = "within_isochrone"
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(supply, by = supply_id) %>%
    dplyr::group_by(.data[[supply_id]]) %>%
    dplyr::mutate(R = 1 / sum(.data$population, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$geoid_iso, .data$isomax) %>%
    dplyr::mutate(A = sum(.data$R, na.rm = TRUE) * 10000) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$geoid_iso, .data$isomax, .data$A) %>%
    dplyr::distinct()
  result %>%
    tidyr::pivot_wider(
      names_from = .data$isomax,
      values_from = .data$A,
      names_prefix = paste0(access_name, "_")
    ) %>%
    dplyr::rename(geoid = .data$geoid_iso)
}

#' Fixed ACS point estimates for mode weights (`access_fixed_acs`)
#'
#' @param data Tract table with `GEOID`, `estimate_car_truck_van`, `estimate_total_trans`.
#' @param pnts,iso,supply As in [access_step_ratio()].
#' @param access_name Output column name.
#' @param support `"c"` uses drive share `p`; any other value uses `1 - p`.
#' @param supply_id Supply identifier column.
#' @param geoid_col Tract id on `iso`.
#'
#' @export
access_fixed_acs <- function(data,
                             pnts,
                             iso,
                             supply,
                             access_name,
                             support,
                             supply_id = "query",
                             geoid_col = "GEOID") {
  S <- pnts %>%
    dplyr::select(.data[[supply_id]], .data$within_isochrone) %>%
    tidyr::unnest(.data$within_isochrone) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(
      iso %>%
        sf::st_drop_geometry() %>%
        dplyr::mutate(
          within_isochrone = dplyr::row_number(),
          geoid_iso = .data[[geoid_col]]
        ),
      by = "within_isochrone"
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(supply, by = supply_id)
  mode_probs <- data %>%
    dplyr::select(.data$GEOID, .data$estimate_car_truck_van, .data$estimate_total_trans) %>%
    dplyr::mutate(
      p_raw = .data$estimate_car_truck_van / .data$estimate_total_trans,
      p_raw = dplyr::if_else(
        is.na(.data$p_raw) | is.infinite(.data$p_raw),
        NA_real_,
        .data$p_raw
      ),
      p = if (as.character(support) == "c") .data$p_raw else (1 - .data$p_raw)
    ) %>%
    dplyr::select(.data$GEOID, .data$p)
  S %>%
    dplyr::left_join(mode_probs, by = c("geoid_iso" = "GEOID")) %>%
    dplyr::group_by(.data[[supply_id]]) %>%
    dplyr::mutate(
      D = sum(.data$population * .data$p, na.rm = TRUE),
      R = dplyr::if_else(.data$D > 0, 1 / .data$D, 0)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$geoid_iso) %>%
    dplyr::mutate(A = sum(.data$R, na.rm = TRUE) * 10000) %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$geoid_iso, .data$A) %>%
    dplyr::distinct() %>%
    dplyr::rename(geoid = .data$geoid_iso) %>%
    dplyr::rename_with(~ access_name, .cols = .data$A)
}
