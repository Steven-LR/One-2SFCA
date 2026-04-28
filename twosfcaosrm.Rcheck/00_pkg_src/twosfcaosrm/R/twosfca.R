#' Two-step floating catchment accessibility scores
#'
#' Implements the standard **2SFCA** logic (Luo & Wang 2003):
#'
#' **Step 1**: For each supply site \eqn{j}, compute the ratio
#' \deqn{R_j = S_j / \sum_{i \in D_j} P_i}
#' where \eqn{D_j} is the set of demand zones whose catchment contains supply
#' \eqn{j}, \eqn{P_i} is demand (population), and \eqn{S_j} is supply capacity.
#'
#' **Step 2**: For each demand zone \eqn{i},
#' \deqn{A_i = \sum_{j \in S_i} R_j}
#' where \eqn{S_i} is the set of supplies inside zone \eqn{i}'s catchment.
#'
#' @param pairs Long-form table with columns `supply_id` and `demand_id` (as
#'   returned by [match_supplies_to_isochrones()]).
#' @param demand_tbl Table of demand: must include the same id column name as
#'   `pairs$demand_id` uses for joining (typically name passed as
#'   `demand_key_col`) and a population column.
#' @param supply_tbl Table of supplies: must include supply ids matching
#'   `pairs$supply_id`. If `supply_col` is `NULL` or missing from `supply_tbl`,
#'   every supply uses `default_supply`.
#' @param demand_key_col Name of the demand zone id column in `demand_tbl`
#'   (e.g. `"GEOID"`).
#' @param supply_key_col Name of the supply id column in `supply_tbl`.
#' @param pop_col Name of the population / demand column in `demand_tbl`.
#' @param supply_col Name of the supply-capacity column in `supply_tbl`, or
#'   `NULL` to use `default_supply`.
#' @param default_supply Scalar capacity when `supply_col` is not used.
#'
#' @return A tibble with columns `demand_key_col` (same name as input) and `A`.
#' @references
#' Luo, W., & Wang, F. (2003). Measures of spatial accessibility to health care
#' in a GIS environment. *International Journal of Geographical Information
#' Science*, 17(1), 15--27.
#' @export
twosfca_scores <- function(pairs,
                           demand_tbl,
                           supply_tbl,
                           demand_key_col = "GEOID",
                           supply_key_col = "query",
                           pop_col = "population",
                           supply_col = NULL,
                           default_supply = 1) {
  req_c <- c("supply_id", "demand_id")
  if (!all(req_c %in% names(pairs))) {
    rlang::abort("`pairs` must contain `supply_id` and `demand_id`.")
  }
  if (!demand_key_col %in% names(demand_tbl)) {
    rlang::abort("`demand_tbl` is missing `demand_key_col`.")
  }
  if (!pop_col %in% names(demand_tbl)) {
    rlang::abort("`demand_tbl` is missing `pop_col`.")
  }
  if (!supply_key_col %in% names(supply_tbl)) {
    rlang::abort("`supply_tbl` is missing `supply_key_col`.")
  }

  pr <- pairs[!is.na(pairs$demand_id), , drop = FALSE]
  pr <- dplyr::distinct(pr, .data$supply_id, .data$demand_id)

  demand2 <- dplyr::transmute(
    demand_tbl,
    demand_id = .data[[demand_key_col]],
    ..pop = .data[[pop_col]]
  )

  if (!is.null(supply_col) && supply_col %in% names(supply_tbl)) {
    supply2 <- dplyr::transmute(
      supply_tbl,
      supply_id = .data[[supply_key_col]],
      ..sup = .data[[supply_col]]
    )
  } else {
    supply2 <- dplyr::transmute(
      supply_tbl,
      supply_id = .data[[supply_key_col]],
      ..sup = default_supply
    )
  }

  denom_tbl <- dplyr::left_join(pr, demand2, by = "demand_id") %>%
    dplyr::group_by(.data$supply_id) %>%
    dplyr::summarize(denom = sum(.data$..pop, na.rm = TRUE), .groups = "drop")

  r_tbl <- dplyr::left_join(denom_tbl, supply2, by = "supply_id") %>%
    dplyr::mutate(
      Rj = dplyr::if_else(
        .data$denom > 0,
        .data$..sup / .data$denom,
        NA_real_
      )
    )

  pr %>%
    dplyr::left_join(
      dplyr::select(r_tbl, .data$supply_id, .data$Rj),
      by = "supply_id"
    ) %>%
    dplyr::group_by(.data$demand_id) %>%
    dplyr::summarize(A = sum(.data$Rj, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(!!demand_key_col := .data$demand_id)
}

#' @rdname twosfca_scores
#' @export
twosfca_from_pairs <- twosfca_scores
