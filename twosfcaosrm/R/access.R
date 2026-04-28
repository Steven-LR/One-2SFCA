#' Join 2SFCA accessibility scores to demand polygons
#'
#' Runs [match_supplies_to_isochrones()], then [twosfca_scores()], and
#' left-joins the result to `isochrones` so you keep census-tract geometry.
#' This mirrors the **`access_*` pattern** used in manuscript workflows where
#' pharmacy points (`pnts`) carry a `st_within` match against isochrone
#' polygons.
#'
#' @seealso [access_step_ratio()] for the thesis-style ratio
#'   (\eqn{R=1/\sum P}, \eqn{A} per 10k) used with pharmacy data.
#'
#' @param pnts `sf` point layer of supplies (e.g. facilities). Must contain
#'   `supply_id_col`.
#' @param isochrones `sf` polygons with `demand_id_col`, `pop_col`, and any
#'   other attributes you wish to retain in the output.
#' @param supply Non-spatial attribute table for supplies (e.g. row-level
#'   capacity). Must contain `supply_id_col`. Rows need not match `pnts`
#'   one-to-one; unique supply ids are used.
#' @param name_col Name of the new numeric column (e.g. `"access_5"`).
#' @param demand_id_col Zone id on `isochrones` (e.g. `"GEOID"`).
#' @param supply_id_col Supply key on `pnts` and `supply` (e.g. `"query"` or
#'   `"name"`).
#' @param pop_col Demand weight (usually population).
#' @param crs_for_match Projected CRS for [sf::st_within()] (e.g. `3857` or a
#'   local state plane like `2263` for NYC-area work).
#' @param default_supply Capacity when no weight column is provided.
#' @param supply_weight_col Optional column name on `supply` for Step-1
#'   numerators \eqn{S_j}.
#'
#' @return `sf` object = `isochrones` plus one new column `name_col`.
#' @export
access <- function(pnts,
                   isochrones,
                   supply,
                   name_col,
                   demand_id_col = "GEOID",
                   supply_id_col = "query",
                   pop_col = "population",
                   crs_for_match = 3857,
                   default_supply = 1,
                   supply_weight_col = NULL) {
  if (!pop_col %in% names(isochrones)) {
    rlang::abort("`isochrones` must contain `pop_col` for demand weights.")
  }
  if (!supply_id_col %in% names(pnts)) {
    rlang::abort("`pnts` must contain `supply_id_col`.")
  }
  pairs <- match_supplies_to_isochrones(
    pnts,
    isochrones,
    supply_id_col = supply_id_col,
    demand_id_col = demand_id_col,
    crs = crs_for_match
  )
  d_tab <- if (inherits(isochrones, "sf")) {
    sf::st_drop_geometry(isochrones)
  } else {
    isochrones
  }
  s_tab <- if (inherits(supply, "sf")) {
    sf::st_drop_geometry(supply)
  } else {
    supply
  }
  s_tab <- dplyr::distinct(s_tab, .data[[supply_id_col]], .keep_all = TRUE)

  sc <- twosfca_scores(
    pairs,
    demand_tbl = d_tab,
    supply_tbl = s_tab,
    demand_key_col = demand_id_col,
    supply_key_col = supply_id_col,
    pop_col = pop_col,
    supply_col = supply_weight_col,
    default_supply = default_supply
  )
  names(sc)[names(sc) == "A"] <- name_col
  dplyr::left_join(isochrones, sc, by = demand_id_col)
}

#' @rdname access
#' @param ... Passed to [access()].
#' @export
#' @details
#' `gpt_access` is an alias kept for compatibility with older analysis
#' scripts; behavior is identical to [access()].
gpt_access <- function(...) {
  access(...)
}
