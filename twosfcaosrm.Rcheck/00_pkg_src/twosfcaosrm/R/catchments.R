#' Build demand--supply pairs from point-in-polygon tests
#'
#' For each supply site, finds all demand zones (isochrone polygons) whose
#' geometry contains that site after an optional common projection. This matches
#' the common workflow: isochrones are built from demand centroids; pharmacies
#' (supplies) are tested with [sf::st_within()].
#'
#' @param supply_sf `sf` points with a unique supply identifier column.
#' @param isochrones_sf `sf` polygons with a demand identifier column (e.g.
#'   tract `GEOID`).
#' @param supply_id_col Name of the supply id column in `supply_sf`.
#' @param demand_id_col Name of the demand id column in `isochrones_sf`.
#' @param crs CRS to use for predicates (projected CRS recommended). If `NULL`,
#'   both layers must already share the same CRS.
#'
#' @return A tibble with columns `supply_id`, `demand_id`.
#' @export
match_supplies_to_isochrones <- function(supply_sf,
                                         isochrones_sf,
                                         supply_id_col = "query",
                                         demand_id_col = "GEOID",
                                         crs = 3857) {
  if (!supply_id_col %in% names(supply_sf)) {
    rlang::abort("`supply_sf` must contain `supply_id_col`.")
  }
  if (!demand_id_col %in% names(isochrones_sf)) {
    rlang::abort("`isochrones_sf` must contain `demand_id_col`.")
  }
  if (!is.null(crs)) {
    supply_p <- sf::st_transform(supply_sf, crs)
    iso_p <- sf::st_transform(isochrones_sf, crs)
  } else {
    if (!identical(sf::st_crs(supply_sf), sf::st_crs(isochrones_sf))) {
      rlang::abort("If `crs` is NULL, `supply_sf` and `isochrones_sf` must share a CRS.")
    }
    supply_p <- supply_sf
    iso_p <- isochrones_sf
  }
  wids <- sf::st_within(supply_p, iso_p)
  sid <- supply_p[[supply_id_col]]
  did <- iso_p[[demand_id_col]]
  rows <- lapply(seq_along(wids), function(i) {
    jj <- as.integer(wids[[i]])
    if (length(jj) == 0L) {
      return(data.frame(
        supply_id = sid[[i]],
        demand_id = NA,
        stringsAsFactors = FALSE
      ))
    }
    data.frame(
      supply_id = sid[[i]],
      demand_id = did[jj],
      stringsAsFactors = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

#' Prepare OD tables after `st_within` list columns (nearest-neighbor style prep)
#'
#' Expands `within_isochrone` integer indices (row positions in `zone_sf`) into
#' long form and attaches zone centroids as `longitude_c` / `latitude_c` for use
#' with [duration_url()] or [distance_url()].
#'
#' @param supply_with_zones A data frame or `sf` with `query` (or `supply_id_col`)
#'   and `within_isochrone` list column from [sf::st_within()].
#' @param zone_sf `sf` polygons whose row order defines `within_isochrone`
#'   indices (same as passed to `st_within`).
#' @param supply_lookup Optional additional columns merged by supply id.
#' @param supply_id_col Supply key column name.
#' @return A tibble ready for pairwise OSRM calls.
#' @export
iso_nearest_data_prep <- function(supply_with_zones,
                                  zone_sf,
                                  supply_lookup = NULL,
                                  supply_id_col = "query") {
  if (!supply_id_col %in% names(supply_with_zones)) {
    rlang::abort("`supply_with_zones` must contain the supply id column.")
  }
  if (!"within_isochrone" %in% names(supply_with_zones)) {
    rlang::abort("`supply_with_zones` must contain `within_isochrone`.")
  }
  cent <- sf::st_centroid(sf::st_geometry(zone_sf))
  cc <- sf::st_coordinates(cent)
  zdf <- sf::st_drop_geometry(zone_sf)
  zdf$.within_row <- seq_len(nrow(zdf))
  zdf$longitude_c <- cc[, "X"]
  zdf$latitude_c <- cc[, "Y"]

  sw <- supply_with_zones
  if (inherits(sw, "sf")) {
    sw <- sf::st_drop_geometry(sw)
  }
  out <- sw %>%
    tidyr::unnest(.data$within_isochrone) %>%
    dplyr::left_join(zdf, by = c("within_isochrone" = ".within_row"))
  if (!is.null(supply_lookup)) {
    lk <- if (inherits(supply_lookup, "sf")) {
      sf::st_drop_geometry(supply_lookup)
    } else {
      supply_lookup
    }
    out <- dplyr::left_join(out, lk, by = supply_id_col, suffix = c("", ".lk"))
  }
  out
}
