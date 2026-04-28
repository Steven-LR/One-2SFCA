#' Prepare nearest-facility OD rows (manuscript `iso_nearest_data_prep`)
#'
#' `iso` must contain **tract** `longitude` / `latitude` (e.g. centroids).
#' `supply` must contain the same supply key and **facility** coordinates.
#'
#' @param pnts `sf` with supply key and `within_isochrone`.
#' @param iso Non-spatial attributes merged from isochrones (with `geoid_col`,
#'   `longitude`, `latitude`).
#' @param supply Facility coordinates.
#' @param id_col Supply key (default `"name"`).
#' @param geoid_col Tract id column on `iso`.
#'
#' @export
iso_nearest_data_prep_named <- function(pnts,
                                        iso,
                                        supply,
                                        id_col = "name",
                                        geoid_col = "GEOID") {
  iso_tbl <- iso %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(
      within_isochrone = dplyr::row_number(),
      GEOID = .data[[geoid_col]]
    ) %>%
    dplyr::rename(longitude_c = .data$longitude, latitude_c = .data$latitude)
  sup_tbl <- supply %>%
    dplyr::transmute(
      !!id_col := .data[[id_col]],
      longitude_p = .data$longitude,
      latitude_p = .data$latitude
    )
  pnts %>%
    dplyr::select(.data[[id_col]], .data$within_isochrone) %>%
    tidyr::unnest(.data$within_isochrone) %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(iso_tbl, by = "within_isochrone") %>%
    dplyr::left_join(sup_tbl, by = id_col)
}

#' Nearest OD durations (uses [duration_url()] = `osrmTable`)
#'
#' @inheritParams iso_nearest_data_prep_named
#' @param lat1,lng1 Column names for centroid (typically `"latitude_c"`, `"longitude_c"`).
#' @param lat2,lng2 Column names for facility (here `"latitude_p"`, `"longitude_p"`).
#'
#' @export
iso_nearest <- function(pnts,
                        iso,
                        supply,
                        lat1 = "latitude_c",
                        lat2 = "latitude_p",
                        lng1 = "longitude_c",
                        lng2 = "longitude_p",
                        profile,
                        server,
                        id_col = "name") {
  prep <- iso_nearest_data_prep_named(pnts, iso, supply, id_col = id_col)
  duration_url(
    data = prep,
    lat1 = lat1,
    lat2 = lat2,
    lng1 = lng1,
    lng2 = lng2,
    profile = profile,
    server = server
  ) %>%
    dplyr::select(
      dplyr::all_of(id_col),
      .data$longitude_p, .data$latitude_p,
      .data$GEOID,
      .data$longitude_c, .data$latitude_c,
      dplyr::any_of("population"),
      .data$duration
    )
}
