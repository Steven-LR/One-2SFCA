#' Batch isochrones from a local OSRM server
#'
#' Calls [osrm::osrmIsochrone()] for each demand location. Failed rows are
#' omitted unless `keep_failures` is `TRUE` (then `geometry` may be missing).
#'
#' @param x A data frame with longitude and latitude columns (see `lon_col`,
#'   `lat_col`).
#' @param lon_col,lat_col Column names for WGS84 coordinates.
#' @param breaks Passed to [osrm::osrmIsochrone()] (e.g. `seq(0, 5, by = 5)`
#'   for a 5-minute contour only).
#' @param osrm_base_url Base URL of the running OSRM service, e.g.
#'   `"http://127.0.0.1:5001/"`.
#' @param profile Routing profile, e.g. `"car"` or `"foot"`.
#' @param keep_failures If `FALSE`, rows with routing errors are dropped.
#'
#' @return An `sf` object (same rows as `x` that succeeded, or all if
#'   `keep_failures`, with isochrone geometry).
#' @export
#' @examples
#' \dontrun{
#' d <- data.frame(id = 1, lon = -74.0, lat = 40.7)
#' osrm_isochrones_batch(
#'   d, "lon", "lat",
#'   breaks = seq(0, 10, by = 10),
#'   osrm_base_url = "http://127.0.0.1:5002/",
#'   profile = "foot"
#' )
#' }
osrm_isochrones_batch <- function(x,
                                  lon_col,
                                  lat_col,
                                  breaks,
                                  osrm_base_url,
                                  profile,
                                  keep_failures = FALSE) {
  if (!inherits(x, "data.frame")) {
    rlang::abort("`x` must be a data frame.")
  }
  lon <- x[[lon_col]]
  lat <- x[[lat_col]]
  n <- nrow(x)
  geoms <- vector("list", n)
  for (i in seq_len(n)) {
    res <- try(
      osrm::osrmIsochrone(
        loc = c(lon[[i]], lat[[i]]),
        breaks = breaks,
        osrm.server = osrm_base_url,
        osrm.profile = profile
      ),
      silent = TRUE
    )
    geoms[[i]] <- if (inherits(res, "try-error")) NULL else res
  }
  if (!keep_failures) {
    ok <- vapply(geoms, function(g) !is.null(g), logical(1L))
    x <- x[ok, , drop = FALSE]
    geoms <- geoms[ok]
  }
  if (length(geoms) == 0L) {
    rlang::abort("All isochrone requests failed.")
  }
  first_ok <- which(!vapply(geoms, is.null, logical(1L)))[1]
  if (is.na(first_ok)) {
    rlang::abort("All isochrone requests failed.")
  }
  crs0 <- sf::st_crs(geoms[[first_ok]])
  out <- x
  out$geometry <- do.call(
    c,
    lapply(geoms, function(g) {
      if (is.null(g)) {
        return(sf::st_sfc(list(sf::st_polygon()), crs = crs0))
      }
      sf::st_geometry(g)[1]
    })
  )
  sf::st_as_sf(out, crs = crs0)
}

#' @rdname osrm_isochrones_batch
#' @param max_distance_meters Maximum network distance in meters (single break).
#' @export
osrm_isodistance_batch <- function(x,
                                   lon_col,
                                   lat_col,
                                   max_distance_meters,
                                   osrm_base_url,
                                   profile,
                                   keep_failures = FALSE) {
  lon <- x[[lon_col]]
  lat <- x[[lat_col]]
  n <- nrow(x)
  geoms <- vector("list", n)
  br <- c(0, max_distance_meters)
  for (i in seq_len(n)) {
    res <- try(
      osrm::osrmIsodistance(
        loc = c(lon[[i]], lat[[i]]),
        breaks = seq(br[1], br[2], by = br[2]),
        osrm.server = osrm_base_url,
        osrm.profile = profile
      ),
      silent = TRUE
    )
    geoms[[i]] <- if (inherits(res, "try-error")) NULL else res
  }
  if (!keep_failures) {
    ok <- vapply(geoms, function(g) !is.null(g), logical(1L))
    x <- x[ok, , drop = FALSE]
    geoms <- geoms[ok]
  }
  if (length(geoms) == 0L) {
    rlang::abort("All isodistance requests failed.")
  }
  first_ok <- which(!vapply(geoms, is.null, logical(1L)))[1]
  crs0 <- sf::st_crs(geoms[[first_ok]])
  out <- x
  out$geometry <- do.call(
    c,
    lapply(geoms, function(g) {
      if (is.null(g)) {
        return(sf::st_sfc(list(sf::st_polygon()), crs = crs0))
      }
      sf::st_geometry(g)[1]
    })
  )
  sf::st_as_sf(out, crs = crs0)
}

#' Isochrones in the style used in workflow scripts (one ring per row)
#'
#' Convenience wrapper: builds a single travel-time ring at `minutes` (via
#' `breaks = seq(0, minutes, by = minutes))`, drops to an `sf` polygon layer,
#' and optionally reprojects.
#'
#' @param df Input data frame.
#' @param lat_col,lon_col Latitude / longitude column **names** (character).
#' @param minutes One positive number (e.g. `5` for a five-minute shell).
#' @inheritParams osrm_isochrones_batch
#' @param crs Output CRS; passed to [sf::st_transform()]. Use `NULL` to skip.
#'
#' @return `sf` object with original columns plus polygon geometry.
#' @export
isochrones_func <- function(df,
                            lat_col,
                            lon_col,
                            minutes,
                            osrm_base_url,
                            profile,
                            crs = 4326) {
  if (!all(c(lat_col, lon_col) %in% names(df))) {
    rlang::abort("`df` must contain `lat_col` and `lon_col`.")
  }
  br <- seq(0, minutes, by = minutes)
  lat <- df[[lat_col]]
  lon <- df[[lon_col]]
  iso <- purrr::map2(lon, lat, function(x, y) {
    tryCatch(
      osrm::osrmIsochrone(
        loc = c(x, y),
        breaks = br,
        osrm.server = osrm_base_url,
        osrm.profile = profile
      ),
      error = function(e) NULL
    )
  })
  dropped <- df %>%
    dplyr::mutate(nested_iso = iso) %>%
    dplyr::filter(!vapply(.data$nested_iso, is.null, logical(1L)))
  if (nrow(dropped) == 0L) {
    rlang::abort("All isochrone requests failed.")
  }
  geom_col <- attr(dropped$nested_iso[[which(!vapply(dropped$nested_iso, is.null, TRUE))[1]]], "sf_column")
  flat <- dropped %>%
    sf::st_drop_geometry() %>%
    tidyr::unnest(.data$nested_iso)
  g <- flat[[geom_col]]
  flat[[geom_col]] <- NULL
  out <- sf::st_sf(flat, geometry = g)
  if (!is.null(crs)) {
    out <- sf::st_transform(out, crs)
  }
  out
}

#' Pairwise travel times via OSRM table service
#'
#' Row `i` uses origin (`lng1`,`lat1`) and destination (`lng2`,`lat2`).
#' Implemented with [osrm::osrmTable()] (diagonal of the duration matrix).
#'
#' @param data A data frame.
#' @param lat1,lng1,lat2,lng2 Column names as character strings.
#' @param profile,server Passed through as `osrm.profile` and `osrm.server`.
#'
#' @return `data` with a new numeric column `duration` (seconds).
#' @export
duration_url <- function(data,
                         lat1,
                         lng1,
                         lat2,
                         lng2,
                         profile,
                         server) {
  out <- data
  src <- sf::st_as_sf(
    out,
    coords = c(lng1, lat1),
    crs = 4326,
    remove = FALSE
  )
  dst <- sf::st_as_sf(
    out,
    coords = c(lng2, lat2),
    crs = 4326,
    remove = FALSE
  )
  tab <- osrm::osrmTable(
    src = src,
    dst = dst,
    measure = "duration",
    osrm.server = server,
    osrm.profile = profile
  )
  dmat <- tab$durations
  if (!is.matrix(dmat) || nrow(dmat) != ncol(dmat)) {
    rlang::abort("Unexpected `osrmTable` shape for pairwise routing.")
  }
  out$duration <- diag(dmat)
  out
}

#' Pairwise route distances via OSRM table service
#'
#' @inheritParams duration_url
#'
#' @return `data` with a new column `distance` (meters unless OSRM returns other;
#'   matches **osrm** package behavior).
#' @export
distance_url <- function(data,
                         lat1,
                         lng1,
                         lat2,
                         lng2,
                         profile,
                         server) {
  out <- data
  src <- sf::st_as_sf(
    out,
    coords = c(lng1, lat1),
    crs = 4326,
    remove = FALSE
  )
  dst <- sf::st_as_sf(
    out,
    coords = c(lng2, lat2),
    crs = 4326,
    remove = FALSE
  )
  tab <- osrm::osrmTable(
    src = src,
    dst = dst,
    measure = "distance",
    osrm.server = server,
    osrm.profile = profile
  )
  dmat <- tab$distances
  if (!is.matrix(dmat) || nrow(dmat) != ncol(dmat)) {
    rlang::abort("Unexpected `osrmTable` shape for pairwise routing.")
  }
  out$distance <- diag(dmat)
  out
}
