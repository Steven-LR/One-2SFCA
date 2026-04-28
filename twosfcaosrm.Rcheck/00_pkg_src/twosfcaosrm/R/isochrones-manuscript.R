#' Manuscript-style isochrones (`purrr::map2` + `unnest`)
#'
#' Matches the common pattern in thesis code: drop geometry, nest isochrone
#' `sf` objects per row, `unnest`, set geometry. Column names `lat` and `lng`
#' are **strings** (e.g. `"latitude"`, `"longitude"`).
#'
#' @param data An `sf` object (geometry dropped before OSRM calls).
#' @param lat,lng Names of latitude/longitude columns.
#' @param minutes Single break at `minutes` (see `breaks = seq(0, minutes, by = minutes)`).
#' @param server OSRM base URL.
#' @param profile OSRM profile.
#'
#' @return `sf` with one polygon per successful request.
#' @export
isochrones_purrr <- function(data,
                             lat,
                             lng,
                             minutes,
                             server,
                             profile) {
  d0 <- if (inherits(data, "sf")) sf::st_drop_geometry(data) else data
  lngv <- d0[[lng]]
  latv <- d0[[lat]]
  br <- seq(0, as.numeric(minutes), by = as.numeric(minutes))
  nested <- purrr::map2(lngv, latv, function(lon, la) {
    tryCatch(
      osrm::osrmIsochrone(
        loc = c(lon, la),
        breaks = br,
        osrm.server = server,
        osrm.profile = profile
      ),
      error = function(e) NULL
    )
  })
  geom_col <- NULL
  first <- which(!vapply(nested, is.null, logical(1L)))[1]
  if (is.na(first)) {
    rlang::abort("All isochrone requests failed.")
  }
  geom_col <- attr(nested[[first]], "sf_column")
  d0$iso <- nested
  d0 <- d0[!vapply(nested, is.null, logical(1L)), , drop = FALSE]
  if (nrow(d0) == 0L) {
    rlang::abort("All isochrone requests failed.")
  }
  flat <- tidyr::unnest(d0, cols = "iso")
  g <- flat[[geom_col]]
  flat[[geom_col]] <- NULL
  flat$geometry <- g
  sf::st_as_sf(flat, crs = sf::st_crs(g))
}

#' Multi-band isochrones with `safely` (gpt-style)
#'
#' Builds breaks `seq(0, total_minutes, by = interval)` and stacks results with
#' `row_id` for joining back to attributes.
#'
#' @param data `sf` with coordinates in columns given by `lat` and `lng` (**unquoted**).
#' @param lat,lng bare column names.
#' @param total_minutes,interval Passed to `seq()`.
#' @param server,profile OSRM connection.
#'
#' @return `sf` with original attribute columns (no geometry from input) plus
#'   isochrone geometry and `row_id`.
#' @export
gpt_isochrones_func <- function(data,
                                lat,
                                lng,
                                total_minutes,
                                interval,
                                server,
                                profile) {
  lat <- rlang::ensym(lat)
  lng <- rlang::ensym(lng)
  data_no_geom <- data %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(row_id = dplyr::row_number())
  lng_vals <- dplyr::pull(data_no_geom, !!lng)
  lat_vals <- dplyr::pull(data_no_geom, !!lat)
  the_breaks <- seq(0, total_minutes, by = interval)
  safe_iso <- purrr::safely(function(lon, latv) {
    osrm::osrmIsochrone(
      loc = c(lon, latv),
      breaks = the_breaks,
      osrm.server = server,
      osrm.profile = profile
    )
  }, otherwise = NULL)
  out_list <- vector("list", nrow(data_no_geom))
  for (i in seq_len(nrow(data_no_geom))) {
    res <- safe_iso(lng_vals[i], lat_vals[i])$result
    if (!is.null(res)) {
      res$row_id <- i
      out_list[[i]] <- res
    }
  }
  pieces <- out_list[!vapply(out_list, is.null, logical(1L))]
  if (length(pieces) == 0L) {
    rlang::abort("All isochrone requests failed.")
  }
  result_sf <- do.call(rbind, pieces)
  dplyr::left_join(result_sf, data_no_geom, by = "row_id")
}
