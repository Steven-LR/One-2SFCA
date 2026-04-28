# OSRM Route service (HTTP): one request per OD pair ------------------------
# Used when you want the same behavior as incremental curl/docker checks.
# Slower than [duration_url()] / [distance_url()] (full osrmTable) for many rows.

#' @importFrom rlang .data
NULL

route_v1_url <- function(lng1, lat1, lng2, lat2, profile, server) {
  base <- sub("/$", "", server)
  paste0(
    base, "/route/v1/", profile, "/",
    lng1, ",", lat1, ";", lng2, ",", lat2,
    "?overview=false&steps=true"
  )
}

#' One-row network distance via OSRM Route HTTP API (miles by default)
#'
#' @param i Row index into vectors `lat1`, `lng1`, `lat2`, `lng2`.
#' @param scale_fac Meters per **one mile** (multiply route meters by `1/scale_fac`).
#' @inheritParams duration_url_route
#' @return Numeric distance in miles if HTTP 200; otherwise a string starting with `"error"`.
#' @keywords internal
dis_url <- function(i,
                    scale_fac = 1.60934 * 1000,
                    lat1,
                    lng1,
                    lat2,
                    lng2,
                    profile,
                    server) {
  url <- route_v1_url(lng1[i], lat1[i], lng2[i], lat2[i], profile, server)
  response <- httr::GET(url)
  if (httr::status_code(response) == 200L) {
    route <- httr::content(response, as = "parsed")
    route$routes[[1]]$distance / scale_fac
  } else {
    NA_real_
  }
}

#' @rdname duration_url_route
#' @export
distance_url_route <- function(data,
                               scale_fac = 1.60934 * 1000,
                               lat1,
                               lng1,
                               lat2,
                               lng2,
                               profile,
                               server) {
  lng1v <- dplyr::pull(data, !!rlang::sym(lng1))
  lat1v <- dplyr::pull(data, !!rlang::sym(lat1))
  lng2v <- dplyr::pull(data, !!rlang::sym(lng2))
  lat2v <- dplyr::pull(data, !!rlang::sym(lat2))
  n <- nrow(data)
  mat <- vector("list", n)
  for (i in seq_len(n)) {
    mat[[i]] <- tryCatch(
      dis_url(
        i = i, scale_fac = scale_fac,
        lat1 = lat1v, lng1 = lng1v, lat2 = lat2v, lng2 = lng2v,
        profile = profile, server = server
      ),
      error = function(e) NA_real_
    )
  }
  dplyr::mutate(data, distance = as.numeric(unlist(mat)))
}

#' One-row duration via OSRM Route HTTP API (seconds)
#'
#' @param i Row index.
#' @inheritParams distance_url_route
#' @return Duration in seconds, or error string.
#' @keywords internal
dur_url <- function(i,
                    lat1,
                    lng1,
                    lat2,
                    lng2,
                    profile,
                    server) {
  url <- route_v1_url(lng1[i], lat1[i], lng2[i], lat2[i], profile, server)
  response <- httr::GET(url)
  if (httr::status_code(response) == 200L) {
    route <- httr::content(response, as = "parsed")
    route$routes[[1]]$duration
  } else {
    NA_real_
  }
}

#' Pairwise durations via OSRM **Route** API (loop; one HTTP GET per row)
#'
#' Unlike [duration_url()], which uses [osrm::osrmTable()] on the full table,
#' this matches manuscript `duration_url` behavior against a Docker OSRM
#' `route/v1` endpoint—useful for throttling or debugging single pairs.
#'
#' @param data A data frame.
#' @param lat1,lng1,lat2,lng2 Character names of latitude / longitude columns.
#' @param profile Routing profile (e.g. `"driving"`, `"foot"` — must match OSRM).
#' @param server Base URL (with or without trailing slash).
#'
#' @return `data` with a `duration` column (may be coerced; failed rows become `NA`).
#' @export
duration_url_route <- function(data,
                               lat1,
                               lng1,
                               lat2,
                               lng2,
                               profile,
                               server) {
  lng1v <- dplyr::pull(data, !!rlang::sym(lng1))
  lat1v <- dplyr::pull(data, !!rlang::sym(lat1))
  lng2v <- dplyr::pull(data, !!rlang::sym(lng2))
  lat2v <- dplyr::pull(data, !!rlang::sym(lat2))
  n <- nrow(data)
  mat <- vector("list", n)
  for (i in seq_len(n)) {
    mat[[i]] <- tryCatch(
      dur_url(
        i = i,
        lat1 = lat1v, lng1 = lng1v, lat2 = lat2v, lng2 = lng2v,
        profile = profile, server = server
      ),
      error = function(e) NA_real_
    )
  }
  dplyr::mutate(data, duration = as.numeric(unlist(mat)))
}

#' Distance and duration from Route API with annotations
#'
#' @inheritParams duration_url_route
#' @param meters_to_miles Divide OSRM meters by this to get miles (default miles per km * m).
#' @param sec_to_min Divide seconds by this for minutes.
#'
#' @return `data` with `distance` (miles) and `duration` (minutes).
#' @export
proximity_url_route <- function(data,
                                meters_to_miles = 1.60934 * 1000,
                                sec_to_min = 60,
                                lat1,
                                lng1,
                                lat2,
                                lng2,
                                profile,
                                server) {
  base <- sub("/$", "", server)
  lng1v <- dplyr::pull(data, !!rlang::sym(lng1))
  lat1v <- dplyr::pull(data, !!rlang::sym(lat1))
  lng2v <- dplyr::pull(data, !!rlang::sym(lng2))
  lat2v <- dplyr::pull(data, !!rlang::sym(lat2))
  n <- nrow(data)
  mat <- vector("list", n)
  for (i in seq_len(n)) {
    url <- paste0(
      base, "/route/v1/", profile, "/",
      lng1v[i], ",", lat1v[i], ";", lng2v[i], ",", lat2v[i],
      "?overview=false&steps=true&annotations=true"
    )
    mat[[i]] <- tryCatch(
      {
        response <- httr::GET(url)
        if (httr::status_code(response) != 200L) {
          return(list(NA_real_, NA_real_))
        }
        route <- httr::content(response, as = "parsed")
        list(
          route$routes[[1]]$distance / meters_to_miles,
          route$routes[[1]]$duration / sec_to_min
        )
      },
      error = function(e) list(NA_real_, NA_real_)
    )
  }
  dplyr::mutate(
    data,
    distance = vapply(mat, `[[`, numeric(1), 1L),
    duration = vapply(mat, `[[`, numeric(1), 2L)
  )
}
