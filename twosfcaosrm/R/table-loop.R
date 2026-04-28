# Incremental osrmTable (2-point loc) — row loop --------------------------------

duration_one_table <- function(data,
                               i,
                               osrm.server,
                               measure = "duration",
                               osrm.profile = "car",
                               lat1,
                               lng1,
                               lat2,
                               lng2) {
  row <- data[i, , drop = FALSE]
  o <- sf::st_as_sf(
    row,
    coords = c(lng1, lat1),
    crs = 4326,
    remove = FALSE
  )
  d <- sf::st_as_sf(
    row,
    coords = c(lng2, lat2),
    crs = 4326,
    remove = FALSE
  )
  tab <- osrm::osrmTable(
    src = o,
    dst = d,
    measure = measure,
    osrm.server = osrm.server,
    osrm.profile = osrm.profile
  )
  m <- tab[[if (measure == "distance") "distances" else "durations"]]
  if (!is.matrix(m) || nrow(m) < 1L || ncol(m) < 1L) {
    return(NA_real_)
  }
  diag(as.matrix(m))[1L]
}

#' Pairwise durations using **osrmTable** in a `for` loop (manuscript style)
#'
#' One OSRM table request per row. Use [duration_url()] for a single matrix call
#' when all pairs fit in one request.
#'
#' @inheritParams duration_url
#' @param osrm.server,measure,osrm.profile Passed to [osrm::osrmTable()].
#'
#' @export
duration_table_loop <- function(data,
                                osrm.server = "http://127.0.0.1:5001/",
                                measure = "duration",
                                osrm.profile = "car",
                                lat1,
                                lng1,
                                lat2,
                                lng2) {
  n <- nrow(data)
  mat <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    mat[i] <- tryCatch(
      duration_one_table(
        data, i,
        osrm.server = osrm.server,
        measure = measure,
        osrm.profile = osrm.profile,
        lat1 = lat1, lng1 = lng1, lat2 = lat2, lng2 = lng2
      ),
      error = function(e) NA_real_
    )
  }
  dplyr::mutate(data, duration = mat)
}

distance_one_table <- function(data,
                               i,
                               osrm.server,
                               osrm.profile = "car",
                               scale_fac = 1.60934 * 1000,
                               lat1,
                               lng1,
                               lat2,
                               lng2) {
  duration_one_table(
    data, i,
    osrm.server = osrm.server,
    measure = "distance",
    osrm.profile = osrm.profile,
    lat1 = lat1, lng1 = lng1, lat2 = lat2, lng2 = lng2
  ) / scale_fac
}

#' @rdname duration_table_loop
#' @param scale_fac Divide OSRM meters by this for miles (default matches manuscript).
#' @export
distance_table_loop <- function(data,
                                osrm.server = "http://127.0.0.1:5001/",
                                osrm.profile = "car",
                                scale_fac = 1.60934 * 1000,
                                lat1,
                                lng1,
                                lat2,
                                lng2) {
  n <- nrow(data)
  mat <- rep(NA_real_, n)
  for (i in seq_len(n)) {
    mat[i] <- tryCatch(
      distance_one_table(
        data, i,
        osrm.server = osrm.server,
        osrm.profile = osrm.profile,
        scale_fac = scale_fac,
        lat1 = lat1, lng1 = lng1, lat2 = lat2, lng2 = lng2
      ),
      error = function(e) NA_real_
    )
  }
  dplyr::mutate(data, distance = mat)
}
