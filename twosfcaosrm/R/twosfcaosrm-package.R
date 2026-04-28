#' Local 2SFCA + OSRM helpers
#'
#' Build floating catchments with a **self-hosted OSRM** instance (typically
#' via Docker), attach facilities to demand zones, and compute two-step
#' floating catchment (2SFCA) scores.
#'
#' The R client for HTTP routing is the **osrm** package (not "orsmr"). It
#' expects `osrm.server` pointing at your container, e.g.
#' `http://127.0.0.1:5001/`.
#'
#' @keywords internal
"_PACKAGE"
NULL
