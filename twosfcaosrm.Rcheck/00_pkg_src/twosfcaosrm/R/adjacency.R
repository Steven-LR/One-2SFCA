#' Build spatial neighborhood structure for Moran / local indices
#'
#' @param map `sf` polygons.
#' @param style `"queen"` contiguity or `"distance"` threshold graph.
#' @param order Lag order for queen (see [spdep::nblag()]).
#' @param dist_threshold Upper distance bound when `style = "distance"`.
#'
#' @return List with binary matrix `W`, reduced `W_small`, `nb_list` (row-standardized [spdep::nb2listw()]), etc.
#' @export
get_adj <- function(map,
                    style = c("queen", "distance"),
                    order = 1L,
                    dist_threshold = NULL) {
  style <- match.arg(style)
  if (!inherits(map, "sf")) {
    rlang::abort("`map` must be an sf object.")
  }
  if (style == "queen") {
    nb1 <- spdep::poly2nb(map, queen = TRUE)
    if (order > 1L) {
      nb_lag <- spdep::nblag(nb1, maxlag = order)
      nb_use <- spdep::nblag_cumul(nb_lag)
    } else {
      nb_use <- nb1
    }
  } else {
    if (is.null(dist_threshold)) {
      rlang::abort("`dist_threshold` required when style = 'distance'.")
    }
    pts <- suppressWarnings(sf::st_centroid(sf::st_geometry(map)))
    coords <- sf::st_coordinates(pts)
    nb_use <- spdep::dnearneigh(coords, d1 = 0, d2 = dist_threshold, longlat = FALSE)
  }
  B <- as.matrix(spdep::nb2mat(nb_use, style = "B", zero.policy = TRUE))
  W <- spdep::nb2listw(nb_use, style = "W", zero.policy = TRUE)
  zero_i <- which(rowSums(B) == 0L)
  if (length(zero_i) > 0L) {
    B_small <- B[-zero_i, -zero_i, drop = FALSE]
  } else {
    B_small <- B
  }
  keep <- setdiff(seq_len(nrow(map)), zero_i)
  list(
    W = B,
    W_small = B_small,
    nb_use = nb_use,
    zero_i = zero_i,
    keep = keep,
    nb_list = W
  )
}
