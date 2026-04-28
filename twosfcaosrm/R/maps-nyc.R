#' Map tract data with NYC five-borough county filter (requires \pkg{tigris})
#'
#' @param map_shp `sf` with `GEOID` and `county` (or `county.x`).
#' @param data Tract attributes with `geoid`.
#' @param access One or more columns merged from `access` output.
#' @param A_name Bare name of access column to replace `NA` with 0 (used by `map_data`).
#' @param counties Character or numeric county **FIPS** to retain (defaults NYC boroughs).
#'
#' @name map_nyc_helpers
NULL

#' @rdname map_nyc_helpers
#' @export
map_data <- function(map_shp, data, access, A_name,
                     counties = c(36005L, 36047L, 36061L, 36081L, 36085L)) {
  counties <- as.character(counties)
  an <- rlang::ensym(A_name)
  d <- data %>%
    dplyr::select(
      .data$geoid,
      .data$pharm_count,
      .data$pharm_den,
      .data$population,
      .data$ind,
      .data$SIR,
      .data$E
    ) %>%
    dplyr::distinct() %>%
    dplyr::left_join(access, by = "geoid") %>%
    dplyr::mutate(!!an := dplyr::if_else(is.na(!!an), 0, !!an))
  if (!requireNamespace("tigris", quietly = TRUE)) {
    rlang::abort("Install the tigris package to use map_data().")
  }
  out <- tigris::geo_join(
    map_shp,
    d,
    by_sp = "GEOID",
    by_df = "geoid",
    how = "inner"
  )
  cn <- if ("county" %in% names(out)) "county" else "county.x"
  dplyr::filter(out, as.character(.data[[cn]]) %in% counties)
}

#' @rdname map_nyc_helpers
#' @export
map_access_data <- function(map_shp, data, access,
                            counties = c(36005L, 36047L, 36061L, 36081L, 36085L)) {
  counties <- as.character(counties)
  d <- data %>%
    dplyr::select(
      .data$geoid,
      .data$pharm_count,
      .data$pharm_den,
      .data$population,
      .data$ind,
      .data$SIR,
      .data$E
    ) %>%
    dplyr::distinct() %>%
    dplyr::left_join(access, by = "geoid")
  if (!requireNamespace("tigris", quietly = TRUE)) {
    rlang::abort("Install the tigris package to use map_access_data().")
  }
  out <- tigris::geo_join(
    map_shp,
    d,
    by_sp = "GEOID",
    by_df = "geoid",
    how = "inner"
  )
  cn <- if ("county.x" %in% names(out)) "county.x" else "county"
  dplyr::filter(out, as.character(.data[[cn]]) %in% counties)
}

#' @rdname map_nyc_helpers
#' @export
map_access_data2 <- function(map_shp, access,
                             counties = c(36005L, 36047L, 36061L, 36081L, 36085L)) {
  counties <- as.character(counties)
  if (!requireNamespace("tigris", quietly = TRUE)) {
    rlang::abort("Install the tigris package to use map_access_data2().")
  }
  out <- tigris::geo_join(
    map_shp,
    access,
    by_sp = "GEOID",
    by_df = "geoid",
    how = "inner"
  )
  cn <- if ("county" %in% names(out)) "county" else "county.x"
  dplyr::filter(out, as.character(.data[[cn]]) %in% counties)
}

#' @rdname map_nyc_helpers
#' @param var_name Bare column name for fill.
#' @param color_option Passed to [ggplot2::scale_fill_viridis_d()].
#'
#' @export
plot_access_map <- function(data, var_name, color_option = "B") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    rlang::abort("Install ggplot2 to use plot_access_map().")
  }
  vn <- rlang::ensym(var_name)
  data <- data %>%
    dplyr::rename(A_g = !!vn) %>%
    dplyr::mutate(Ag_q = dplyr::case_when(
      .data$A_g == 0 ~ "0",
      .data$A_g > 0 & .data$A_g <= stats::quantile(.data$A_g, na.rm = TRUE)[2] ~ "1",
      .data$A_g > stats::quantile(.data$A_g, na.rm = TRUE)[2] &
        .data$A_g <= stats::quantile(.data$A_g, na.rm = TRUE)[3] ~ "2",
      .data$A_g > stats::quantile(.data$A_g, na.rm = TRUE)[3] &
        .data$A_g <= stats::quantile(.data$A_g, na.rm = TRUE)[4] ~ "3",
      .data$A_g > stats::quantile(.data$A_g, na.rm = TRUE)[4] ~ "4"
    ))
  ggplot2::ggplot(data) +
    ggplot2::geom_sf(ggplot2::aes(fill = .data$Ag_q)) +
    ggplot2::scale_fill_viridis_d(
      option = color_option,
      name = as.character(vn),
      na.value = "gray60"
    )
}
