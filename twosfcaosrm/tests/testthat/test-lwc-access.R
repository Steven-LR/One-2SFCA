#' Toy walk (small) vs drive (large overlapping) catchments, mock ACS commuter
#' counts/MOEs, and `within_isochrone` from point-in-polygon tests.

walk_drive_toy_fixture <- function() {
  crs <- 3857L
  box_rect <- function(xmin, xmax, ymin, ymax) {
    matrix(
      c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
      ncol = 2L,
      byrow = TRUE
    )
  }

  iso_w <- sf::st_sf(
    GEOID = c("T1", "T2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(box_rect(-620, -380, -120, 120))),
      sf::st_polygon(list(box_rect(380, 620, -120, 120))),
      crs = crs
    )
  )

  iso_c <- sf::st_sf(
    GEOID = c("T1", "T2"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(box_rect(-1500, 600, -400, 400))),
      sf::st_polygon(list(box_rect(-600, 1500, -400, 400))),
      crs = crs
    )
  )

  ph <- sf::st_sf(
    query = "ph1",
    geometry = sf::st_sfc(sf::st_point(c(500, 0)), crs = crs)
  )
  idx_w <- sort(as.integer(sf::st_within(ph, iso_w)[[1L]]))
  idx_c <- sort(as.integer(sf::st_within(ph, iso_c)[[1L]]))

  stopifnot(idx_w[[1L]] %in% idx_c)

  ph_w <- ph
  ph_w$within_isochrone <- list(idx_w)
  ph_c <- ph
  ph_c$within_isochrone <- list(idx_c)

  tract_tbl <- data.frame(
    GEOID = c("T1", "T2"),
    population = c(1000L, 600L),
    estimate_car_truck_van = c(420, 288),
    moe_car_truck_van = c(65, 50),
    estimate_total_trans = c(760, 450),
    moe_total_trans = c(80, 58),
    stringsAsFactors = FALSE
  )

  stopifnot(
    all(tract_tbl$estimate_car_truck_van < tract_tbl$estimate_total_trans),
    all(tract_tbl$moe_car_truck_van >= 2),
    all(tract_tbl$moe_total_trans >= 2)
  )

  list(
    data = tract_tbl,
    iso_w = iso_w,
    iso_c = iso_c,
    pnts_w = ph_w,
    pnts_c = ph_c,
    idx_w = idx_w,
    idx_c = idx_c
  )
}

test_that("walk catchments nest inside overlapping drive catchments (toy polygons)", {
  fix <- walk_drive_toy_fixture()
  expect_true(length(fix$idx_w) < length(fix$idx_c))
})

test_that("join_iso attaches tract ids for drive-only overlaps (GEOID complete)", {
  fix <- walk_drive_toy_fixture()
  ji <- join_iso(
    data = fix$data,
    pnts_c = fix$pnts_c,
    pnts_w = fix$pnts_w,
    iso_c = fix$iso_c,
    iso_w = fix$iso_w
  )

  expect_true(all(!is.na(ji$GEOID)))

  ji_w <- dplyr::filter(ji, .data$support == "L")
  ji_t1_car <- dplyr::filter(ji, .data$GEOID == "T1")
  ji_t2 <- dplyr::filter(ji, .data$GEOID == "T2")

  expect_equal(ji_w$GEOID, "T2")
  expect_equal(ji_t1_car$support, "K")

  expect_setequal(unique(ji_t2$support), c("K", "L"))

  expect_true(all(stats::na.omit(ji$population) %in% fix$data$population))
})

test_that("lwc_access_fixed and lwc_access run on toy walk/drive + mock ACS counts", {
  fix <- walk_drive_toy_fixture()

  fixed <- lwc_access_fixed(
    fix$data,
    fix$pnts_c,
    fix$pnts_w,
    fix$iso_c,
    fix$iso_w
  )

  fixed_t1 <- fixed$A[fixed$GEOID == "T1"]
  fixed_t2 <- fixed$A[fixed$GEOID == "T2"]
  expect_true(is.finite(fixed_t1))
  expect_true(is.finite(fixed_t2))

  p_car_t1 <- fix$data$estimate_car_truck_van[
    fix$data$GEOID == "T1"
  ] /
    fix$data$estimate_total_trans[fix$data$GEOID == "T1"]

  denom_one_supplier <-
    fix$data$population[fix$data$GEOID == "T2"] +
    fix$data$population[fix$data$GEOID == "T1"] * unname(p_car_t1)

  R_ph <- 1 / denom_one_supplier
  expect_equal(unname(as.numeric(fixed_t1)), R_ph * 10000)
  expect_equal(unname(as.numeric(fixed_t2)), 2 * R_ph * 10000)

  set.seed(2026)
  lw <- lwc_access(
    fix$data,
    fix$pnts_c,
    fix$pnts_w,
    fix$iso_c,
    fix$iso_w,
    n_sims = 300L,
    tract_id_col = "GEOID"
  )

  expect_equal(sort(lw$GEOID), sort(fix$data$GEOID))
  expect_true(all(stats::complete.cases(lw)))

  lw1 <- dplyr::filter(lw, .data$GEOID == "T1")
  lw2 <- dplyr::filter(lw, .data$GEOID == "T2")

  expect_true(all(
    dplyr::between(lw2$median_A, lw2$q0.025, lw2$q0.975),
    dplyr::between(lw1$median_A, lw1$q0.025, lw1$q0.975)
  ))

  expect_true(lw2$q0.975[[1]] > lw2$q0.025[[1]])
})
