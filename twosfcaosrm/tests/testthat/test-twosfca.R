test_that("twosfca_scores matches hand-checked ratios", {
  pairs <- data.frame(
    supply_id = c("a", "a", "b"),
    demand_id = c("1", "2", "1"),
    stringsAsFactors = FALSE
  )
  demand_tbl <- data.frame(
    GEOID = c("1", "2"),
    population = c(100, 200),
    stringsAsFactors = FALSE
  )
  supply_tbl <- data.frame(
    query = c("a", "b"),
    stringsAsFactors = FALSE
  )
  res <- twosfca_scores(
    pairs,
    demand_tbl = demand_tbl,
    supply_tbl = supply_tbl,
    demand_key_col = "GEOID",
    supply_key_col = "query",
    pop_col = "population",
    supply_col = NULL,
    default_supply = 1
  )
  r_a <- 1 / 300
  r_b <- 1 / 100
  expect_equal(res$A[res$GEOID == "1"], r_a + r_b)
  expect_equal(res$A[res$GEOID == "2"], r_a)
})

test_that("match_supplies requires shared CRS when crs is NULL", {
  p <- sf::st_sfc(sf::st_point(c(0, 0)), crs = 4326)
  poly <- sf::st_buffer(sf::st_sfc(sf::st_point(c(0, 0)), crs = 3857), 1000)
  pts <- sf::st_sf(query = "x", geometry = p)
  pol <- sf::st_sf(GEOID = "A", geometry = poly)
  expect_error(
    match_supplies_to_isochrones(pts, pol, crs = NULL),
    "share a CRS"
  )
})

test_that("twosfca_from_pairs matches twosfca_scores", {
  pairs <- data.frame(
    supply_id = c("a", "b"),
    demand_id = c("1", "1"),
    stringsAsFactors = FALSE
  )
  demand_tbl <- data.frame(
    GEOID = c("1", "2"),
    population = c(50, 50),
    stringsAsFactors = FALSE
  )
  supply_tbl <- data.frame(query = c("a", "b"), stringsAsFactors = FALSE)
  s <- twosfca_scores(
    pairs, demand_tbl, supply_tbl,
    demand_key_col = "GEOID",
    supply_key_col = "query",
    pop_col = "population",
    supply_col = NULL,
    default_supply = 1
  )
  f <- twosfca_from_pairs(
    pairs, demand_tbl, supply_tbl,
    demand_key_col = "GEOID",
    supply_key_col = "query",
    pop_col = "population",
    supply_col = NULL,
    default_supply = 1
  )
  expect_equal(f, s)
})

test_that("match_supplies_to_isochrones returns expected pairs (overlapping zones)", {
  crs <- 3857
  b1 <- matrix(c(-200, -200, 200, -200, 200, 200, -200, 200, -200, -200),
    ncol = 2L, byrow = TRUE)
  b2 <- matrix(c(0, -200, 600, -200, 600, 200, 0, 200, 0, -200),
    ncol = 2L, byrow = TRUE)
  poly <- sf::st_sfc(sf::st_polygon(list(b1)), sf::st_polygon(list(b2)), crs = crs)
  zones <- sf::st_sf(
    GEOID = c("1", "2"),
    population = c(100, 200),
    geometry = poly
  )
  pts <- sf::st_sf(
    query = c("a", "b"),
    geometry = sf::st_sfc(
      sf::st_point(c(100, 0)),
      sf::st_point(c(-100, 0)),
      crs = crs
    )
  )
  pr <- match_supplies_to_isochrones(
    pts, zones, supply_id_col = "query",
    demand_id_col = "GEOID", crs = crs
  )
  pr <- pr[!is.na(pr$demand_id), , drop = FALSE]
  pr <- dplyr::distinct(pr)
  expect_setequal(paste(pr$supply_id, pr$demand_id),
    c("a 1", "a 2", "b 1"))
})

test_that("access merges same scores as twosfca_scores (overlapping zones)", {
  crs <- 3857
  b1 <- matrix(c(-200, -200, 200, -200, 200, 200, -200, 200, -200, -200),
    ncol = 2L, byrow = TRUE)
  b2 <- matrix(c(0, -200, 600, -200, 600, 200, 0, 200, 0, -200),
    ncol = 2L, byrow = TRUE)
  iso <- sf::st_sf(
    GEOID = c("1", "2"),
    population = c(100, 200),
    geometry = sf::st_sfc(
      sf::st_polygon(list(b1)),
      sf::st_polygon(list(b2)),
      crs = crs
    )
  )
  pnts <- sf::st_sf(
    query = c("a", "b"),
    geometry = sf::st_sfc(
      sf::st_point(c(100, 0)),
      sf::st_point(c(-100, 0)),
      crs = crs
    )
  )
  supply <- data.frame(query = c("a", "b"), stringsAsFactors = FALSE)
  pairs <- match_supplies_to_isochrones(pnts, iso, crs = crs)
  d_iso <- dplyr::distinct(sf::st_drop_geometry(iso), .keep_all = FALSE)
  sc <- twosfca_scores(
    pairs,
    demand_tbl = d_iso,
    supply_tbl = supply,
    demand_key_col = "GEOID",
    supply_key_col = "query",
    pop_col = "population",
    supply_col = NULL,
    default_supply = 1
  )
  out_access <- access(
    pnts, iso, supply,
    name_col = "access_test",
    demand_id_col = "GEOID",
    supply_id_col = "query",
    pop_col = "population",
    crs_for_match = crs,
    default_supply = 1
  )
  out_gpt <- gpt_access(
    pnts, iso, supply,
    name_col = "access_gpt",
    crs_for_match = crs
  )
  r_a <- 1 / 300
  r_b <- 1 / 100
  expect_equal(out_access$access_test[out_access$GEOID == "1"], r_a + r_b)
  expect_equal(out_access$access_test[out_access$GEOID == "2"], r_a)
  expect_equal(out_access$access_test, dplyr::pull(sc[match(out_access$GEOID, sc$GEOID), ], A))
  expect_equal(out_gpt$access_gpt, out_access$access_test)
})

test_that("iso_nearest_data_prep attaches zone centroid columns", {
  z <- sf::st_sf(
    GEOID = c("x", "y"),
    geometry = sf::st_sfc(
      sf::st_point(c(-1, 0)),
      sf::st_point(c(2, 0)),
      crs = 4326
    ),
    crs = 4326
  )
  z <- suppressWarnings(sf::st_buffer(sf::st_transform(z, 3857), 300))
  sw <- tibble::tibble(
    query = "site1",
    within_isochrone = list(1L)
  )
  out <- iso_nearest_data_prep(sw, z, supply_id_col = "query")
  expect_true(all(c("longitude_c", "latitude_c", "GEOID") %in% names(out)))
  expect_equal(as.character(out$GEOID[[1]]), "x")
})

test_that("get_adj queen contiguity joins edge-sharing polygons", {
  crs <- 3857
  b1 <- matrix(c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0), ncol = 2L, byrow = TRUE)
  b2 <- matrix(c(1, 0, 2, 0, 2, 1, 1, 1, 1, 0), ncol = 2L, byrow = TRUE)
  m <- sf::st_sf(
    id = 1L:2L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(b1)),
      sf::st_polygon(list(b2)),
      crs = crs
    )
  )
  adj <- get_adj(m, style = "queen", order = 1L)
  expect_identical(dim(adj$W), c(2L, 2L))
  expect_equal(adj$W[1, 2], 1)
  expect_equal(adj$W[2, 1], 1)
})

test_that("twosfca_scores aborts on bad column names", {
  expect_error(
    twosfca_scores(
      data.frame(supply_id = 1),
      demand_tbl = data.frame(GEOID = 1),
      supply_tbl = data.frame(query = 1)
    ),
    "supply_id.*demand_id|demand_id.*supply_id"
  )
})
