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
