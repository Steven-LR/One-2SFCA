# twosfcaosrm

R helpers for **catchment-based access** with **self-hosted OSRM** (often via Docker): isochrones, assigning supply points to demand zones, and tract-level scores.

Routing uses the CRAN package [**osrm**](https://CRAN.R-project.org/package=osrm) against an **OSRM** HTTP service (e.g. `http://127.0.0.1:5002/`).

---

## End-to-end score workflow (typical)

**Data:**

- **Demand:** table of areal units with `GEOID`, `population`, and centroid `longitude` / `latitude` (often joined to polygon attributes in a `map_only`-style object).
- **Supply:** `sf` point layer of facilities (e.g. pharmacies) with a unique id (`name` or `query`) and the same ids in a non-spatial attribute table that includes `population` (tract population repeated per facility row for weighting, as in a join to `pharm_rx`).

**Steps:**

1. **Demand as points** (optional `sf`):

   ```r
   pop_centroids_sf <- sf::st_as_sf(
     pop_centroids,
     coords = c("longitude", "latitude"),
     crs = 4326
   )
   ```

2. **Isochrones** at each demand centroid for a travel-time threshold, using the running OSRM instance for that **profile** (below). Either `purrr::map2` + `osrm::osrmIsochrone` or this package’s `isochrones_func()` / `isochrones_purrr()`.

3. **Attach population (and `GEOID`)** to the isochrone `sf` so each ring row lines up with its tract.

4. **Which supplies fall in which catchment:** project to a projected CRS, then `sf::st_within` from supply points to isochrone polygons (row order of `isochrones_walk_5` must match the `within_isochrone` indices you use later).

   ```r
   crs_proj <- 2263  # or 3857, local State Plane, etc.

   pnts_walk_5 <- pharm_sf %>%
     sf::st_transform(crs = crs_proj) %>%
     dplyr::mutate(
       within_isochrone = sf::st_within(
         .data$geometry,
         isochrones_walk_5 %>% sf::st_transform(crs = crs_proj)
       )
     )
   ```

5. **Access scores** — **`access_step_ratio()`** matches the usual `access(pnts, iso, supply, "access_5")` pattern: per supply, **R = 1 / sum(P)** over tracts in that catchment; per tract, **A = sum(R) × 10,000**, in the column named by `access_name`.

   ```r
   access_walk_5 <- access_step_ratio(
     pnts = pnts_walk_5 %>% dplyr::rename(query = name),
     iso = isochrones_walk_5,
     supply = pharm_rx %>% dplyr::rename(query = name),
     access_name = "access_5",
     supply_id = "query",
     geoid_col = "GEOID"
   )
   ```

   Join `access_walk_5` to polygons or maps on **`geoid`** (not `sf`; use `left_join` to `sf` if needed).

**Alternative (Luo–Wang style 2SFCA with explicit supply weights):** build long `supply_id` / `demand_id` pairs with `match_supplies_to_isochrones()`, then `twosfca_scores()`, or call **`access()`** to run matching + `twosfca_scores()` and attach the result to the isochrone `sf`. That path uses \(R_j = S_j / \sum_{i\in catch(j)} P_i\), not \(1/\sum P\) per supply.

---

## ACS uncertainty when decomposing demand (`lwc_access`)

When you split tract **population** into mode-specific demand with **ACS counts** (a numerator/denominator pair and their published **MOEs**), ignoring sampling error understates uncertainty in accessibility. This package propagates that uncertainty by Monte Carlo: for each simulation draw, **truncated normals** approximate ACS count distributions (mean = published estimate, SD = MOE/1.645 for 90% approximate MOEs), enforce **numerator ≤ denominator**, form the realized share \(p\), then run the same **`R` → `A`** chain as the LWC helpers (`join_iso()` → `acs_gen()` → `R_func()` → `A_func()`). After `n_sims` draws, **`lwc_access()`** returns per-tract summaries of \(A\) and \(p\) (means, medians, 2.5% and 97.5% quantiles).

The workflow is **general**: column names are arguments (`supply_id_col`, `tract_id_col`, `pop_col`, and the four ACS estimate/MOE columns), so you can point the same functions at other ACS tables—not only commute mode splits. **`data`** must be tract-level with the tract id, population, and four ACS columns. **`pnts_c` / `pnts_w`** need your facility id column (often renamed to `query`) plus **`within_isochrone`** from `st_within` against driving and walking isochrones whose rows align with tract ids as in **`join_iso()`**.

**Example** (matches a typical pipeline: supply id as `query`, optional renames for downstream column names):

```r
lwc_access(
  data = data1,
  pnts_c = pnts_car_5 %>% dplyr::rename(query = address1),
  pnts_w = pnts_walk_5 %>% dplyr::rename(query = address1),
  iso_c = car_5,
  iso_w = isochrones_walk_5,
  n_sims = 100,
  supply_id_col = "query",
  tract_id_col = "GEOID"
) %>%
  dplyr::rename(
    geoid = GEOID,
    lwc_A5 = median_A,
    `lwc_A5_0.025` = q0.025,
    `lwc_A5_0.975` = q0.975,
    p_A5 = mean_p
  )
```

Pass **`keep`** if you need a fixed tract universe (e.g. city list) so tracts with no supply in a given draw still appear with \(A = 0\); if omitted, **`keep`** defaults to all tracts in **`data`**. For a single run at published estimates only (no MOE draws), use **`lwc_access_fixed()`**.

---

## OSRM profiles (`foot` vs `car`)

Each **profile** (e.g. `foot`, `car`) needs a **routing graph** built for that profile in OSRM. Common setups:

- One Docker container per profile, each mapped to a **different host port** (e.g. `5002` for walking, `5001` for driving); pass the matching base URL into `osrm.server` / `osrm_base_url`.
- Or a single multi-profile deployment, if your stack supports it.

`osrm.profile` / `profile` must match the service you call (e.g. `"foot"` for a foot-routing container).

---

## Docker / OSRM (minimal)

1. Docker running; obtain an `.osm.pbf` for the study region.
2. Build the OSRM graph for the desired profile(s) (`osrm-extract`, `osrm-partition`, `osrm-customize` as in the [osrm-backend](https://github.com/Project-OSRM/osrm-backend) docs).
3. Run the routing container(s); map ports (host `5002` → container `5000`, etc.).
4. Test:  
   `curl "http://127.0.0.1:5002/route/v1/foot/-74.0,40.7;-73.99,40.72?overview=false"`

---

## Function index (short)

| Topic | Functions |
|-------|-----------|
| Isochrones | `isochrones_func()`, `isochrones_purrr()`, `osrm_isochrones_batch()`, `gpt_isochrones_func()` |
| Assign supply ↔ zone | `match_supplies_to_isochrones()` or manual `st_within` + list column |
| Score (step ratio / thesis style) | `access_step_ratio()`, `gpt_access_bands()` |
| Score (Luo–Wang 2SFCA) | `twosfca_scores()`, `access()` |
| Pairwise time / distance | `duration_url()`, `distance_url()`, `duration_url_route()`, … |
| ACS / multi-mode | `join_iso()`, `acs_gen()`, `R_func()`, `A_func()`, `lwc_access()`, … |
| Other | `get_adj()`, `map_access_data()`, … |

---

## Install

Repository root:

```r
# install.packages("remotes")
Sys.setenv(COPYFILE_DISABLE = "1")  # macOS + some external disks: fewer ._ files in builds
remotes::install_local("twosfcaosrm", dependencies = TRUE)
```

```sh
export COPYFILE_DISABLE=1
R CMD build twosfcaosrm
R CMD INSTALL twosfcaosrm_*.tar.gz
```

---

## License

MIT — see `LICENSE` and `DESCRIPTION`.
