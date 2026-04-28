pkgname <- "twosfcaosrm"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('twosfcaosrm')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("osrm_isochrones_batch")
### * osrm_isochrones_batch

flush(stderr()); flush(stdout())

### Name: osrm_isochrones_batch
### Title: Batch isochrones from a local OSRM server
### Aliases: osrm_isochrones_batch osrm_isodistance_batch

### ** Examples

## Not run: 
##D d <- data.frame(id = 1, lon = -74.0, lat = 40.7)
##D osrm_isochrones_batch(
##D   d, "lon", "lat",
##D   breaks = seq(0, 10, by = 10),
##D   osrm_base_url = "http://127.0.0.1:5002/",
##D   profile = "foot"
##D )
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
