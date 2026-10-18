# PM2.5: annual county mean (ACAG V5.NA.05)

source(here::here("config.R"))
source(here::here("R", "functions.R"))
suppressMessages({ library(data.table); library(sf); library(terra); library(exactextractr); library(stringr) })

counties <- readRDS(file.path(WORK, "counties.rds"))

pm25 <- cached("pm25.rds", {
  f <- list.files(file.path(RAW, "air_quality", "acag_satellite_pm25"), "\\.nc$", full.names = TRUE)
  f <- f[grepl("V5NA05", basename(f))]
  yr <- as.integer(str_match(basename(f), "\\.(\\d{4})001-")[, 2])
  f <- f[!is.na(yr)][order(yr[!is.na(yr)])]
  yr <- sort(yr[!is.na(yr)])
  read_pm <- function(x) {
    r <- rast(x)
    if (nlyr(r) > 1) { i <- grep("PM25|GWR|PM2", names(r), ignore.case = TRUE); r <- r[[if (length(i)) i[1] else 1]] }
    if (crs(r) == "") crs(r) <- "EPSG:4326"
    r
  }
  cty <- st_transform(counties, crs(read_pm(f[1])))
  rbindlist(lapply(seq_along(f), function(i)
    data.table(fips = cty$fips, year = yr[i],
               pm25_annual_ugm3 = round(exact_extract(read_pm(f[i]), cty, "mean", progress = FALSE), 3))))
})
