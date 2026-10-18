# Extreme heat: days above the county 95th percentile of daily maximum temperature (PRISM)

source(here::here("config.R"))
source(here::here("R", "functions.R"))
suppressMessages({ library(data.table); library(sf); library(terra); library(exactextractr); library(stringr) })

counties <- readRDS(file.path(WORK, "counties.rds"))

heat <- cached("heat.rds", {
  dir <- file.path(RAW, "climate", "prism_temperature", "daily_tmax_zip", "prism")
  tif <- function(z) {
    n <- unzip(z, list = TRUE)$Name
    n <- n[grepl("\\.tif$", n, ignore.case = TRUE)]
    if (length(n) == 1) file.path("/vsizip", z, n) else NA_character_
  }
  with_crs <- function(r) { if (crs(r) == "") crs(r) <- "EPSG:4269"; r }
  years <- sort(list.dirs(dir, recursive = FALSE))
  first <- with_crs(rast(tif(sort(list.files(years[1], "\\.zip$", full.names = TRUE))[1])))
  cty <- st_transform(counties, crs(first))
  daily <- rbindlist(lapply(years, function(y) cached(paste0("heat_", basename(y), ".rds"), {
    z <- sort(list.files(y, "\\.zip$", full.names = TRUE))
    p <- vapply(z, tif, "")
    day <- str_extract(basename(z), "[0-9]{8}")
    ok <- !is.na(p) & !is.na(day)
    m <- as.data.table(exact_extract(with_crs(rast(p[ok])), cty, "mean", progress = FALSE))
    setnames(m, day[ok])
    m[, fips := cty$fips]
    melt(m, id.vars = "fips", variable.name = "date", value.name = "tmax")
  })))
  daily[, year := as.integer(substr(as.character(date), 1, 4))]
  daily[, p95 := round(as.numeric(quantile(tmax, HEAT_PCTILE, na.rm = TRUE)), 3), by = fips]
  daily[, .(heat_days_p95 = sum(tmax > p95, na.rm = TRUE)), by = .(fips, year)]
})
