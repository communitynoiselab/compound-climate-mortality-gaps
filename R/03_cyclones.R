# Tropical cyclones: maximum modelled sustained wind at the county centroid (IBTrACS)

source(here::here("config.R"))
source(here::here("R", "functions.R"))
suppressMessages({ library(data.table) })

centroids <- readRDS(file.path(WORK, "centroids.rds"))

cyclone <- cached("cyclone.rds", {
  ib <- fread(file.path(RAW, "cyclones", "ibtracs", "ibtracs_NA_v04r01.csv"), colClasses = "character",
              select = c("SID", "SEASON", "ISO_TIME", "LAT", "LON", "WMO_WIND", "USA_WIND"))
  ib <- ib[grepl("^[0-9]{4}$", SEASON)]
  suppressWarnings(ib[, `:=`(season = as.integer(SEASON), lat = as.numeric(LAT), lon = as.numeric(LON),
                             usa = as.numeric(USA_WIND), wmo = as.numeric(WMO_WIND))])
  ib[, wind := fifelse(!is.na(usa), usa, wmo)]
  ib[, date := substr(gsub("[-: ]", "", ISO_TIME), 1, 12)]
  ib <- ib[season %in% YEARS & !is.na(lat) & !is.na(lon) & !is.na(wind) & wind > 0]
  near <- ib[lon >= -105 & lon <= -55 & lat >= 20 & lat <= 50, unique(SID)]
  tracks <- ib[SID %in% near, .(storm_id = SID, season, date, latitude = lat, longitude = lon, wind)]
  setorder(tracks, storm_id, date)

  grid <- data.frame(gridid = centroids$fips, glat = centroids$lat, glon = centroids$lon, glandsea = TRUE)
  storms <- split(as.data.frame(tracks), tracks$storm_id)
  future::plan(future::multisession, workers = max(1, parallel::detectCores() - 1))
  winds <- future.apply::future_lapply(storms, function(s) {
    if (nrow(s) < 3) return(NULL)
    w <- tryCatch(stormwindmodel::get_grid_winds(hurr_track = s[, c("date", "latitude", "longitude", "wind")],
                                                 grid_df = grid), error = function(e) NULL)
    if (is.null(w) || is.null(w$vmax_sust)) return(NULL)
    data.table::data.table(fips = w$gridid, year = s$season[1], vmax = w$vmax_sust)
  }, future.seed = TRUE, future.packages = "stormwindmodel")
  future::plan(future::sequential)
  rbindlist(winds)[, .(max_sust_wind_ms = round(max(vmax), 3)), by = .(fips, year)]
})
