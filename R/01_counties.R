# County boundaries (2020) for the contiguous United States, and county centroids

source(here::here("config.R"))
source(here::here("R", "functions.R"))
suppressMessages({ library(data.table); library(sf); library(stringr) })

sf_use_s2(TRUE)
counties <- cached("counties.rds", {
  f <- file.path(RAW, "shapefiles", "counties", "county_geometry_2020.rds")
  g <- if (file.exists(f)) readRDS(f) else
    st_read(file.path("/vsizip", RAW, "shapefiles", "counties", "tl_2020_us_county.zip", "tl_2020_us_county.shp"), quiet = TRUE)
  if (is.na(st_crs(g))) g <- st_set_crs(g, 4326)
  id <- intersect(c("GEOID", "geoid", "GEOID20", "FIPS", "fips"), names(g))[1]
  g$fips <- if (!is.na(id)) str_pad(gsub("\\D", "", as.character(g[[id]])), 5, "left", "0")
            else paste0(str_pad(g$STATEFP, 2, "left", "0"), str_pad(g$COUNTYFP, 3, "left", "0"))
  outside <- c("02", "15", "60", "66", "69", "72", "74", "78")
  g <- g[!substr(g$fips, 1, 2) %in% outside & nchar(g$fips) == 5, ]
  st_transform(st_make_valid(g), CRS)[, "fips"]
})
xy <- st_coordinates(st_transform(suppressWarnings(st_point_on_surface(counties)), 4326))
centroids <- data.table(fips = counties$fips, lon = round(xy[, 1], 5), lat = round(xy[, 2], 5))
saveRDS(centroids, file.path(WORK, "centroids.rds"))
