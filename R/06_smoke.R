# Wildfire smoke: days with a heavy-density plume over the county (NOAA HMS)

source(here::here("config.R"))
source(here::here("R", "functions.R"))
suppressMessages({ library(data.table); library(sf) })

counties <- readRDS(file.path(WORK, "counties.rds"))

smoke <- cached("smoke.rds", {
  sf_use_s2(FALSE)
  dir <- file.path(RAW, "wildfire", "noaa_hms_smoke", "hms_polygons")
  cty <- st_transform(counties, 4326)
  read_day <- function(z) {
    s <- tryCatch(suppressWarnings(st_read(file.path("/vsizip", z, sub("\\.zip$", ".shp", basename(z))), quiet = TRUE)),
                  error = function(e) NULL)
    if (is.null(s)) {
      shp <- grep("\\.shp$", tryCatch(unzip(z, list = TRUE)$Name, error = function(e) character(0)),
                  ignore.case = TRUE, value = TRUE)
      if (length(shp) == 1)
        s <- tryCatch(suppressWarnings(st_read(file.path("/vsizip", z, shp), quiet = TRUE)), error = function(e) NULL)
    }
    s
  }
  # some daily files contain invalid polygons; repair them, dropping any that cannot be fixed
  repair <- function(s) {
    if (is.null(s) || nrow(s) == 0) return(NULL)
    s <- s[as.character(st_geometry_type(s)) %in% c("POLYGON", "MULTIPOLYGON"), ]
    if (nrow(s) == 0) return(NULL)
    v <- tryCatch(suppressWarnings(suppressMessages(st_make_valid(s))), error = function(e) NULL)
    if (is.null(v)) {
      g <- st_geometry(s); good <- vector("list", length(g)); keep <- logical(length(g))
      for (i in seq_along(g)) {
        gi <- tryCatch(suppressWarnings(suppressMessages(st_make_valid(g[i]))), error = function(e) NULL)
        if (!is.null(gi) && !isTRUE(tryCatch(st_is_empty(gi)[1], error = function(e) TRUE))) {
          good[[i]] <- gi[[1]]; keep[i] <- TRUE
        }
      }
      if (!any(keep)) return(NULL)
      v <- s[keep, ]
      st_geometry(v) <- st_sfc(good[keep], crs = st_crs(s))
    }
    v <- v[!tryCatch(st_is_empty(v), error = function(e) rep(TRUE, nrow(v))), ]
    if (nrow(v) == 0) NULL else v
  }
  density <- function(x) {   # 1 light, 2 medium, 3 heavy
    num <- suppressWarnings(as.numeric(as.character(x)))
    if (mean(!is.na(num)) > 0.5) return(fifelse(num >= 27, 3L, fifelse(num >= 16, 2L, 1L)))
    x <- tolower(trimws(as.character(x)))
    fifelse(grepl("heav|dens", x), 3L, fifelse(grepl("med", x), 2L, 1L))
  }
  years <- sort(list.dirs(dir, recursive = FALSE))
  out <- rbindlist(lapply(years, function(y) cached(paste0("smoke_", basename(y), ".rds"), {
    heavy <- integer(nrow(cty))
    for (z in sort(list.files(y, "\\.zip$", full.names = TRUE))) {
      s <- tryCatch(repair(read_day(z)), error = function(e) NULL)
      if (is.null(s)) next
      col <- grep("density", names(s), ignore.case = TRUE, value = TRUE)[1]
      if (is.na(col)) next
      h <- s[which(density(s[[col]]) == 3), ]
      if (nrow(h)) heavy <- heavy + (suppressMessages(lengths(st_intersects(cty, h))) > 0)
    }
    data.table(fips = cty$fips, year = as.integer(basename(y)), smoke_days_heavy = heavy)
  })))
  sf_use_s2(TRUE)
  out
})
