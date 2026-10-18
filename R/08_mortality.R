# IHME age-standardized mortality by county, race and ethnicity, and year

source(here::here("config.R"))
source(here::here("R", "functions.R"))
suppressMessages({ library(data.table); library(stringr) })

mortality <- cached("mortality.rds", {
  causes <- c(ALL = "all_cause", CVD = "cvd", RESP = "resp")
  files <- list.files(file.path(RAW, "health", "ihme", "unzipped"), "\\.(csv|CSV)$", full.names = TRUE)
  m <- rbindlist(lapply(files, function(f) {
    tk <- str_match(basename(f), "_MX_(\\d{4})_([A-Za-z]+)_")
    d <- fread(f, select = c("fips", "race_name", "age_group_id", "metric_id", "val", "upper", "lower"))
    d <- d[age_group_id == 27 & metric_id == 3 & !is.na(fips)]   # age-standardized rate
    d[, fips := str_pad(as.character(fips), 5, "left", "0")]
    d[nchar(fips) == 5 & substr(fips, 3, 5) != "000",
      .(fips, year = as.integer(tk[, 2]), cause = causes[[toupper(tk[, 3])]], race = race_name,
        mx_per100k = round(val * 1e5, 2), mx_lo = round(lower * 1e5, 2), mx_hi = round(upper * 1e5, 2))]
  }))
  m[order(fips, cause, race, year)]
})
