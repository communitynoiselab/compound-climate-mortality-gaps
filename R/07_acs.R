# ACS five-year county estimates, interpolated to every year

source(here::here("config.R"))
source(here::here("R", "functions.R"))
suppressMessages({ library(data.table); library(stringr) })

acs <- cached("acs.rds", {
  files <- sort(list.files(file.path(RAW, "sociodemographic", "census_acs", "county"),
                           "^acs5_\\d{4}_county\\.csv$", full.names = TRUE))
  a <- rbindlist(lapply(files, function(f) {
    d <- fread(f)
    setnames(d, toupper(names(d)))
    v <- function(x) if (x %in% names(d)) as.numeric(d[[x]]) else NA_real_
    tot <- v("B01003_001E")
    inc <- v("B19013_001E"); inc[!is.na(inc) & inc < 0] <- NA
    data.table(fips = sprintf("%02d%03d", as.integer(d$STATE), as.integer(d$COUNTY)),
               year = as.integer(str_extract(basename(f), "\\d{4}")),
               total_pop = as.integer(tot),
               median_hh_income = as.integer(inc),
               pct_nonwhite = round(100 * (tot - v("B02001_002E")) / tot, 2),
               pct_black = round(100 * v("B02001_003E") / tot, 2),
               pct_hispanic = round(100 * v("B03003_003E") / tot, 2),
               pct_poverty = round(100 * v("B17001_002E") / tot, 2))
  }), fill = TRUE)
  a[nchar(fips) == 5 & substr(fips, 3, 5) != "000"]
})

acs_sorted <- fix_fips(acs[order(fips, year)])
num <- setdiff(names(acs_sorted), c("fips", "year"))
acs_sorted[, (num) := lapply(.SD, as.numeric), .SDcols = num]
acs_long <- melt(acs_sorted, id.vars = c("fips", "year"), variable.name = "var", value.name = "val")
acs_long <- acs_long[CJ(fips = unique(acs_long$fips), year = YEARS, var = unique(acs_long$var)),
                     on = c("fips", "year", "var")]
setorder(acs_long, fips, var, year)
acs_long[, val := interp(year, val), by = .(fips, var)]
acs_annual <- dcast(acs_long, fips + year ~ var, value.var = "val")
acs_annual[, total_pop := round(total_pop)]
saveRDS(acs_annual, file.path(WORK, "acs_annual.rds"))
