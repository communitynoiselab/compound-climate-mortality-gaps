# County-year panel with hazard indicators, and the analysis cells (one group in one county-year)

source(here::here("config.R"))
source(here::here("R", "functions.R"))
suppressMessages({ library(data.table) })

counties <- readRDS(file.path(WORK, "counties.rds"))
drought  <- readRDS(file.path(WORK, "drought.rds"))
cyclone  <- readRDS(file.path(WORK, "cyclone.rds"))
pm25     <- readRDS(file.path(WORK, "pm25.rds"))
heat     <- readRDS(file.path(WORK, "heat.rds"))
smoke    <- readRDS(file.path(WORK, "smoke.rds"))
acs_annual <- readRDS(file.path(WORK, "acs_annual.rds"))
mortality  <- readRDS(file.path(WORK, "mortality.rds"))

# County-year panel ------------------------------------------------------------

panel <- CJ(fips = sort(counties$fips), year = YEARS)
for (x in list(drought, cyclone, pm25, heat, smoke)) panel <- merge(panel, fix_fips(x), by = c("fips", "year"), all.x = TRUE)
panel <- merge(panel, acs_annual, by = c("fips", "year"), all.x = TRUE)
panel[, `:=`(haz_drought = as.integer(drought_weeks_d2plus >= DROUGHT_WEEKS),
             haz_cyclone = as.integer(max_sust_wind_ms >= CYCLONE_MS),
             haz_heat    = as.integer(heat_days_p95 >= HEAT_DAYS),
             haz_pm25    = as.integer(pm25_annual_ugm3 >= PM25_UGM3),
             haz_smoke   = as.integer(smoke_days_heavy >= SMOKE_DAYS))]
haz <- c("haz_drought", "haz_cyclone", "haz_heat", "haz_pm25", "haz_smoke")
panel[, n_hazards_assessed := rowSums(!is.na(.SD)), .SDcols = haz]
panel[, compound_count := rowSums(.SD, na.rm = TRUE), .SDcols = haz]
saveRDS(panel, file.path(WORK, "panel.rds"))

# Analysis cells: one group in one county-year ----------------------------------
# Group populations come from ACS counts: White alone, Black alone, and Hispanic or Latino.

cells <- merge(mortality, panel[, .(fips, year, compound_count, total_pop, pct_nonwhite, pct_black,
                                    pct_hispanic, pct_poverty, median_hh_income)], by = c("fips", "year"))
cells[, race_pop := fcase(race == "White",  total_pop * (1 - pct_nonwhite / 100),
                          race == "Black",  total_pop * pct_black / 100,
                          race == "Latino", total_pop * pct_hispanic / 100,
                          default = total_pop)]
cells <- cells[year %in% ANALYSIS_YEARS & race %in% c("White", "Black", "Latino") &
               !is.na(mx_per100k) & is.finite(race_pop) & race_pop >= POP_FLOOR]
setorder(cells, fips, year, cause, race)
saveRDS(cells, file.path(WORK, "cells.rds"))
