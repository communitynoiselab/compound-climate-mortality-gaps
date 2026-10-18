# Drought: weeks per year with any area at D2 or worse (US Drought Monitor)

source(here::here("config.R"))
source(here::here("R", "functions.R"))
suppressMessages({ library(data.table); library(stringr) })

drought <- cached("drought.rds", {
  d <- fread(file.path(RAW, "climate", "usdm_drought", "usdm_county_all_2000_2024.csv"),
             colClasses = list(character = c("MapDate", "FIPS", "ValidStart", "ValidEnd")))
  if ("None" %in% names(d)) setnames(d, "None", "D_none")
  for (v in c("D_none", "D0", "D1", "D2", "D3", "D4")) set(d, j = v, value = as.numeric(d[[v]]))
  # categories are either exclusive (summing to 100) or cumulative (D2 = D2 or worse)
  exclusive <- abs(median(d$D_none + d$D0 + d$D1 + d$D2 + d$D3 + d$D4, na.rm = TRUE) - 100) <=
               abs(median(d$D_none + d$D0, na.rm = TRUE) - 100)
  d[, d2plus := if (exclusive) D2 + D3 + D4 else D2]
  d[, year := as.integer(format(as.Date(ValidStart, tryFormats = c("%Y%m%d", "%Y-%m-%d", "%m/%d/%Y")), "%Y"))]
  d[, fips := str_pad(gsub("\\D", "", FIPS), 5, "left", "0")]
  d <- d[!is.na(year) & nchar(fips) == 5 & substr(fips, 3, 5) != "000" & year %in% YEARS]
  out <- d[, .(drought_weeks_d2plus = sum(d2plus > 0, na.rm = TRUE)), by = .(fips, year)]

  # Connecticut is reported by planning region; assign the statewide mean to each county
  regions <- sprintf("09%03d", seq(110, 190, 10))
  ct <- out[fips %in% regions, .(drought_weeks_d2plus = round(mean(drought_weeks_d2plus))), by = year]
  ct <- ct[rep(seq_len(.N), each = 8)][, fips := rep(sprintf("09%03d", seq(1, 15, 2)), times = nrow(ct))]
  rbind(out[!fips %in% regions], ct[, .(fips, year, drought_weeks_d2plus)])[order(fips, year)]
})
