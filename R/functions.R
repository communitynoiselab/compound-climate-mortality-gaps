# Helpers shared by the data scripts

cached <- function(file, expr) {
  f <- file.path(WORK, file)
  if (file.exists(f)) return(readRDS(f))
  x <- expr
  saveRDS(x, f)
  x
}

# County codes that changed during the study period. Where an old and a new code
# both report in the same year, the row that comes first in fips order is kept.
recode <- c("46113" = "46102", "51515" = "51019", "51560" = "51005")
fix_fips <- function(d) {
  d <- copy(d)
  m <- unname(recode[d$fips])
  d[, fips := fifelse(is.na(m), fips, m)]
  setorder(d, fips, year)
  unique(d, by = c("fips", "year"))
}

# Linear interpolation across years, carrying end values outward
interp <- function(year, val) {
  ok <- !is.na(val)
  if (sum(ok) >= 2) approx(year[ok], val[ok], xout = year, rule = 2)$y
  else if (sum(ok) == 1) rep(val[ok][1], length(year)) else rep(NA_real_, length(year))
}
