# Models and tables: writes Tables 1 to 3 and S1 to S17 to output/tables

source(here::here("config.R"))
suppressMessages({ library(data.table); library(fixest) })
setFixest_notes(FALSE)

save_table <- function(x, name) fwrite(x, file.path(OUT, "tables", paste0(name, ".csv")))

# Data -------------------------------------------------------------------------

haz <- c("haz_drought", "haz_cyclone", "haz_heat", "haz_pm25", "haz_smoke")
haz_label <- c(haz_drought = "drought", haz_cyclone = "tropical_cyclone", haz_heat = "extreme_heat",
               haz_pm25 = "pm25", haz_smoke = "wildfire_smoke")
outcomes <- c("all_cause", "cvd")

panel <- readRDS(file.path(WORK, "panel.rds"))
for (h in haz) panel[is.na(get(h)), (h) := 0L]
panel[, cc := rowSums(.SD), .SDcols = haz]

division <- c(`09`=1, `23`=1, `25`=1, `33`=1, `44`=1, `50`=1, `34`=2, `36`=2, `42`=2,
              `17`=3, `18`=3, `26`=3, `39`=3, `55`=3, `19`=4, `20`=4, `27`=4, `29`=4, `31`=4, `38`=4, `46`=4,
              `10`=5, `11`=5, `12`=5, `13`=5, `24`=5, `37`=5, `45`=5, `51`=5, `54`=5, `01`=6, `21`=6, `28`=6, `47`=6,
              `05`=7, `22`=7, `40`=7, `48`=7, `04`=8, `08`=8, `16`=8, `30`=8, `32`=8, `35`=8, `49`=8, `56`=8,
              `06`=9, `41`=9, `53`=9)
panel[, division := unname(division[substr(fips, 1, 2)])]

A <- merge(readRDS(file.path(WORK, "cells.rds")), panel[, c("fips", "year", haz), with = FALSE],
           by = c("fips", "year"), all.x = TRUE)
A[, `:=`(division = unname(division[substr(fips, 1, 2)]), state = substr(fips, 1, 2),
         race = relevel(factor(race), ref = "White"))]
A[, `:=`(isB = as.numeric(race == "Black"), isL = as.numeric(race == "Latino"),
         z_pov = as.numeric(scale(pct_poverty)), z_inc = as.numeric(scale(median_hh_income)),
         comp = as.numeric(compound_count), t = year - 2010L)]
A[, `:=`(t2 = t^2, ui_se = (mx_hi - mx_lo) / (2 * 1.96), fr = paste0(fips, "_", race))]
A[, ivw := fifelse(is.finite(ui_se) & ui_se > 0, 1 / ui_se^2, NA_real_)]

base <- function(cause_, floor = POP_FLOOR) {
  s <- copy(A[cause == cause_ & race_pop >= floor])
  s[, `:=`(any = as.numeric(comp >= 1), ge2 = as.numeric(comp >= 2),
           d1 = as.numeric(comp == 1), d2 = as.numeric(comp == 2), d3 = as.numeric(comp >= 3))]
  s
}
lead <- panel[, .(fips, year = year - 1L, comp_lead = as.numeric(cc), smoke_lead = as.numeric(haz_smoke))]
lag1 <- panel[, .(fips, year = year + 1L, cl1 = as.numeric(cc))]
lag2 <- panel[, .(fips, year = year + 2L, cl2 = as.numeric(cc))]

# Model helpers ----------------------------------------------------------------

fe <- list(original     = "fips^race + division^year",
           race_year    = "fips^race + division^year + race^year",
           race_div     = "fips^race + race^division^year",
           trends       = "fr[t] + division^year",
           trends_ryear = "fr[t] + division^year + race^year")
both <- fe[c("original", "race_year")]

rhs <- function(v = "comp") sprintf("%1$s + %1$s:isB + %1$s:isL + z_pov + z_inc + %1$s:z_pov + %1$s:z_inc", v)

fit <- function(d, r, fe_, wt = "population", vc = ~fips) {
  f <- as.formula(paste("mx_per100k ~", r, "|", fe_))
  if (wt == "population") return(feols(f, data = d, weights = ~race_pop, vcov = vc))
  if (wt == "inverse_variance") return(feols(f, data = d[!is.na(ivw)], weights = ~ivw, vcov = vc))
  feols(f, data = d, vcov = vc)
}

# Linear combination of coefficients with a t-based interval
lincom <- function(m, w, label) {
  b <- coef(m)
  names(w) <- vapply(names(w), function(n) {
    if (n %in% names(b)) return(n)
    p <- strsplit(n, ":", fixed = TRUE)[[1]]
    if (length(p) == 2 && paste(p[2], p[1], sep = ":") %in% names(b)) paste(p[2], p[1], sep = ":") else n
  }, "")
  if (!all(names(w) %in% names(b)))
    return(data.table(quantity = label, est = NA_real_, se = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_, n = nobs(m)))
  est <- sum(w * b[names(w)])
  se <- sqrt(drop(t(w) %*% vcov(m)[names(w), names(w), drop = FALSE] %*% w))
  df <- tryCatch(as.numeric(fixest::degrees_freedom(m, type = "t")), error = function(e) Inf)
  if (!is.finite(df) || df <= 0) df <- Inf
  q <- qt(0.975, df)
  data.table(quantity = label, est = round(est, 3), se = round(se, 3), lo = round(est - q * se, 3),
             hi = round(est + q * se, 3), p = signif(2 * pt(-abs(est / se), df), 3), n = nobs(m))
}
w1 <- function(a) setNames(1, a)
w2 <- function(a, b) setNames(c(1, 1), c(a, b))

slopes <- function(m, v = "comp") rbind(
  lincom(m, w1(v), "White slope"),
  lincom(m, w2(v, paste0(v, ":isB")), "Black slope"),
  lincom(m, w1(paste0(v, ":isB")), "Black-White"),
  lincom(m, w1(paste0(v, ":isL")), "Latino-White"))

run_both <- function(s, v = "comp", r = rhs(v))
  rbindlist(lapply(names(both), function(k) cbind(model = k, slopes(fit(s, r, both[[k]]), v))))

# Table 1. Descriptives by group ------------------------------------------------

s <- base("all_cause")
t1 <- s[, .(cells = .N, counties = uniqueN(fips),
            mean_count = round(weighted.mean(comp, race_pop), 3),
            pct_ge1 = round(100 * weighted.mean(comp >= 1, race_pop), 1),
            pct_ge2 = round(100 * weighted.mean(comp >= 2, race_pop), 1),
            pct_ge3 = round(100 * weighted.mean(comp >= 3, race_pop), 1),
            poverty = round(weighted.mean(pct_poverty, race_pop), 1),
            income = round(weighted.mean(median_hh_income, race_pop))), by = race]
for (k in c("all_cause", "cvd", "resp"))
  t1 <- merge(t1, base(k)[, setNames(list(round(weighted.mean(mx_per100k, race_pop), 1)), paste0("rate_", k)), by = race], by = "race")
save_table(t1, "table_1")

# Table 2 and Table S7. Race-specific time controls and placebo -----------------

specs <- list(
  "(1)" = list(r = rhs(), fe = fe$original),
  "(2)" = list(r = paste(rhs(), "+ isB:t + isL:t"), fe = fe$original),
  "(3)" = list(r = paste(rhs(), "+ isB:t + isL:t + isB:t2 + isL:t2"), fe = fe$original),
  "(4)" = list(r = rhs(), fe = fe$race_year),
  "(5)" = list(r = rhs(), fe = fe$race_div),
  "(6)" = list(r = rhs(), fe = fe$trends),
  "(7)" = list(r = rhs(), fe = fe$trends_ryear))
placebo <- c("(1)" = "original", "(4)" = "race_year", "(6)" = "trends", "(7)" = "trends_ryear")
r_lead <- paste(rhs(), "+ comp_lead + comp_lead:isB + comp_lead:isL")

t2 <- list()
for (k in c("all_cause", "cvd", "resp")) {
  s <- base(k)
  for (sp in names(specs)) {
    m <- fit(s, specs[[sp]]$r, specs[[sp]]$fe)
    g <- slopes(m)
    st <- lincom(summary(m, vcov = ~state), w1("comp:isB"), "")
    tw <- lincom(summary(m, vcov = ~fips + year), w1("comp:isB"), "")
    g[quantity == "Black-White", `:=`(state_lo = st$lo, state_hi = st$hi, twoway_lo = tw$lo, twoway_hi = tw$hi)]
    t2[[length(t2) + 1]] <- cbind(outcome = k, model = sp, g)
  }
  sl <- merge(s, lead, by = c("fips", "year"))
  for (sp in names(placebo)) {
    m <- fit(sl, r_lead, fe[[placebo[[sp]]]])
    t2[[length(t2) + 1]] <- cbind(outcome = k, model = paste("placebo", sp), rbind(
      lincom(m, w1("comp:isB"), "Black-White, current year"),
      lincom(m, w1("comp_lead:isB"), "Black-White, next year"),
      lincom(m, w1("comp:isL"), "Latino-White, current year"),
      lincom(m, w1("comp_lead:isL"), "Latino-White, next year"),
      lincom(m, w1("comp_lead"), "White, next year")))
  }
}
save_table(rbindlist(t2, fill = TRUE), "table_2_s7")

# Table 3 and Table S14. Simulation ---------------------------------------------
# Real counties, years, hazard counts, weights and covariates; simulated rates with
# no hazard effect or a true Black-White difference of 2, with and without a
# Black-specific linear trend equal to the observed narrowing of the gap.

set.seed(20260929)
reps <- 200
s <- base("all_cause")
s[, alpha := weighted.mean(mx_per100k, race_pop), by = .(fips, race)]
s[, sig := fifelse(is.finite(ui_se) & ui_se > 0, ui_se, median(ui_se, na.rm = TRUE))]
gap <- dcast(s[race %in% c("White", "Black"), .(rate = weighted.mean(mx_per100k, race_pop)), by = .(race, year)],
             year ~ race, value.var = "rate")
trend <- unname(coef(lm(I(Black - White) ~ year, data = gap))["year"])
dy <- unique(s[, .(division, year)])
dy_key <- match(paste(s$division, s$year), paste(dy$division, dy$year))
scenarios <- list(null_trend = c(0, trend), effect_trend = c(2, trend), null_flat = c(0, 0), effect_flat = c(2, 0))
ycols <- paste0("y", seq_len(reps))
t3 <- list()
for (sc in names(scenarios)) {
  delta <- scenarios[[sc]][1]; tau <- scenarios[[sc]][2]
  Y <- matrix(NA_real_, nrow(s), reps)
  for (r in seq_len(reps))
    Y[, r] <- s$alpha + rnorm(nrow(dy), 0, 5)[dy_key] + tau * s$isB * (s$t - 4.5) +
              delta * s$comp * s$isB + rnorm(nrow(s), 0, s$sig)
  s[, (ycols) := as.data.table(Y)]
  for (k in names(both)) {
    mm <- feols(as.formula(paste0("c(", paste(ycols, collapse = ","), ") ~ ", rhs(), " | ", both[[k]])),
                data = s, weights = ~race_pop, vcov = ~fips)
    ct <- t(vapply(seq_len(reps), function(i) {
      mi <- tryCatch(mm[[i]], error = function(e) mm[lhs = i])
      coeftable(mi)["comp:isB", 1:2]
    }, numeric(2)))
    t3[[length(t3) + 1]] <- data.table(scenario = sc, true_difference = delta, trend = round(tau, 2), model = k,
      mean_estimate = round(mean(ct[, 1]), 3), empirical_sd = round(sd(ct[, 1]), 3), mean_se = round(mean(ct[, 2]), 3),
      coverage = round(mean(abs(ct[, 1] - delta) <= 1.96 * ct[, 2]), 3),
      excludes_zero = round(mean(abs(ct[, 1] / ct[, 2]) > 1.96), 3))
  }
  s[, (ycols) := NULL]
}
save_table(rbindlist(t3), "table_3_s14")

# Table S1. Population floor and precision of the mortality estimates -----------

mort <- readRDS(file.path(WORK, "mortality.rds"))
m <- mort[cause == "all_cause" & year %in% ANALYSIS_YEARS & fips %in% unique(panel$fips)]
kept <- base("all_cause")[, .(retained = .N, counties = uniqueN(fips), person_years = sum(race_pop)), by = race]
s1 <- merge(m[race %in% c("White", "Black", "Latino"), .(possible = .N), by = race], kept[, race := as.character(race)], by = "race")
s1[, `:=`(share_retained = round(retained / possible, 4),
          max_share_person_years_excluded = round((possible - retained) * 999 / (person_years + (possible - retained) * 999), 4))]
save_table(s1, "table_s1_floor")
save_table(m[race != "Total", .(cells = .N, median_rate = round(median(mx_per100k, na.rm = TRUE), 1),
                                median_ui_width = round(median(mx_hi - mx_lo, na.rm = TRUE), 1),
                                relative_ui = round(median((mx_hi - mx_lo) / mx_per100k, na.rm = TRUE), 3),
                                share_ui_over_half_rate = round(mean((mx_hi - mx_lo) > 0.5 * mx_per100k, na.rm = TRUE), 3)),
             by = race], "table_s1_precision")

# Table S2. Hazard prevalence and distribution of the count ---------------------

windows <- list(`2010-2019` = ANALYSIS_YEARS, `2010-2023` = 2010:2023)
save_table(rbindlist(lapply(names(windows), function(w) {
  p <- panel[year %in% windows[[w]]]
  data.table(window = w, hazard = haz_label[haz], share = round(sapply(haz, function(h) mean(p[[h]])), 4))
})), "table_s2_prevalence")
save_table(rbindlist(lapply(names(windows), function(w) {
  p <- panel[year %in% windows[[w]]]
  p[, .(county_years = .N, share = round(.N / nrow(p), 4)), by = .(count = cc)][, window := w][order(count)]
})), "table_s2_distribution")

# Table S3. Pairwise co-occurrence ----------------------------------------------

p <- panel[year %in% ANALYSIS_YEARS]
save_table(rbindlist(lapply(haz, function(a) rbindlist(lapply(setdiff(haz, a), function(b) data.table(
  hazard_a = haz_label[a], hazard_b = haz_label[b], both = round(mean(p[[a]] == 1 & p[[b]] == 1), 4),
  b_given_a = round(mean(p[[b]][p[[a]] == 1]), 4), phi = round(cor(p[[a]], p[[b]]), 3)))))), "table_s3_cooccurrence")

# Table S4. Variation within counties -------------------------------------------

p <- panel[year %in% ANALYSIS_YEARS & fips %in% unique(A$fips)]
v <- c(haz, "cc")
x <- as.matrix(p[, ..v])
dm <- fixest::demean(x, f = p[, .(fips, dy = paste(division, year))])
save_table(rbindlist(lapply(seq_along(v), function(k) {
  cm <- p[, .(m = mean(get(v[k]))), by = fips]
  data.table(hazard = c(haz_label, cc = "compound count")[v[k]], prevalence = round(mean(x[, k]), 4),
             counties_ever = round(mean(cm$m > 0), 4), counties_switch = round(mean(cm$m > 0 & cm$m < 1), 4),
             within_share = round(var(x[, k] - ave(x[, k], p$fips)) / var(x[, k]), 4),
             left_after_fe = round(var(dm[, k]) / var(x[, k]), 4))
})), "table_s4_within")

# Table S5. Without the socioeconomic terms -------------------------------------

save_table(rbindlist(lapply(outcomes, function(k)
  cbind(outcome = k, run_both(base(k), r = "comp + comp:isB + comp:isL")))), "table_s5_no_ses")

# Table S6. Unadjusted gap by number of hazards ----------------------------------

save_table(rbindlist(lapply(outcomes, function(k) {
  s <- base(k)[race %in% c("White", "Black")]
  s[, level := fifelse(comp >= 3, "3+", as.character(comp))]
  d <- dcast(s[, .(rate = weighted.mean(mx_per100k, race_pop)), by = .(race, level)], level ~ race, value.var = "rate")
  cbind(outcome = k, d[, gap := round(Black - White, 1)][order(level)])
})), "table_s6_unadjusted")

# Table S8. Alternative standard errors -----------------------------------------

vcs <- list(county = ~fips, state = ~state, two_way = ~fips + year)
s8 <- rbindlist(lapply(outcomes, function(k) rbindlist(lapply(c("comp", "any"), function(v) rbindlist(lapply(names(both), function(b) {
  m <- fit(base(k), rhs(v), both[[b]])
  rbindlist(lapply(names(vcs), function(vc) cbind(outcome = k, exposure = v, model = b, se = vc,
    lincom(summary(m, vcov = vcs[[vc]]), w1(paste0(v, ":isB")), "Black-White"))))
}))))))
options(tigris_use_cache = TRUE)
cty <- tigris::counties(cb = TRUE, resolution = "20m", year = 2020)
xy <- sf::st_coordinates(sf::st_transform(suppressWarnings(sf::st_point_on_surface(sf::st_transform(cty, 5070))), 4326))
cen <- data.table(fips = as.character(cty$GEOID), lon = xy[, 1], lat = xy[, 2])
conley <- rbindlist(lapply(outcomes, function(k) {
  s <- merge(base(k), cen, by = "fips")
  rbindlist(lapply(names(both), function(b) {
    m <- fit(s, rhs(), both[[b]])
    rbindlist(lapply(c(100, 200, 500), function(km) cbind(outcome = k, exposure = "comp", model = b,
      se = paste0("conley_", km, "km"), lincom(tryCatch(summary(m, vcov = conley(km)),
        error = function(e) summary(m, vcov = vcov_conley(m, lat = "lat", lon = "lon", cutoff = km))), w1("comp:isB"), "Black-White"))))
  }))
}))
save_table(rbind(s8, conley), "table_s8_standard_errors")

# Table S9. Population floors ---------------------------------------------------

save_table(rbindlist(lapply(outcomes, function(k) rbindlist(lapply(c(1000, 10000, 25000, 50000, 100000), function(fl)
  cbind(outcome = k, floor = fl, run_both(base(k, fl))))))), "table_s9_floors")

# Table S10. Leaving out one year -----------------------------------------------

save_table(rbindlist(lapply(outcomes, function(k) rbindlist(lapply(ANALYSIS_YEARS, function(y)
  cbind(outcome = k, year_left_out = y, run_both(base(k)[year != y])[quantity == "Black-White"]))))), "table_s10_leave_year")

# Table S11. Leaving out one hazard ---------------------------------------------

save_table(rbindlist(lapply(outcomes, function(k) {
  s <- base(k)
  rbind(cbind(outcome = k, removed = "none", run_both(s)),
        rbindlist(lapply(haz, function(h) {
          s2 <- copy(s)[, comp_lo := comp - get(h)]
          cbind(outcome = k, removed = haz_label[[h]], run_both(s2, "comp_lo"))
        })))
})), "table_s11_leave_hazard")

# Table S12. Alternative thresholds ---------------------------------------------

alt <- list(drought = list(haz = "haz_drought", var = "drought_weeks_d2plus", cuts = c(4, 8, 12, 16)),
            heat    = list(haz = "haz_heat",    var = "heat_days_p95",        cuts = c(20, 30, 40, 50)),
            pm25    = list(haz = "haz_pm25",    var = "pm25_annual_ugm3",     cuts = c(8, 9, 10, 12)),
            smoke   = list(haz = "haz_smoke",   var = "smoke_days_heavy",     cuts = c(5, 10, 20)))
s12 <- list()
for (h in names(alt)) for (cut in alt[[h]]$cuts) {
  a <- alt[[h]]
  tmp <- panel[, .(fips, year, flag = as.integer(!is.na(get(a$var)) & get(a$var) >= cut), old = get(a$haz), cc)]
  tmp[, comp_t := as.numeric(cc - old + flag)]
  for (k in outcomes) {
    s <- merge(base(k), tmp[, .(fips, year, comp_t)], by = c("fips", "year"))
    s12[[length(s12) + 1]] <- cbind(hazard = h, threshold = cut, prevalence = round(mean(tmp[year %in% ANALYSIS_YEARS]$flag), 4),
                                    outcome = k, run_both(s, "comp_t")[quantity == "Black-White"])
  }
}
save_table(rbindlist(s12), "table_s12_thresholds")

# Table S13. Lagged and cumulative counts ---------------------------------------
# 2012-2019 is the main window: earlier lag years lack heat and smoke data.

save_table(rbindlist(lapply(outcomes, function(k) {
  s0 <- merge(merge(base(k), lag1, by = c("fips", "year")), lag2, by = c("fips", "year"))
  s0[, cum3 := (comp + cl1 + cl2) / 3]
  rbindlist(lapply(c("2012-2019", "2010-2019"), function(w) {
    s <- if (w == "2012-2019") s0[year >= 2012] else s0
    rbindlist(lapply(c("comp", "cl1", "cl2", "cum3"), function(v)
      cbind(outcome = k, window = w, exposure = v, run_both(s, v)[quantity %in% c("White slope", "Black-White")])))
  }))
})), "table_s13_lags")

# Table S15. Categorical and binary contrasts -----------------------------------

r_cat <- "d1 + d2 + d3 + d1:isB + d2:isB + d3:isB + d1:isL + d2:isL + d3:isL + z_pov + z_inc + comp:z_pov + comp:z_inc"
save_table(rbindlist(lapply(outcomes, function(k) rbindlist(lapply(names(both), function(b) {
  m <- fit(base(k), r_cat, both[[b]])
  rows <- unlist(lapply(1:3, function(j) {
    d <- paste0("d", j); lv <- c("1", "2", "3+")[j]
    list(lincom(m, w1(d), paste0("White, ", lv, " vs 0")),
         lincom(m, w2(d, paste0(d, ":isB")), paste0("Black, ", lv, " vs 0")),
         lincom(m, w1(paste0(d, ":isB")), paste0("Black-White, ", lv, " vs 0")))
  }), recursive = FALSE)
  rows <- c(rows, list(lincom(m, setNames(c(1, -1), c("d2:isB", "d1:isB")), "Black-White step, 2 vs 1"),
                       lincom(m, setNames(c(1, -1), c("d3:isB", "d2:isB")), "Black-White step, 3+ vs 2")))
  cbind(outcome = k, model = b, rbindlist(rows))
})))), "table_s15_categorical")
save_table(rbindlist(lapply(outcomes, function(k) rbindlist(lapply(c("any", "ge2"), function(v)
  cbind(outcome = k, contrast = v, run_both(base(k), v)))))), "table_s15_binary")

# Table S16. Weighting -----------------------------------------------------------

s16 <- list()
for (k in outcomes) for (wt in c("population", "unweighted", "inverse_variance")) for (fl in c(1000, 10000, 25000, 50000, 100000)) {
  s <- base(k, fl)
  sl <- merge(s, lead, by = c("fips", "year"))
  s16[[length(s16) + 1]] <- cbind(outcome = k, weights = wt, floor = fl, rbind(
    lincom(fit(s, rhs(), fe$original, wt), w1("comp:isB"), "Black-White, original"),
    lincom(fit(s, rhs(), fe$race_year, wt), w1("comp:isB"), "Black-White, race-by-year"),
    lincom(fit(sl, r_lead, fe$race_year, wt), w1("comp_lead:isB"), "Black-White next year, race-by-year")))
}
save_table(rbindlist(s16), "table_s16_weighting")
s <- base("all_cause")
s[, size := cut(race_pop, c(0, 5e3, 1e4, 2.5e4, 5e4, 1e5, Inf), right = FALSE,
                labels = c("1k-5k", "5k-10k", "10k-25k", "25k-50k", "50k-100k", "100k+"))]
sz <- s[, .(cells = .N, person_years = sum(race_pop), median_ui_width = round(median(mx_hi - mx_lo, na.rm = TRUE), 1)), by = .(race, size)]
sz[, `:=`(share_cells = round(cells / sum(cells), 3), share_person_years = round(person_years / sum(person_years), 3)), by = race]
save_table(sz[order(race, size), .(race, size, cells, share_cells, share_person_years, median_ui_width)], "table_s16_cells")

# Table S17. Individual hazards, joint occurrence, and the smoke placebo ---------

r_haz <- paste(paste(sprintf("%1$s + %1$s:isB + %1$s:isL", haz), collapse = " + "), "+ z_pov + z_inc + comp:z_pov + comp:z_inc")
save_table(rbindlist(lapply(outcomes, function(k) rbindlist(lapply(names(both), function(b) {
  m <- fit(base(k), r_haz, both[[b]])
  rbindlist(lapply(haz, function(h) cbind(outcome = k, model = b, hazard = haz_label[[h]], rbind(
    lincom(m, w1(h), "White"), lincom(m, w2(h, paste0(h, ":isB")), "Black"), lincom(m, w1(paste0(h, ":isB")), "Black-White")))))
})))), "table_s17_hazards")
pairs <- list(c("haz_drought", "haz_heat"), c("haz_heat", "haz_smoke"), c("haz_pm25", "haz_smoke"))
save_table(rbindlist(lapply(outcomes, function(k) rbindlist(lapply(pairs, function(pr) rbindlist(lapply(names(both), function(b) {
  s <- base(k)
  s$jnt <- as.numeric(s[[pr[1]]] == 1 & s[[pr[2]]] == 1)
  m <- fit(s, paste(rhs(), "+ jnt + jnt:isB + jnt:isL"), both[[b]])
  cbind(outcome = k, model = b, pair = paste(haz_label[pr], collapse = " x "), joint_cells = sum(s$jnt),
        rbind(lincom(m, w1("jnt"), "White"), lincom(m, w1("jnt:isB"), "Black-White")))
})))))), "table_s17_joint")
save_table(rbindlist(lapply(outcomes, function(k) rbindlist(lapply(c("population", "unweighted"), function(wt) {
  s <- merge(base(k), lead, by = c("fips", "year"))
  m <- fit(s, paste(r_haz, "+ smoke_lead + smoke_lead:isB + smoke_lead:isL"), fe$race_year, wt)
  cbind(outcome = k, weights = wt, rbind(lincom(m, w1("haz_smoke:isB"), "Black-White, current year"),
                                         lincom(m, w1("smoke_lead:isB"), "Black-White, next year")))
})))), "table_s17_smoke_placebo")

# Figure data -------------------------------------------------------------------

s <- base("all_cause")
save_table(s[, .(mean_count = round(weighted.mean(comp, race_pop), 3),
                 p0 = round(weighted.mean(comp == 0, race_pop), 3), p1 = round(weighted.mean(comp == 1, race_pop), 3),
                 p2 = round(weighted.mean(comp == 2, race_pop), 3), p3 = round(weighted.mean(comp >= 3, race_pop), 3)),
             by = race], "figure_2_exposure")
annual <- panel[year %in% ANALYSIS_YEARS, c(list(mean_count = mean(cc)), lapply(.SD, mean)), by = year, .SDcols = haz]
for (k in outcomes) {
  g <- dcast(base(k)[race %in% c("White", "Black"), .(rate = weighted.mean(mx_per100k, race_pop)), by = .(race, year)],
             year ~ race, value.var = "rate")
  setnames(g, c("Black", "White"), paste0(k, c("_black", "_white")))
  g[, (paste0(k, "_gap")) := get(paste0(k, "_black")) - get(paste0(k, "_white"))]
  annual <- merge(annual, g, by = "year")
}
save_table(annual, "figure_3_annual")
