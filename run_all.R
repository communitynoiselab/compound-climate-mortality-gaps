# Run the full pipeline in order

scripts <- c("01_counties.R", "02_drought.R", "03_cyclones.R", "04_heat.R", "05_pm25.R", "06_smoke.R",
             "07_acs.R", "08_mortality.R", "09_panel.R", "10_analysis.R", "11_figures.R")
for (s in scripts) source(here::here("R", s))
