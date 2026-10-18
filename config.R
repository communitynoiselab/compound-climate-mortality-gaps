# Paths and analysis settings

ROOT <- here::here()
RAW  <- file.path(ROOT, "data", "raw")
WORK <- file.path(ROOT, "data", "work")
OUT  <- file.path(ROOT, "output")

YEARS          <- 2000:2024   # years with hazard data
ANALYSIS_YEARS <- 2010:2019   # years with race-specific mortality
CRS            <- 5070        # USA Contiguous Albers Equal Area
POP_FLOOR      <- 1000        # minimum group population in a county-year

# Hazard thresholds
DROUGHT_WEEKS <- 8       # weeks at D2 (severe) or worse
CYCLONE_MS    <- 17.49   # maximum sustained wind, 34 knots
HEAT_PCTILE   <- 0.95    # county percentile of daily maximum temperature
HEAT_DAYS     <- 30      # days above that percentile
PM25_UGM3     <- 9       # annual mean PM2.5
SMOKE_DAYS    <- 10      # days under heavy smoke

for (d in c(WORK, file.path(OUT, "tables"), file.path(OUT, "figures")))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
