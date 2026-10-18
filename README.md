# Race-specific time trends and Black-White mortality gaps with co-occurring hazards

Code for:

> Mandalapu SV, Walker ED. Race-specific time trends and the apparent association between co-occurring climate and air quality hazards and Black-White mortality gaps in US counties, 2010 to 2019. *Environmental Research* (under review).

The code builds a county-year panel of five hazards for the 3,108 contiguous US counties, links it to age-standardized mortality by race and ethnicity, and reproduces every table and figure in the paper.

## Files

| File | What it does |
|---|---|
| `config.R` | Paths and hazard thresholds |
| `run_all.R` | Runs every script below in order |
| `R/functions.R` | Helpers shared by the data scripts |
| `R/01_counties.R` | County boundaries and centroids |
| `R/02_drought.R` | Drought weeks at D2 or worse |
| `R/03_cyclones.R` | Modelled tropical cyclone winds |
| `R/04_heat.R` | Extreme heat days |
| `R/05_pm25.R` | Annual PM2.5 |
| `R/06_smoke.R` | Heavy smoke days |
| `R/07_acs.R` | ACS covariates, interpolated to every year |
| `R/08_mortality.R` | IHME mortality by county, race and ethnicity, and year |
| `R/09_panel.R` | County-year panel (2000 to 2024) and analysis cells (2010 to 2019) |
| `R/10_analysis.R` | All models; writes Tables 1 to 3 and S1 to S17 to `output/tables` |
| `R/11_figures.R` | Figures 1 to 5, written to `output/figures` |

## Running the code

1. Download the data listed below into `data/raw`, using the folder layout shown.
2. Open `compound-climate-mortality-gaps.Rproj` in RStudio, or start R in this folder.
3. Run `source("run_all.R")`, or run the scripts in `R/` one at a time in numbered order.

The first run takes a few hours, mostly for the heat, smoke, and cyclone steps. Each step saves its result in `data/work`, so later runs take minutes and any single step can be rerun on its own. The analysis takes about 15 minutes.

## Data

None of the data are included. All sources are public; the IHME estimates cannot be redistributed under IHME's terms.

| Source | Version used | Location in `data/raw` |
|---|---|---|
| [IHME US mortality by county, race, and ethnicity, 2000 to 2019](https://ghdx.healthdata.org/) | Release dated 12 June 2023 | `health/ihme/unzipped/IHME_USA_COD_COUNTY_RACE_ETHN_2000_2019_MX_<year>_<ALL, CVD, RESP>_BOTH_Y2023M06D12.CSV` |
| [US Drought Monitor county statistics](https://droughtmonitor.unl.edu/) | Percent area by category, 2000 to 2024 | `climate/usdm_drought/usdm_county_all_2000_2024.csv` |
| [IBTrACS](https://www.ncei.noaa.gov/products/international-best-track-archive) | North Atlantic, version 4 revision 1 | `cyclones/ibtracs/ibtracs_NA_v04r01.csv` |
| [PRISM daily maximum temperature](https://prism.oregonstate.edu/) | 4 km, 2010 to 2024 | `climate/prism_temperature/daily_tmax_zip/prism/<year>/*.zip` |
| [ACAG surface PM2.5](https://sites.wustl.edu/acag/datasets/surface-pm2-5/) | North America, V5.NA.05 | `air_quality/acag_satellite_pm25/V5NA05.HybridPM25.NorthAmerica.<year>001-<year>364.nc` |
| [NOAA Hazard Mapping System smoke polygons](https://www.ospo.noaa.gov/products/land/hms.html) | Daily, 2010 to 2024 | `wildfire/noaa_hms_smoke/hms_polygons/<year>/hms_smoke<YYYYMMDD>.zip` |
| [American Community Survey](https://data.census.gov/) | County five-year estimates, releases ending 2010, 2012, 2014, 2016, 2018, 2020, 2022, 2023 | `sociodemographic/census_acs/county/acs5_<year>_county.csv` |
| [TIGER/Line county boundaries](https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html) | 2020 | `shapefiles/counties/tl_2020_us_county.zip` (or the same boundaries saved as `county_geometry_2020.rds`) |

The ACS files need the columns `STATE`, `COUNTY`, `B01003_001E`, `B02001_002E`, `B02001_003E`, `B03003_003E`, `B17001_002E`, and `B19013_001E`. State outlines on the map and county centroids for the spatial standard errors are downloaded with `tigris` on first use.

## Methods notes

- Hazard thresholds are set in `config.R`: at least 8 weeks at drought category D2 or worse; a modelled maximum sustained wind of at least 17.49 m/s (34 knots) at the county centroid; at least 30 days above the county's 95th percentile of daily maximum temperature, computed from all days in 2010 to 2024; an annual mean PM2.5 of at least 9 µg/m³; and at least 10 days under heavy smoke.
- Group populations, used for the weights and the 1,000-person floor, come from ACS counts of White alone, Black alone, and Hispanic or Latino residents. The IHME rates are for non-Hispanic White and non-Hispanic Black populations, so the White and Black populations are approximations.
- The Drought Monitor reports Connecticut by planning region; the statewide mean is assigned to each of the eight legacy counties.
- Three county codes that changed during the study period are recoded: 46113 to 46102, 51515 to 51019, and 51560 to 51005.

## Software

R 4.6.0 with data.table 1.18.4, fixest 0.14.1, sf 1.1.2, terra 1.9.46, exactextractr 0.10.1, stringr, future, future.apply, here, tigris 2.2.1, ggplot2 4.0.3, ragg, and stormwindmodel 0.1.5.9, which is installed from GitHub:

```r
install.packages(c("data.table", "fixest", "sf", "terra", "exactextractr", "stringr", "future",
                   "future.apply", "here", "tigris", "ggplot2", "ragg", "remotes"))
remotes::install_github("geanders/stormwindmodel")
```

## License

MIT. See `LICENSE`.

