# Figures 1 to 5, as 1000 dpi PNG and TIF.

source(here::here("config.R"))
suppressMessages({ library(data.table); library(ggplot2); library(sf) })

# Plot style
LINE    <- c("#00468B", "#ED0000", "#0099B4", "#6F7878")
FILL    <- c("#8CACCB", "#F78C8C", "#8CD1DD", "#BEC2C2")
SHP     <- c(21, 24, 22)
NOISE   <- c("#F8EBEE", "#E6B0BD", "#D3758C", "#C03B5B", "#AD002A")
FRAME   <- "#595959"
REFLINE <- "#737373"
WHITE   <- "#FFFFFF"
MM  <- function(x) x / 25.4
W15 <- MM(140)
W2  <- MM(190)
theme_set(theme_bw(base_size = 8, base_family = "Helvetica") + theme(
  panel.border = element_rect(fill = NA, colour = FRAME, linewidth = 0.45),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(linewidth = 0.22, colour = "#E5E5E5"),
  axis.ticks = element_line(linewidth = 0.35, colour = FRAME),
  axis.ticks.length = grid::unit(1.6, "pt"),
  axis.text = element_text(colour = "#262626", size = 7),
  axis.title = element_text(colour = "#0D0D0D", size = 8),
  legend.position = "top", legend.justification = "left",
  legend.title = element_text(size = 7, colour = "grey30"), legend.text = element_text(size = 7),
  legend.key.size = grid::unit(3.4, "mm"), legend.key = element_blank(), legend.background = element_blank(),
  legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(0, 0, -5, 0),
  strip.text = element_text(size = 7.5, face = "bold", colour = "grey10"),
  strip.background = element_rect(fill = "#F2F2F2", colour = FRAME, linewidth = 0.45),
  plot.margin = margin(5, 7, 4, 4)))
gridx <- theme(panel.grid.major.y = element_blank())
gridy <- theme(panel.grid.major.x = element_blank())

read_table <- function(name) fread(file.path(OUT, "tables", paste0(name, ".csv")))
outcome_label <- c(all_cause = "All-cause", cvd = "Cardiovascular")

save_figure <- function(p, name, w, h, dpi = 1000) {
  for (ext in c("png", "tif")) {
    dev <- if (ext == "png") ragg::agg_png else ragg::agg_tiff
    args <- list(file.path(OUT, "figures", paste0(name, ".", ext)), p, width = w, height = h,
                 units = "in", dpi = dpi, device = dev)
    if (ext == "tif") args$compression <- "lzw"
    do.call(ggsave, args)
  }
}

# Figure 1. Mean annual hazard count by county ---------------------------------

panel <- readRDS(file.path(WORK, "panel.rds"))
haz <- c("haz_drought", "haz_cyclone", "haz_heat", "haz_pm25", "haz_smoke")
for (h in haz) panel[is.na(get(h)), (h) := 0L]
counts <- panel[year %in% ANALYSIS_YEARS, .(mean_count = mean(rowSums(.SD))), by = fips, .SDcols = haz]

cty <- readRDS(file.path(WORK, "counties.rds"))
cty <- st_transform(merge(cty[cty$fips %in% counts$fips, ], as.data.frame(counts), by = "fips", all.x = TRUE), CRS)
options(tigris_use_cache = TRUE)
states <- tigris::states(cb = TRUE, resolution = "20m", year = 2020)
states <- st_transform(states[states$STATEFP %in% unique(substr(cty$fips, 1, 2)), ], CRS)

p1 <- ggplot() +
  geom_sf(data = cty, aes(fill = mean_count), colour = NA) +
  geom_sf(data = states, fill = NA, colour = WHITE, linewidth = 0.3) +
  scale_fill_gradientn(colours = NOISE, na.value = "#F0F0F0", limits = c(0, NA),
                       name = "Mean annual number of co-occurring hazards, 2010 to 2019") +
  coord_sf(crs = CRS, datum = NA) +
  theme(panel.border = element_blank(), panel.grid = element_blank(), axis.text = element_blank(),
        axis.ticks = element_blank(), legend.title.position = "top",
        legend.key.width = grid::unit(16, "mm"), legend.key.height = grid::unit(2.2, "mm"))
save_figure(p1, "figure_1", W2, MM(125))

# Figure 2. Exposure by group ----------------------------------------------------

d <- melt(read_table("figure_2_exposure"), id.vars = c("race", "mean_count"), variable.name = "level", value.name = "share")
d[, level := factor(level, c("p0", "p1", "p2", "p3"), c("0", "1", "2", "3+"))]
d[, race := factor(race, c("White", "Black", "Latino"))]
p2 <- ggplot(d, aes(level, share, fill = race, colour = race)) +
  geom_col(position = position_dodge(0.78), width = 0.72, linewidth = 0.35) +
  scale_colour_manual(values = LINE[c(4, 1, 3)]) +
  scale_fill_manual(values = FILL[c(4, 1, 3)]) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Co-occurring hazards in county-year", y = "Share of person-time", fill = NULL, colour = NULL) +
  gridy
save_figure(p2, "figure_2", W15, MM(70))

# Figure 3. National series ------------------------------------------------------

a <- read_table("figure_3_annual")
d <- rbind(a[, .(year, panel = "A  Mean annual hazard count, all counties", value = mean_count)],
           a[, .(year, panel = "B  Black-White all-cause mortality gap (per 100,000)", value = all_cause_gap)],
           a[, .(year, panel = "C  Black-White cardiovascular mortality gap (per 100,000)", value = cvd_gap)])
d[, panel := factor(panel, unique(panel))]
p3 <- ggplot(d, aes(year, value)) +
  geom_line(colour = LINE[1], linewidth = 0.45) +
  geom_point(shape = SHP[1], size = 1.6, stroke = 0.4, colour = LINE[1], fill = FILL[1]) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  scale_x_continuous(breaks = ANALYSIS_YEARS) +
  labs(x = NULL, y = NULL) +
  gridy
save_figure(p3, "figure_3", W15, MM(130))

# Figure 4. Black-White difference across models, with placebo ------------------

model_label <- c("(1)" = "(1) Original model", "(2)" = "(2) + race-specific linear trends",
                 "(3)" = "(3) + race-specific quadratic trends", "(4)" = "(4) + race-by-year fixed effects",
                 "(5)" = "(5) Race-by-division-by-year fixed effects", "(6)" = "(6) + county-by-race linear trends",
                 "(7)" = "(7) (6) + race-by-year fixed effects",
                 "placebo (1)" = "Placebo on model (1)", "placebo (4)" = "Placebo on model (4)",
                 "placebo (7)" = "Placebo on model (7)")
t2 <- read_table("table_2_s7")[outcome %in% names(outcome_label)]
d <- rbind(t2[model %in% names(model_label)[1:7] & quantity == "Black-White",
              .(outcome, model, est, lo, hi, group = "Main models", term = "Same-year hazards")],
           t2[model %in% names(model_label)[8:10] & quantity == "Black-White, next year",
              .(outcome, model, est, lo, hi, group = "Placebo", term = "Next-year hazards (placebo)")])
d[, label := factor(model_label[model], rev(model_label))]
d[, group := factor(group, c("Main models", "Placebo"))]
d[, term := factor(term, c("Same-year hazards", "Next-year hazards (placebo)"))]
d[, outcome := factor(outcome_label[outcome], outcome_label)]
p4 <- ggplot(d, aes(est, label, colour = term, fill = term, shape = term)) +
  geom_vline(xintercept = 0, colour = REFLINE, linewidth = 0.35, linetype = 2) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0, orientation = "y", linewidth = 0.45) +
  geom_point(size = 1.9, stroke = 0.45) +
  scale_colour_manual(values = LINE[1:2]) +
  scale_fill_manual(values = FILL[1:2]) +
  scale_shape_manual(values = SHP[1:2]) +
  facet_grid(group ~ outcome, scales = "free", space = "free_y") +
  labs(x = "Black-White difference in mortality slope per added hazard (per 100,000), 95% CI",
       y = NULL, colour = NULL, fill = NULL, shape = NULL) +
  gridx
save_figure(p4, "figure_4", W2, MM(105))

# Figure 5. Each hazard level compared with none --------------------------------

d <- read_table("table_s15_categorical")[grepl("^Black-White, ", quantity) & !grepl("step", quantity)]
d[, level := factor(sub(" vs 0", "", sub("Black-White, ", "", quantity)), c("1", "2", "3+"))]
d[, model := factor(model, c("original", "race_year"), c("Original model", "With race-by-year fixed effects"))]
d[, outcome := factor(outcome_label[outcome], outcome_label)]
dodge <- position_dodge(0.45)
p5 <- ggplot(d, aes(level, est, colour = model, fill = model, shape = model)) +
  geom_hline(yintercept = 0, colour = REFLINE, linewidth = 0.35, linetype = 2) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.45, position = dodge) +
  geom_point(size = 1.9, stroke = 0.45, position = dodge) +
  scale_colour_manual(values = LINE[c(4, 1)]) +
  scale_fill_manual(values = FILL[c(4, 1)]) +
  scale_shape_manual(values = SHP[c(3, 1)]) +
  facet_wrap(~outcome, scales = "free_y") +
  labs(x = "Hazard count versus zero", y = "Black-White difference (per 100,000), 95% CI",
       colour = NULL, fill = NULL, shape = NULL) +
  gridy
save_figure(p5, "figure_5", W15, MM(75))
