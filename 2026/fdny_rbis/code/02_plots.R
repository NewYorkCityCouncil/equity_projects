source("code/00_load_dependencies.R")

# Cached prepared data -----------------------------------------------
# 01_data-preparation.R is expensive (open-data downloads, spatial joins,
# councilcount). Build the analysis-ready objects once and reuse them. Delete
# data/output/prepared.rds to force a rebuild (e.g. after editing 01).
prepared_path <- "data/output/prepared.rds"
prepared_objs <- c("rbis_dedup", "mifc_dedup", "fidd_dedup", "bldgs_by_cd",
                   "demographies_by_cd_2013", "demographies_by_cd_2023", "rbis_start")
if (file.exists(prepared_path)) {
  message("Loading cached prepared data: ", prepared_path)
  list2env(readRDS(prepared_path), envir = environment())
} else {
  source("code/01_data-preparation.R")
  dir.create(dirname(prepared_path), showWarnings = FALSE, recursive = TRUE)
  saveRDS(mget(prepared_objs), prepared_path)
  message("Saved prepared data cache: ", prepared_path)
}

# Functions -----------------------------------------------
# Rank stability of an inspection regime across districts over time. Per year,
# rank CDs by inspection rate (1 = highest). Returns:
#   $ranks  — long table of CD x year ranks (regime tagged)
#   $consec — year-to-year Spearman correlation of rates across CDs (near 1 =
#             the same districts are prioritized year to year; dips = reshuffles)
rank_stability <- function(dt, rate_col, regime_label) {
  d <- dt[!is.na(get(rate_col)) & !is.na(citycouncildistrict),
          .(citycouncildistrict, year, rate = get(rate_col))]
  d[, rank := frank(-rate), by = year]
  d[, regime := regime_label]

  yrs <- sort(unique(d$year))
  consec <- rbindlist(lapply(seq_len(length(yrs) - 1L), function(i) {
    m <- merge(d[year == yrs[i],     .(citycouncildistrict, rate_1 = rate)],
               d[year == yrs[i + 1], .(citycouncildistrict, rate_2 = rate)],
               by = "citycouncildistrict")
    data.table(year = yrs[i + 1], regime = regime_label, n = nrow(m),
               spearman = cor(m$rate_1, m$rate_2, method = "spearman"))
  }))
  list(ranks = d, consec = consec)
}

# Pre-compute per-year counts by CD, normalized by total buildings -----------------------------------------------
# Council district boundaries were redrawn for 2023 (post-2020-census
# redistricting), so a given CD number refers to different geography before and
# after. We restrict to years before the change to keep every district a stable
# unit across the panel (CD fixed effects assume constant geography); 2023+ data
# would need an old<->new boundary crosswalk to include.
boundary_change_year <- 2023

mifc_by_cd_year <- mifc_dedup[!is.na(citycouncildistrict) & year < boundary_change_year,
                               .(mifc_n = .N), by = .(citycouncildistrict, year)]
mifc_by_cd_year <- merge(mifc_by_cd_year, bldgs_by_cd, by = c("citycouncildistrict", "year"), all.x = TRUE)
mifc_by_cd_year[, rate := mifc_n / total_bldgs]

rbis_by_cd_year <- rbis_dedup[!is.na(citycouncildistrict) & year < boundary_change_year,
                               .(rbis_n = .N), by = .(citycouncildistrict, year)]
rbis_by_cd_year <- merge(rbis_by_cd_year, bldgs_by_cd, by = c("citycouncildistrict", "year"), all.x = TRUE)
rbis_by_cd_year[, rate := rbis_n / total_bldgs]

fidd_by_cd_year <- fidd_dedup[!is.na(citycouncildistrict) & year < boundary_change_year,
                                    .(fidd_n = .N), by = .(citycouncildistrict, year)]
fidd_by_cd_year <- merge(fidd_by_cd_year, bldgs_by_cd, by = c("citycouncildistrict", "year"), all.x = TRUE)
fidd_by_cd_year[, fidd_rate := fidd_n / total_bldgs]

insp_years <- sort(unique(c(mifc_by_cd_year$year, rbis_by_cd_year$year)))
fidd_years  <- sort(unique(fidd_by_cd_year$year))

# Fire rate over time by CD -----------------------------------------------
fidd_by_cd_year[, date := as.Date(paste(year, "01", "01", sep = "-"))]

p_fidd <- ggplot(fidd_by_cd_year,
                 aes(x = date, y = fidd_rate, group = citycouncildistrict,
                     text = paste0("CD ", citycouncildistrict,
                                   "<br>Year: ", year,
                                   "<br>Rate: ", round(fidd_rate, 4)))) +
  geom_line(alpha = 0.4, linewidth = 0.4, color = "firebrick") +
  geom_vline(xintercept = as.Date(rbis_start), linetype = "dotted", color = "black") +
  annotate("text", x = as.Date(rbis_start), y = Inf, label = "RBIS introduced",
           hjust = -0.05, vjust = 1.5, size = 3, color = "black") +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  labs(title = "Structural Fire Rate by Council District Over Time",
       x = NULL, y = "Fires per Building") +
  theme_nycc()

p_fidd

# Static: CDs diverging from pre-RBIS trend -----------------------------------------------
rbis_year <- as.integer(format(rbis_start, "%Y"))

# Fit linear trend per CD on pre-RBIS years only
cd_trends <- fidd_by_cd_year[year < rbis_year & !is.na(fidd_rate), {
  if (.N >= 3) {
    fit <- lm(fidd_rate ~ year)
    list(intercept = coef(fit)[1], slope = coef(fit)[2])
  } else {
    list(intercept = NA_real_, slope = NA_real_)
  }
}, by = citycouncildistrict]

# Project trend across all years
all_years <- sort(unique(fidd_by_cd_year$year))
trend_proj <- cd_trends[!is.na(slope)][
  , .(year = all_years,
      fidd_rate_trend = intercept + slope * all_years),
  by = citycouncildistrict
]
trend_proj[, date := as.Date(paste(year, "01", "01", sep = "-"))]

# Post-RBIS residuals: actual minus projected
post_rbis_resid <- merge(
  fidd_by_cd_year[year >= rbis_year],
  trend_proj[, .(citycouncildistrict, year, fidd_rate_trend)],
  by = c("citycouncildistrict", "year"), all.x = TRUE
)
post_rbis_resid[, residual := fidd_rate - fidd_rate_trend]

# Better/worse than pre-RBIS trend
cd_direction <- post_rbis_resid[
  !is.na(residual),
  .(mean_resid = mean(residual)),
  by = citycouncildistrict
]
cd_direction[, direction := ifelse(mean_resid < 0, "better", "worse")]
print(cd_direction[, .N, by = direction])

# CDs with higher/lower fire rate now vs 2005
rate_2005   <- fidd_by_cd_year[year == 2005, .(citycouncildistrict, rate_2005 = fidd_rate)]
rate_latest <- fidd_by_cd_year[year == max(year), .(citycouncildistrict, rate_latest = fidd_rate)]
rate_change <- merge(rate_2005, rate_latest, by = "citycouncildistrict")[
  !is.na(rate_2005) & !is.na(rate_latest)
]
rate_change[, vs_2005 := ifelse(rate_latest > rate_2005, "higher", "lower")]
cat("\nFire rate vs 2005 (latest year =", max(fidd_by_cd_year$year), "):\n")
print(rate_change[, .N, by = vs_2005])

# Top 5 CDs by mean absolute post-RBIS residual
deviant_cds <- post_rbis_resid[
  !is.na(residual),
  .(mean_abs_resid = mean(abs(residual))),
  by = citycouncildistrict
][order(-mean_abs_resid)][1:5, citycouncildistrict]

fidd_deviant      <- fidd_by_cd_year[citycouncildistrict %in% deviant_cds]
trend_deviant     <- trend_proj[citycouncildistrict %in% deviant_cds]
fidd_deviant_end  <- fidd_deviant[, .SD[which.max(date)], by = citycouncildistrict]

ggplot() +
  geom_line(data = fidd_by_cd_year,
            aes(x = date, y = fidd_rate, group = citycouncildistrict),
            color = "grey85", linewidth = 0.3) +
  geom_vline(xintercept = rbis_start, linetype = "dotted", color = "grey40") +
  annotate("text", x = rbis_start, y = Inf, label = "RBIS introduced",
           hjust = -0.05, vjust = 1.5, size = 3, color = "grey40") +
  geom_line(data = trend_deviant,
            aes(x = date, y = fidd_rate_trend, group = citycouncildistrict,
                color = factor(citycouncildistrict)),
            linetype = "dashed", linewidth = 0.6, alpha = 0.7) +
  geom_line(data = fidd_deviant,
            aes(x = date, y = fidd_rate, group = citycouncildistrict,
                color = factor(citycouncildistrict)),
            linewidth = 0.9) +
  geom_text(data = fidd_deviant_end,
            aes(x = date, y = fidd_rate, label = paste0("CD ", citycouncildistrict),
                color = factor(citycouncildistrict)),
            hjust = -0.1, size = 3, fontface = "bold") +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years",
               expand = expansion(mult = c(0.02, 0.1))) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Structural Fire Rate: Deviation from Pre-RBIS Trend",
       subtitle = "Dashed = projected pre-RBIS trend  |  Solid = actual  |  Top 5 CDs by post-RBIS deviation highlighted",
       x = NULL, y = "Fires per Building") +
  theme_nycc() +
  theme(legend.position = "none")

# Demographics of deviant CDs vs city average -----------------------------------------------
# Use the 2013-boundary demographics (demographies_by_cd_2013): the fire and
# inspection data above is pre-2023, so it sits on the old council boundaries.
# Joining the 2023-boundary demographics here would attach each CD's new-footprint
# demographics to its old-footprint fire data — a geography mismatch.
cd_col <- names(demographies_by_cd_2013)[1]

demo_long <- demographies_by_cd_2013 |>
  rename(citycouncildistrict = !!cd_col) |>
  mutate(citycouncildistrict = as.integer(citycouncildistrict)) |>
  pivot_longer(cols = ends_with("_percent"),
               names_to = "demographic",
               values_to = "percent") |>
  mutate(demographic = sub("_percent$", "", demographic))

city_avg <- demo_long |>
  group_by(demographic) |>
  summarise(percent = mean(percent, na.rm = TRUE), .groups = "drop") |>
  mutate(group = "City Average")

deviant_demo <- demo_long |>
  filter(citycouncildistrict %in% deviant_cds) |>
  mutate(group = paste0("CD ", citycouncildistrict)) |>
  select(group, demographic, percent)

demo_order <- demo_long |>
  group_by(demographic) |>
  summarise(mean_pct = mean(percent, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(mean_pct)) |>
  pull(demographic)

demo_stacked <- bind_rows(city_avg, deviant_demo) |>
  mutate(group = factor(group, levels = c(sort(unique(deviant_demo$group), decreasing = TRUE),
                                          "City Average")),
         demographic = factor(demographic, levels = demo_order))

ggplot(demo_stacked, aes(x = percent, y = group, fill = demographic)) +
  geom_col() +
  geom_text(data = demo_stacked[demo_stacked$percent >= 5, ],
            aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", fontface = "bold") +
  scale_fill_nycc() +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  labs(title = "Demographics: CDs Diverging from Pre-RBIS Fire Trend",
       subtitle = "City Average shown for comparison",
       x = NULL, y = NULL, fill = NULL) +
  theme_nycc() +
  theme(legend.position = "bottom")

# High-fire CDs: define by baseline (pre-RBIS) structural fire rate -------------
# Top tercile of pre-RBIS fire rate = highest fire need. Used here for
# demographic comparison and downstream in 04_analysis.R for inspection targeting.
cd_fire_need <- fidd_by_cd_year[year < rbis_year & !is.na(fidd_rate),
                                .(mean_fidd_rate = mean(fidd_rate)),
                                by = citycouncildistrict][order(-mean_fidd_rate)]

need_cut <- quantile(cd_fire_need$mean_fidd_rate, 2/3, na.rm = TRUE)
cd_fire_need[, fire_group := ifelse(mean_fidd_rate >= need_cut, "High-fire", "Lower-fire")]

cat("\nHigh-fire CDs (top tercile, pre-RBIS structural fire rate):\n")
print(cd_fire_need[fire_group == "High-fire"])

# Pre-RBIS MIFC coverage vs fire rate by CD -------------------------------------
# One point per CD, pre-2014 means. Shows how mandatory inspection coverage
# scaled with fire need before RBIS. CDs below the line = under-served relative
# to fire level (gap dose, formalized in 04_analysis.R). Relationship is convex
# in levels, so plotted on log-log axes where the fit is a straight line.
cd_gap <- merge(
  mifc_by_cd_year[year < rbis_year & !is.na(total_bldgs),
                  .(pre_mifc_rate = sum(mifc_n) / sum(total_bldgs)), by = citycouncildistrict],
  fidd_by_cd_year[year < rbis_year & !is.na(total_bldgs),
                  .(pre_fidd_rate = sum(fidd_n) / sum(total_bldgs)), by = citycouncildistrict],
  by = "citycouncildistrict"
)

ggplot(cd_gap, aes(x = pre_fidd_rate, y = pre_mifc_rate)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40", linewidth = 0.7) +
  geom_point(color = "firebrick", size = 2) +
  geom_text(aes(label = citycouncildistrict), vjust = -0.8, size = 2.8) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Mandatory Inspection Coverage vs Fire Rate, Pre-RBIS (by Council District)",
       subtitle = "Pre-2014 means, log-log scale  |  CDs below the line received less mandatory coverage than their fire level predicts",
       x = "Structural Fires per Building (log scale)",
       y = "MIFC Inspections per Building (log scale)") +
  theme_nycc()

# Demographic makeup of high-fire CDs ------------------------------------------
# Reuse demo_long + city_avg built above.
# as.data.table: demo_long is a tibble (from the dplyr pipeline), so merge would
# return a data.frame and the dcast calls below need a data.table.
demo_fire <- merge(as.data.table(demo_long),
                   cd_fire_need[, .(citycouncildistrict, fire_group, mean_fidd_rate)],
                   by = "citycouncildistrict")

demo_by_group <- demo_fire[
  , .(percent = mean(percent, na.rm = TRUE)), by = .(fire_group, demographic)]

cat("\nMean demographic percent by fire group:\n")
print(dcast(demo_by_group, demographic ~ fire_group, value.var = "percent"))

# Stacked bar: high-fire vs lower-fire vs city average -------------------------
demo_grp_order <- demo_by_group[, .(m = mean(percent)), by = demographic][order(-m), demographic]

demo_groups_stacked <- rbind(
  demo_by_group[, .(group = fire_group, demographic, percent)],
  as.data.table(city_avg)[, .(group = "City Average", demographic, percent)]
)[, `:=`(group       = factor(group, levels = c("High-fire", "Lower-fire", "City Average")),
         demographic = factor(demographic, levels = demo_grp_order))]

ggplot(demo_groups_stacked, aes(x = percent, y = group, fill = demographic)) +
  geom_col() +
  geom_text(data = demo_groups_stacked[percent >= 5],
            aes(label = paste0(round(percent, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold") +
  scale_fill_nycc() +
  scale_x_continuous(labels = scales::label_percent(scale = 1)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(title = "Demographics of High-Fire Council Districts",
       subtitle = "High-fire = top tercile of pre-RBIS structural fire rate",
       x = NULL, y = NULL, fill = NULL) +
  theme_nycc() +
  theme(legend.position = "bottom")

# Trend: correlation between each demographic and fire rate across CDs ----------
# One row per CD: mean fire rate + each demographic percent. Pearson r shows
# which demographics track higher structural fire rates.
demo_wide <- dcast(demo_fire, citycouncildistrict + mean_fidd_rate ~ demographic,
                   value.var = "percent")
demo_names <- setdiff(names(demo_wide), c("citycouncildistrict", "mean_fidd_rate"))

demo_fire_cor <- rbindlist(lapply(demo_names, function(d) {
  ok <- !is.na(demo_wide[[d]]) & !is.na(demo_wide$mean_fidd_rate)
  data.table(demographic = d,
             r = cor(demo_wide[[d]][ok], demo_wide$mean_fidd_rate[ok]),
             n = sum(ok))
}))[order(-r)]

cat("\nCorrelation: demographic percent vs mean structural fire rate (by CD):\n")
print(demo_fire_cor)

ggplot(demo_fire_cor, aes(x = r, y = reorder(demographic, r), fill = r > 0)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "grey40") +
  scale_fill_manual(values = c("TRUE" = "firebrick", "FALSE" = "steelblue"), guide = "none") +
  labs(title = "Which Demographics Track Higher Structural Fire Rates?",
       subtitle = "Pearson r across council districts  |  Fire rate = mean pre-RBIS fires per building",
       x = "Correlation with fire rate", y = NULL) +
  theme_nycc()

# Inspection focus over time: has each regime targeted the same districts? ------
# Rank stability — does the cross-district pattern of inspection rates persist
# year to year, or do priorities reshuffle? MIFC and RBIS handled identically.
# Fires included alongside the two inspection regimes: if the high-fire
# districts reshuffle year to year, defining "high-fire" from a pre-2014 mean
# (cd_fire_need) would misrepresent later years and undercut the premise that
# RBIS can target persistent high-fire areas.
mifc_stab <- rank_stability(mifc_by_cd_year, "rate",      "MIFC")
rbis_stab <- rank_stability(rbis_by_cd_year, "rate",      "RBIS")
fidd_stab <- rank_stability(fidd_by_cd_year, "fidd_rate", "Fires (FIDD)")

ranks_all  <- rbind(mifc_stab$ranks,  rbis_stab$ranks,  fidd_stab$ranks)
consec_all <- rbind(mifc_stab$consec, rbis_stab$consec, fidd_stab$consec)

# Year-to-year Spearman: 1 = identical district ordering as the prior year.
ggplot(consec_all, aes(x = year, y = spearman, color = regime, group = regime)) +
  geom_vline(xintercept = rbis_year - 0.5, linetype = "dotted", color = "grey40") +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = scales::breaks_width(2)) +
  scale_y_continuous(limits = c(NA, 1)) +
  labs(title = "Do Inspections Target the Same Districts Each Year?",
       subtitle = "Year-to-year Spearman of district inspection rates  |  1 = identical priorities to prior year",
       x = NULL, y = "Spearman vs. prior year", color = NULL) +
  theme_nycc() +
  theme(legend.position = "top")

# Overall drift: first year vs last year ranking, per regime.
stability_summary <- ranks_all[, {
  yrs <- sort(unique(year))
  m <- merge(.SD[year == min(yrs), .(citycouncildistrict, r1 = rank)],
             .SD[year == max(yrs), .(citycouncildistrict, r2 = rank)],
             by = "citycouncildistrict")
  .(first_year = min(yrs), last_year = max(yrs), n_cd = nrow(m),
    spearman_first_last = cor(m$r1, m$r2, method = "spearman"))
}, by = regime]
cat("\nInspection-focus stability, first year vs last year (rank Spearman):\n")
print(stability_summary)

# Rank trajectories (bump chart), biggest movers highlighted per regime.
n_movers <- 6
movers <- ranks_all[, {
  yrs <- sort(unique(year))
  .(rank_change = rank[year == max(yrs)] - rank[year == min(yrs)])
}, by = .(regime, citycouncildistrict)
][, .SD[order(-abs(rank_change))][seq_len(min(n_movers, .N))], by = regime]

ranks_all[, is_mover := paste(regime, citycouncildistrict) %in%
            movers[, paste(regime, citycouncildistrict)]]

ggplot(ranks_all, aes(x = year, y = rank, group = citycouncildistrict)) +
  geom_line(data = ranks_all[is_mover == FALSE], color = "grey85", linewidth = 0.3) +
  geom_line(data = ranks_all[is_mover == TRUE],
            aes(color = factor(citycouncildistrict)), linewidth = 0.8) +
  geom_text(data = ranks_all[is_mover == TRUE, .SD[year == max(year)], by = citycouncildistrict],
            aes(label = paste0("CD ", citycouncildistrict), color = factor(citycouncildistrict)),
            hjust = -0.1, size = 2.8, show.legend = FALSE) +
  scale_y_reverse() +
  scale_x_continuous(breaks = scales::breaks_width(2),
                     expand = expansion(mult = c(0.02, 0.12))) +
  facet_wrap(~regime, scales = "free_x") +
  labs(title = "District Inspection-Rank Trajectories Over Time",
       subtitle = paste0("Rank 1 = most inspected per building  |  ", n_movers,
                         " biggest movers per regime highlighted"),
       x = NULL, y = "Inspection rank (1 = highest)", color = NULL) +
  theme_nycc() +
  theme(legend.position = "none")
