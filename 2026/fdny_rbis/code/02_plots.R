source("code/00_load_dependencies.R")
source("code/01_data-preparation.R")

# Pre-compute per-year counts by CD (pre-2023 only), normalized by total buildings -----------------------------------------------
mifc_by_cd_year <- mifc_dedup[!is.na(citycouncildistrict) & year < 2023,
                               .(mifc_n = .N), by = .(citycouncildistrict, year)]
mifc_by_cd_year <- merge(mifc_by_cd_year, bldgs_by_cd, by = c("citycouncildistrict", "year"), all.x = TRUE)
mifc_by_cd_year[, rate := mifc_n / total_bldgs]

rbis_by_cd_year <- rbis_dedup[!is.na(citycouncildistrict) & year < 2023,
                               .(rbis_n = .N), by = .(citycouncildistrict, year)]
rbis_by_cd_year <- merge(rbis_by_cd_year, bldgs_by_cd, by = c("citycouncildistrict", "year"), all.x = TRUE)
rbis_by_cd_year[, rate := rbis_n / total_bldgs]

fidd_by_cd_year <- fidd_with_block[!is.na(citycouncildistrict) & year < 2023,
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
  scale_x_date(date_labels = "%Y") +
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
  scale_x_date(date_labels = "%Y", expand = expansion(mult = c(0.02, 0.1))) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Structural Fire Rate: Deviation from Pre-RBIS Trend",
       subtitle = "Dashed = projected pre-RBIS trend  |  Solid = actual  |  Top 5 CDs by post-RBIS deviation highlighted",
       x = NULL, y = "Fires per Building") +
  theme_nycc() +
  theme(legend.position = "none")

# Demographics of deviant CDs vs city average -----------------------------------------------
cd_col <- names(demographies_by_cd_2023)[1]

demo_long <- demographies_by_cd_2023 |>
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
