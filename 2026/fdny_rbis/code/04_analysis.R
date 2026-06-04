source("code/02_plots.R")

# Pre-RBIS: MIFC vs FIDD correlation (before 2014) -----------------------------------------------
mifc_fidd <- merge(
  mifc_by_cd_year[year < rbis_year, .(citycouncildistrict, year, insp_rate = rate)],
  fidd_by_cd_year[year < rbis_year, .(citycouncildistrict, year, fidd_rate)],
  by = c("citycouncildistrict", "year")
)[!is.na(insp_rate) & !is.na(fidd_rate)]

# Post-RBIS: RBIS vs FIDD correlation (2014 onward) -----------------------------------------------
rbis_fidd <- merge(
  rbis_by_cd_year[year >= rbis_year, .(citycouncildistrict, year, insp_rate = rate)],
  fidd_by_cd_year[year >= rbis_year, .(citycouncildistrict, year, fidd_rate)],
  by = c("citycouncildistrict", "year")
)[!is.na(insp_rate) & !is.na(fidd_rate)]

# Overall correlations -----------------------------------------------
cat("Pre-RBIS  (MIFC vs fires):", round(cor(mifc_fidd$insp_rate, mifc_fidd$fidd_rate), 3), "\n")
cat("Post-RBIS (RBIS vs fires):", round(cor(rbis_fidd$insp_rate, rbis_fidd$fidd_rate), 3), "\n")

# Map-matched correlations: MIFC 2013 vs FIDD 2013, RBIS 2022 vs FIDD 2013 -----------------------------------------------
map_mifc <- merge(
  mifc_by_cd_year[year == 2013, .(citycouncildistrict, insp_rate = rate)],
  fidd_by_cd_year[year == 2013, .(citycouncildistrict, fidd_rate)],
  by = "citycouncildistrict"
)[!is.na(insp_rate) & !is.na(fidd_rate)]

map_rbis <- merge(
  rbis_by_cd_year[year == 2022, .(citycouncildistrict, insp_rate = rate)],
  fidd_by_cd_year[year == 2013, .(citycouncildistrict, fidd_rate)],
  by = "citycouncildistrict"
)[!is.na(insp_rate) & !is.na(fidd_rate)]

cat("MIFC 2013 vs FIDD 2013 (r):", round(cor(map_mifc$insp_rate, map_mifc$fidd_rate), 3), "\n")
cat("RBIS 2022 vs FIDD 2013 (r):", round(cor(map_rbis$insp_rate, map_rbis$fidd_rate), 3), "\n")

# Correlation by year -----------------------------------------------
cor_by_year <- rbind(
  mifc_fidd[, .(r = cor(insp_rate, fidd_rate), n = .N, period = "Pre-RBIS (MIFC)"),  by = year],
  rbis_fidd[, .(r = cor(insp_rate, fidd_rate), n = .N, period = "Post-RBIS (RBIS)"), by = year]
)[order(year)]
print(cor_by_year)

# Correlation by CD -----------------------------------------------
cor_by_cd <- rbind(
  mifc_fidd[, if (.N >= 3) .(r = cor(insp_rate, fidd_rate), n = .N, period = "Pre-RBIS (MIFC)")
              else          .(r = NA_real_,                   n = .N, period = "Pre-RBIS (MIFC)"),
             by = citycouncildistrict],
  rbis_fidd[, if (.N >= 3) .(r = cor(insp_rate, fidd_rate), n = .N, period = "Post-RBIS (RBIS)")
              else          .(r = NA_real_,                   n = .N, period = "Post-RBIS (RBIS)"),
             by = citycouncildistrict]
)
print(cor_by_cd[order(citycouncildistrict)])

# Plot: correlation by year, pre vs post -----------------------------------------------
ggplot(cor_by_year, aes(x = year, y = r, color = period, group = period)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey70") +
  geom_vline(xintercept = rbis_year - 0.5, linetype = "dotted", color = "grey40") +
  annotate("text", x = rbis_year - 0.5, y = Inf, label = "RBIS introduced",
           hjust = -0.05, vjust = 1.5, size = 3, color = "grey40") +
  geom_line(linewidth = 0.7) +
  geom_point(aes(size = n)) +
  scale_x_continuous(breaks = cor_by_year$year) +
  scale_color_manual(values = c("Pre-RBIS (MIFC)" = "grey50", "Post-RBIS (RBIS)" = "steelblue")) +
  scale_size_continuous(range = c(2, 5), guide = "none") +
  labs(title = "Correlation Between Inspection Rate and Structural Fire Rate by Year",
       subtitle = "Pearson r across council districts  |  Point size = number of CDs",
       x = NULL, y = "Pearson r", color = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# Time-lagged correlations -----------------------------------------------
lags <- -3:3

lag_cor <- rbindlist(lapply(lags, function(k) {
  pre <- merge(
    mifc_by_cd_year[year < rbis_year, .(citycouncildistrict, year, insp_rate = rate)],
    fidd_by_cd_year[, .(citycouncildistrict, year = year - k, fidd_rate)],
    by = c("citycouncildistrict", "year")
  )[!is.na(insp_rate) & !is.na(fidd_rate)]

  post <- merge(
    rbis_by_cd_year[year >= rbis_year, .(citycouncildistrict, year, insp_rate = rate)],
    fidd_by_cd_year[, .(citycouncildistrict, year = year - k, fidd_rate)],
    by = c("citycouncildistrict", "year")
  )[!is.na(insp_rate) & !is.na(fidd_rate)]

  rbind(
    if (nrow(pre)  > 0) pre[,  .(lag = k, r = cor(insp_rate, fidd_rate), n = .N, period = "Pre-RBIS (MIFC)")]
    else data.table(lag = k, r = NA_real_, n = 0L, period = "Pre-RBIS (MIFC)"),
    if (nrow(post) > 0) post[, .(lag = k, r = cor(insp_rate, fidd_rate), n = .N, period = "Post-RBIS (RBIS)")]
    else data.table(lag = k, r = NA_real_, n = 0L, period = "Post-RBIS (RBIS)")
  )
}))

print(lag_cor)

ggplot(lag_cor[!is.na(r)], aes(x = lag, y = r, color = period, group = period)) +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  annotate("text", x = 0, y = Inf, label = "lag 0 (same year)",
           hjust = -0.05, vjust = 1.5, size = 3, color = "grey40") +
  geom_line(linewidth = 0.7) +
  geom_point(aes(size = n)) +
  scale_x_continuous(breaks = lags,
                     labels = c("-3\n(fires 3yr\nbefore)", "-2", "-1",
                                "0\n(same yr)", "+1", "+2",
                                "+3\n(fires 3yr\nafter)")) +
  scale_color_manual(values = c("Pre-RBIS (MIFC)" = "grey50", "Post-RBIS (RBIS)" = "steelblue")) +
  scale_size_continuous(range = c(2, 5), guide = "none") +
  labs(title = "Time-Lagged Correlation: Inspection Rate vs Structural Fire Rate",
       subtitle = "Positive lag = fires occur k years after inspections  |  Point size = number of CD-year pairs",
       x = "Lag (years)", y = "Pearson r", color = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

# Plot: scatter pre vs post -----------------------------------------------
combined <- rbind(
  mifc_fidd[, .(citycouncildistrict, year, insp_rate, fidd_rate, period = "Pre-RBIS (MIFC)")],
  rbis_fidd[, .(citycouncildistrict, year, insp_rate, fidd_rate, period = "Post-RBIS (RBIS)")]
)

ggplot(combined, aes(x = insp_rate, y = fidd_rate, color = period)) +
  geom_point(alpha = 0.4, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 0.8) +
  scale_color_manual(values = c("Pre-RBIS (MIFC)" = "grey50", "Post-RBIS (RBIS)" = "steelblue")) +
  labs(title = "Inspection Rate vs Structural Fire Rate by Council District",
       x = "Inspections per Building", y = "Fires per Building", color = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
