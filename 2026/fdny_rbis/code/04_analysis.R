source("code/02_plots.R")

# Functions -----------------------------------------------
# Flag CDs that depart significantly from a bivariate trend. Studentized
# residual = vertical distance from the fitted line, scaled for leverage.
# Many CDs tested at once, so raw p-values are adjusted for multiple testing:
#   p_fdr = Benjamini-Hochberg (controls the false-discovery rate — expected
#           fraction of false flags among those called; powerful, used for the
#           watchlist of departing districts).
#   p_bonf = Bonferroni (controls family-wise error — chance of any false flag;
#            strict, reported alongside for the firmest individual claims).
# standout uses p_fdr at the given alpha. Returns a copy with the columns added.
flag_standouts <- function(dt, model_formula, alpha = 0.05) {
  fit <- lm(model_formula, data = dt)
  rdf <- fit$df.residual - 1                 # rstudent loses one extra df
  out <- copy(dt)
  out[, predicted  := predict(fit)]
  out[, stud_resid := rstudent(fit)]
  out[, p_resid    := 2 * pt(-abs(stud_resid), df = rdf)]
  out[, p_fdr      := p.adjust(p_resid, method = "BH")]
  out[, p_bonf     := p.adjust(p_resid, method = "bonferroni")]
  out[, standout   := fcase(
    p_fdr >= alpha,  "on trend",
    stud_resid > 0,  "above predicted",
    stud_resid < 0,  "below predicted")]
  out[]
}

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

# =============================================================================
# High-fire-rate CDs: did RBIS target them more than MIFC did? -----------------
# Hypothesis: RBIS fills the gap by directing inspections to high-fire areas
# (e.g. the Bronx) that mandatory inspections (MIFC) under-covered.
# cd_fire_need (high-fire group) defined in 02_plots.R.
# Note: pre-period uses MIFC rate, post-period uses RBIS rate (consistent with
# the substitution framing above); MIFC continues post-2014 but is excluded here.
# =============================================================================

# Attach fire group to inspection rates -----------------------------------------
mifc_grp <- merge(mifc_by_cd_year[year < rbis_year, .(citycouncildistrict, year, rate)],
                  cd_fire_need[, .(citycouncildistrict, fire_group)], by = "citycouncildistrict")
rbis_grp <- merge(rbis_by_cd_year[year >= rbis_year, .(citycouncildistrict, year, rate)],
                  cd_fire_need[, .(citycouncildistrict, fire_group)], by = "citycouncildistrict")

# Mean inspection rate: high vs lower-fire CDs, pre (MIFC) vs post (RBIS) --------
insp_by_group <- rbind(
  mifc_grp[, .(insp_rate = mean(rate, na.rm = TRUE), period = "Pre-RBIS (MIFC)"),  by = fire_group],
  rbis_grp[, .(insp_rate = mean(rate, na.rm = TRUE), period = "Post-RBIS (RBIS)"), by = fire_group]
)
cat("\nMean inspection rate by fire group and period:\n")
print(dcast(insp_by_group, fire_group ~ period, value.var = "insp_rate"))

# Targeting ratio: high-fire insp rate / lower-fire insp rate -------------------
# Ratio > 1 means inspections concentrate in high-fire CDs; rise from pre to post
# supports the hypothesis that RBIS targets need better than MIFC did.
targeting <- dcast(insp_by_group, period ~ fire_group, value.var = "insp_rate")
targeting[, targeting_ratio := `High-fire` / `Lower-fire`]
cat("\nTargeting ratio (High-fire / Lower-fire inspection rate):\n")
print(targeting[order(period)])

# Inspection rate over time by fire group, pre vs post --------------------------
insp_ts <- rbind(
  mifc_grp[, .(insp_rate = mean(rate, na.rm = TRUE), n = .N, period = "Pre-RBIS (MIFC)"),  by = .(year, fire_group)],
  rbis_grp[, .(insp_rate = mean(rate, na.rm = TRUE), n = .N, period = "Post-RBIS (RBIS)"), by = .(year, fire_group)]
)[order(year)]

ggplot(insp_ts, aes(x = year, y = insp_rate, color = fire_group, group = fire_group)) +
  geom_vline(xintercept = rbis_year - 0.5, linetype = "dotted", color = "grey40") +
  annotate("text", x = rbis_year - 0.5, y = Inf, label = "RBIS introduced",
           hjust = -0.05, vjust = 1.5, size = 3, color = "grey40") +
  geom_line(linewidth = 0.8) +
  geom_point(aes(size = n)) +
  scale_x_continuous(breaks = insp_ts$year) +
  scale_color_manual(values = c("High-fire" = "firebrick", "Lower-fire" = "grey50")) +
  scale_size_continuous(range = c(2, 5), guide = "none") +
  labs(title = "Inspection Rate in High-Fire vs Lower-Fire Council Districts",
       subtitle = "High-fire = top tercile of pre-RBIS structural fire rate  |  Point size = number of CDs",
       x = NULL, y = "Inspections per Building", color = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

# =============================================================================
# Causal models: does inspection activity reduce fires? ------------------------
# Mean-rate correlations above suffer from simultaneity (inspections chase fires)
# and serial correlation within CD. Two designs address this:
#   A. Event study (dynamic DiD): did fire rates in high-fire CDs bend after
#      RBIS introduction (2014), with flat pre-trends as validation?
#   B. TWFE distributed-lag panel: do lagged inspection rates predict lower
#      fire rates? Lags break simultaneity; CD-clustered SEs handle
#      autocorrelation.
# =============================================================================

# Build CD x year panel: fire rate + total inspection rate (MIFC + RBIS) --------
panel <- merge(
  fidd_by_cd_year[, .(citycouncildistrict, year, fidd_n, fidd_rate, total_bldgs, med_yearbuilt)],
  mifc_by_cd_year[, .(citycouncildistrict, year, mifc_n)],
  by = c("citycouncildistrict", "year"), all.x = TRUE
)
panel <- merge(
  panel,
  rbis_by_cd_year[, .(citycouncildistrict, year, rbis_n)],
  by = c("citycouncildistrict", "year"), all.x = TRUE
)
panel[is.na(mifc_n), mifc_n := 0]
panel[is.na(rbis_n), rbis_n := 0]
panel[, insp_rate := (mifc_n + rbis_n) / total_bldgs]
panel <- merge(panel, cd_fire_need[, .(citycouncildistrict, fire_group, mean_fidd_rate)],
               by = "citycouncildistrict")
panel <- panel[!is.na(fidd_rate) & !is.na(insp_rate)]

# Treatment doses (continuous, per CD, z-scored so coefficients = per SD of dose) ----
# A. RBIS-exposure dose: how hard RBIS actually hit the CD post-2014.
#    Endogenous (FDNY chose targets by risk) — pre-trend test below checks whether
#    high-dose CDs were already trending differently before RBIS existed.
cd_rbis_dose <- panel[year >= rbis_year,
                      .(rbis_dose = mean(rbis_n / total_bldgs, na.rm = TRUE)),
                      by = citycouncildistrict]

# C. Gap dose: pre-2014 under-inspection relative to fire need. Regress MIFC rate
#    on fire rate across CDs (pre-period); negative residual = fewer mandatory
#    inspections than fire need predicts. Sign flipped so higher dose = more
#    under-served = more room for RBIS to fill the gap.
#    Log-log spec: the level relationship is convex (high-fire CDs sit above a
#    straight line), so linear residuals would encode curvature, not neglect.
#    Log residual = % deviation from the coverage the CD's fire level predicts.
#    cd_gap (pre-period rates per CD) built and plotted in 02_plots.R.
stopifnot(cd_gap[, all(pre_mifc_rate > 0 & pre_fidd_rate > 0)])
gap_fit <- lm(log(pre_mifc_rate) ~ log(pre_fidd_rate), data = cd_gap)
cd_gap[, gap_dose := -resid(gap_fit)]

# Functional-form-free robustness: rank gap = fire rank minus inspection rank.
# Positive = ranks high in fires but low in mandatory coverage.
cd_gap[, gap_rank := frank(pre_fidd_rate) - frank(pre_mifc_rate)]
cat("\nGap dose (log-resid) vs rank gap, correlation:",
    round(cd_gap[, cor(gap_dose, gap_rank, method = "spearman")], 3), "\n")

panel <- merge(panel, cd_rbis_dose, by = "citycouncildistrict", all.x = TRUE)
panel <- merge(panel, cd_gap[, .(citycouncildistrict, gap_dose)],
               by = "citycouncildistrict", all.x = TRUE)
panel[is.na(rbis_dose), rbis_dose := 0]

# z-score doses at the CD level (one value per CD), not across panel rows —
# otherwise CDs with more years get more weight in the mean/SD and the
# "per SD of dose" unit drifts on an unbalanced panel.
dose_cd <- unique(panel[, .(citycouncildistrict, rbis_dose, gap_dose)])
dose_cd[, rbis_dose_z := as.numeric(scale(rbis_dose))]
dose_cd[, gap_dose_z  := as.numeric(scale(gap_dose))]
panel <- merge(panel, dose_cd[, .(citycouncildistrict, rbis_dose_z, gap_dose_z)],
               by = "citycouncildistrict", all.x = TRUE)

# Do the two doses agree? r > 0 means RBIS did flow to under-served CDs —
# itself a targeting result. (Correlation is scale-invariant; raw vs z identical.)
cat("\nCorrelation between RBIS-exposure dose and pre-RBIS gap dose (by CD):",
    round(dose_cd[!is.na(gap_dose), cor(rbis_dose, gap_dose)], 3), "\n")

# Dose diagnostics: unpack the rbis_dose vs gap_dose relationship ----------------
dose_cd <- merge(dose_cd, cd_gap[, .(citycouncildistrict, pre_fidd_rate)],
                 by = "citycouncildistrict", all.x = TRUE)

## (i) Does RBIS flow to high-fire CDs in levels? Positive r here + negative
##    gap correlation above = RBIS tracks fire levels but reproduces MIFC's
##    coverage pattern rather than correcting the relative shortfall.
cat("Correlation between RBIS-exposure dose and pre-RBIS fire rate (by CD):",
    round(dose_cd[!is.na(pre_fidd_rate), cor(rbis_dose, pre_fidd_rate)], 3), "\n")

## (ii) Cleanest gap-filling test: purge fire level from RBIS exposure the same
##      way as the gap dose (log-log residual), then correlate the residuals.
##      Question: GIVEN its fire level, does extra RBIS go where MIFC fell short?
##      CDs with zero RBIS dropped from log fit (flagged below).
n_zero_rbis <- dose_cd[rbis_dose <= 0, .N]
if (n_zero_rbis > 0) cat("CDs with zero RBIS exposure excluded from residual fit:", n_zero_rbis, "\n")

rbis_fit <- lm(log(rbis_dose) ~ log(pre_fidd_rate),
               data = dose_cd[rbis_dose > 0 & !is.na(pre_fidd_rate)])
dose_cd[rbis_dose > 0 & !is.na(pre_fidd_rate), rbis_resid := resid(rbis_fit)]

cat("Correlation between RBIS residual (net of fire level) and gap dose:",
    round(dose_cd[!is.na(rbis_resid) & !is.na(gap_dose), cor(rbis_resid, gap_dose)], 3), "\n")

## (iii) Is RBIS doing its job? CDs that depart from the fire -> inspection line -
##     Core question: do districts with more fire get more RBIS inspection?
##     Fit rbis_dose ~ pre_fidd_rate and flag departures. "below predicted" =
##     high fire but under-inspected by RBIS — districts RBIS is failing.
##     "above predicted" = inspected more than fire level warrants.
fire_standouts <- flag_standouts(dose_cd[!is.na(pre_fidd_rate)],
                                 rbis_dose ~ pre_fidd_rate)

cat("\nIs RBIS doing its job? Correlation(RBIS dose, fire rate) =",
    round(dose_cd[!is.na(pre_fidd_rate), cor(rbis_dose, pre_fidd_rate)], 3),
    "(positive = high-fire CDs inspected more)\n")
cat("\nCDs departing significantly from the fire -> inspection line (p<0.05):\n")
print(fire_standouts[standout != "on trend",
              .(citycouncildistrict,
                pre_fidd_rate = round(pre_fidd_rate, 3),
                rbis_dose     = round(rbis_dose, 3),
                rbis_expected = round(predicted, 3),
                stud_resid    = round(stud_resid, 2),
                p_resid       = round(p_resid, 3),
                p_fdr         = round(p_fdr, 3),
                p_bonf        = round(p_bonf, 3),
                standout)][order(stud_resid)])

## Scatter: RBIS coverage vs fire rate, standouts highlighted ------------------
##     Below the line (blue) = high fire, under-inspected = RBIS not doing its job.
ggplot(fire_standouts, aes(x = pre_fidd_rate, y = rbis_dose)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40", linewidth = 0.7) +
  geom_point(aes(color = standout), size = 2) +
  geom_text(aes(label = citycouncildistrict, color = standout),
            vjust = -0.8, size = 2.8, show.legend = FALSE) +
  scale_color_manual(values = c("on trend" = "grey60",
                                "above predicted" = "firebrick",
                                "below predicted" = "steelblue")) +
  labs(title = "Is RBIS Doing Its Job? Inspection Coverage vs Fire Rate by Council District",
       subtitle = "Below the line = high fire but under-inspected by RBIS. Colored = significant departure.",
       x = "Pre-RBIS structural fires per building",
       y = "RBIS inspections per building (post-2014 mean)",
       color = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

## (iv) Same flagging on the gap relationship (secondary) ----------------------
##     Keeps the earlier RBIS-vs-gap view for reference.
gap_standouts <- flag_standouts(dose_cd[!is.na(gap_dose)], rbis_dose ~ gap_dose)
cat("\nCDs departing from the RBIS-vs-gap line (p<0.05):\n")
print(gap_standouts[standout != "on trend",
              .(citycouncildistrict,
                gap_dose   = round(gap_dose, 2),
                rbis_dose  = round(rbis_dose, 3),
                stud_resid = round(stud_resid, 2),
                p_resid    = round(p_resid, 3),
                p_fdr      = round(p_fdr, 3),
                p_bonf     = round(p_bonf, 3),
                standout)][order(-stud_resid)])

ggplot(gap_standouts, aes(x = gap_dose, y = rbis_dose)) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40", linewidth = 0.7) +
  geom_point(aes(color = standout), size = 2) +
  geom_text(aes(label = citycouncildistrict, color = standout),
            vjust = -0.8, size = 2.8, show.legend = FALSE) +
  scale_color_manual(values = c("on trend" = "grey60",
                                "above predicted" = "firebrick",
                                "below predicted" = "steelblue")) +
  labs(title = "RBIS Exposure vs Pre-RBIS Under-Inspection Gap (by Council District)",
       subtitle = "Positive gap dose = under-served by MIFC relative to fire level. Colored = significant departure.",
       x = "Gap dose (under-inspection, pre-RBIS)",
       y = "RBIS inspections per building (post-2014 mean)",
       color = NULL) +
  theme_minimal() +
  theme(legend.position = "top")

## (v) Group test: does the high-fire bloc sit above the RBIS-vs-gap line? ------
## The per-point test (iv) only flags one district because a coherent group above
## the line pulls the OLS line toward itself, deflating each member's residual
## (masking). A group indicator tests the bloc's collective shift, pooling their
## evidence — far more power. The group (high-fire tercile) is defined on FIRE,
## exogenous to RBIS, so this is not circular.
grp_dt <- merge(dose_cd[!is.na(gap_dose)],
                cd_fire_need[, .(citycouncildistrict, fire_group)], by = "citycouncildistrict")
grp_dt[, fire_group := relevel(factor(fire_group), ref = "Lower-fire")]

gap_group_fit <- lm(rbis_dose ~ gap_dose + fire_group, data = grp_dt)
cat("\nGroup test — does the high-fire bloc sit above the RBIS-vs-gap line?\n")
print(summary(gap_group_fit))

# Robustness: drop the single largest per-point outlier (whichever CD that is),
# to confirm the bloc effect is not driven by one district.
top_outlier <- gap_standouts[which.max(abs(stud_resid)), citycouncildistrict]
gap_group_fit_robust <- lm(rbis_dose ~ gap_dose + fire_group,
                           data = grp_dt[citycouncildistrict != top_outlier])
cat("\nHigh-fire bloc coefficient excluding CD", top_outlier, "(largest outlier):\n")
print(round(summary(gap_group_fit_robust)$coefficients["fire_groupHigh-fire", , drop = FALSE], 4))

# --- Dose validity check: is the static post-2014 RBIS dose representative? ----
# rbis_dose collapses 2014-2022 to one number per CD. That fairly summarizes a
# CD only if its RBIS exposure is stable over those years. If RBIS shifted focus
# or ramped unevenly, the static dose (used by the event study and dose scatters)
# mistimes/mismeasures exposure. The distributed-lag model uses annual rates and
# is not exposed to this.
rbis_annual <- panel[year >= rbis_year,
                     .(citycouncildistrict, year, rbis_rate = rbis_n / total_bldgs)]

# (a) Within-CD variability: coefficient of variation of the annual rate.
#     High CV = the mean papers over big year-to-year swings.
rbis_cv <- rbis_annual[, .(mean_rate = mean(rbis_rate),
                           cv        = sd(rbis_rate) / mean(rbis_rate),
                           n_years   = .N), by = citycouncildistrict]
cat("\nRBIS annual-rate CV across CDs (post-2014) — lower = more stable:\n")
print(rbis_cv[, .(median_cv = round(median(cv, na.rm = TRUE), 2),
                  p90_cv    = round(quantile(cv, 0.9, na.rm = TRUE), 2),
                  max_cv    = round(max(cv, na.rm = TRUE), 2))])
cat("\nMost volatile CDs (static dose least representative here):\n")
print(rbis_cv[order(-cv)][seq_len(min(6, .N)),
              .(citycouncildistrict, mean_rate = round(mean_rate, 3),
                cv = round(cv, 2), n_years)])

# (b) Cross-CD representativeness: does each year's district ordering match the
#     static-mean ordering? Spearman near 1 every year = the mean dose ranks CDs
#     the same as any single year, so it is a fair stand-in.
rep_by_year <- merge(rbis_annual, cd_rbis_dose, by = "citycouncildistrict")[
  , .(spearman_vs_mean = round(cor(rbis_rate, rbis_dose, method = "spearman"), 3),
      n = .N), by = year][order(year)]
cat("\nPer-year RBIS ordering vs static-mean ordering (Spearman):\n")
print(rep_by_year)

ggplot(rep_by_year, aes(x = year, y = spearman_vs_mean)) +
  geom_line(linewidth = 0.8, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  scale_x_continuous(breaks = scales::breaks_width(2)) +
  scale_y_continuous(limits = c(NA, 1)) +
  labs(title = "Is the Static RBIS Dose Representative of Each Year?",
       subtitle = "Spearman of each year's district RBIS rates vs the post-2014 mean  |  1 = mean ranks CDs like that year",
       x = NULL, y = "Spearman vs. static mean") +
  theme_minimal()

# --- A. Event study, RBIS-exposure dose ----------------------------------------
# fire_it = a_i + g_t + sum_k b_k (dose_i x 1[year = k]) + e_it
# Dose = CD's actual post-2014 RBIS intensity (z-scored). CD FE absorb fixed
# district risk; year FE absorb citywide shocks. b_k pre-2014 ~ 0 = high-dose
# CDs were not already trending differently (validates against endogenous
# targeting); b_k < 0 post-2014 = fires fell where RBIS concentrated.
pre_years <- panel[year < rbis_year, sort(unique(year))]
if (length(pre_years) == 0) stop("No pre-RBIS years in panel — check merges upstream.")
ref_year <- max(pre_years)

es_rbis <- feols(
  fidd_rate ~ i(year, rbis_dose_z, ref = ref_year) | citycouncildistrict + year,
  data = panel, cluster = ~citycouncildistrict
)
cat("\nEvent study A (dose = post-2014 RBIS exposure, ref =", ref_year, "):\n")
print(summary(es_rbis))

iplot(es_rbis,
      main = "Event Study: Fire Rate by RBIS-Exposure Dose",
      xlab = "Year", ylab = "Dose x year coefficient (fires per bldg, per SD of dose)")
abline(v = rbis_year - 0.5, lty = 3, col = "grey40")

# Robustness: add CD-specific linear trends. High-fire (= high-dose) districts
# were already on a long secular fire decline, so the baseline pre-trends slope
# (parallel trends fail). citycouncildistrict[year] gives each CD its own time
# trend; the dose x year coefficients then measure deviation BEYOND that trend.
# If the post-2014 effect was just the pre-existing decline, it vanishes here.
es_rbis_trend <- feols(
  fidd_rate ~ i(year, rbis_dose_z, ref = ref_year) | citycouncildistrict[year] + year,
  data = panel, cluster = ~citycouncildistrict
)
cat("\nEvent study A + CD-specific linear trends (ref =", ref_year, "):\n")
print(summary(es_rbis_trend))

iplot(es_rbis_trend,
      main = "Event Study + CD-Specific Trends: Fire Rate by RBIS-Exposure Dose",
      xlab = "Year", ylab = "Dose x year coefficient, net of each CD's own trend")
abline(v = rbis_year - 0.5, lty = 3, col = "grey40")

# --- C. Event study, gap dose ----------------------------------------------------
# Dose = pre-2014 under-inspection relative to fire need (z-scored). Directly
# tests the hypothesis: did fires bend after 2014 in the CDs MIFC under-served?
es_gap <- feols(
  fidd_rate ~ i(year, gap_dose_z, ref = ref_year) | citycouncildistrict + year,
  data = panel, cluster = ~citycouncildistrict
)
cat("\nEvent study C (dose = pre-RBIS under-inspection gap, ref =", ref_year, "):\n")
print(summary(es_gap))

iplot(es_gap,
      main = "Event Study: Fire Rate by Pre-RBIS Under-Inspection Gap",
      xlab = "Year", ylab = "Dose x year coefficient (fires per bldg, per SD of dose)")
abline(v = rbis_year - 0.5, lty = 3, col = "grey40")

# --- B. TWFE distributed-lag: insp rate lags 0-3 -------------------------------
# fire_it = a_i + g_t + sum_L b_L insp_{i,t-L} + e_it
# b_0 contaminated by simultaneity (inspections chase fires); interpret lagged
# b_1..b_3 — inspections predate the fire outcome. Negative lagged b = inspection
# activity reduces future fires. SEs clustered by CD (serial correlation).
panel <- panel[order(citycouncildistrict, year)]

dl_mod <- feols(
  fidd_rate ~ l(insp_rate, 0:3) | citycouncildistrict + year,
  data = panel, panel.id = ~citycouncildistrict + year,
  cluster = ~citycouncildistrict
)
cat("\nTWFE distributed-lag model (insp_rate lags 0-3):\n")
print(summary(dl_mod))

# Cumulative effect of sustained 1-unit inspection-rate increase
dl_coefs <- coef(dl_mod)
cat("\nCumulative lag effect (sum of b_0..b_3):", round(sum(dl_coefs), 4), "\n")
cat("Lagged-only cumulative effect (b_1..b_3):",
    round(sum(dl_coefs[grepl("l\\(insp_rate, [1-3]\\)", names(dl_coefs))]), 4), "\n")

# Coefficient plot for lag structure
coefplot(dl_mod,
         main = "Effect of Inspection Rate on Fire Rate by Lag",
         xlab = "Inspection rate lag (years)")
abline(h = 0, lty = 3, col = "grey40")

# =============================================================================
# Robustness & diagnostics ------------------------------------------------------
# =============================================================================

# --- 1. Poisson versions: counts with log(buildings) offset --------------------
# fidd_n is small-count data; OLS on rates is unstable when denominators vary.
# fepois models the count directly, offset makes it a rate model.
es_pois <- fepois(
  fidd_n ~ i(year, rbis_dose_z, ref = ref_year) | citycouncildistrict + year,
  offset = ~log(total_bldgs),
  data = panel, cluster = ~citycouncildistrict
)
cat("\nPoisson event study (fidd_n, offset log(total_bldgs), RBIS-exposure dose):\n")
print(summary(es_pois))

dl_pois <- fepois(
  fidd_n ~ l(insp_rate, 0:3) | citycouncildistrict + year,
  offset = ~log(total_bldgs),
  data = panel, panel.id = ~citycouncildistrict + year,
  cluster = ~citycouncildistrict
)
cat("\nPoisson distributed-lag model:\n")
print(summary(dl_pois))

# --- 2. Continuous-treatment event study ---------------------------------------
# Replace binary tercile with pre-RBIS mean fire rate (dose). b_k = effect of a
# 1-unit higher baseline fire rate on the fire trajectory in year k, vs ref year.
# Uses full dose variation, no arbitrary cut.
es_cont <- feols(
  fidd_rate ~ i(year, mean_fidd_rate, ref = ref_year) | citycouncildistrict + year,
  data = panel, cluster = ~citycouncildistrict
)
cat("\nContinuous-treatment event study (dose = pre-RBIS mean fire rate):\n")
print(summary(es_cont))

iplot(es_cont,
      main = "Event Study, Continuous Treatment: Baseline Fire Rate x Year",
      xlab = "Year", ylab = "Baseline-fire-rate x year coefficient")
abline(v = rbis_year - 0.5, lty = 3, col = "grey40")

# --- 3. Wooldridge test for serial correlation in panel residuals ---------------
# Confirms the autocorrelation that motivates CD-clustered SEs.
pdat <- pdata.frame(as.data.frame(panel),
                    index = c("citycouncildistrict", "year"))
cat("\nWooldridge test for AR(1) in panel residuals:\n")
print(pwartest(fidd_rate ~ insp_rate, data = pdat))

# --- 4. Placebo test: fake RBIS in pre-period only ------------------------------
# Restrict to true pre-RBIS years, pretend RBIS started midway through.
# A significant "effect" here means the design picks up spurious trends.
placebo_year <- 2010
pre_panel <- panel[year < rbis_year]

if (length(unique(pre_panel[year < placebo_year, year])) >= 2) {
  placebo_ref <- pre_panel[year < placebo_year, max(year)]
  placebo_mod <- feols(
    fidd_rate ~ i(year, rbis_dose_z, ref = placebo_ref) | citycouncildistrict + year,
    data = pre_panel, cluster = ~citycouncildistrict
  )
  cat("\nPlacebo event study (fake RBIS =", placebo_year, ", pre-period only):\n")
  print(summary(placebo_mod))

  iplot(placebo_mod,
        main = "Placebo Event Study: Fake RBIS in Pre-Period",
        xlab = "Year", ylab = "Dose x year coefficient")
  abline(v = placebo_year - 0.5, lty = 3, col = "grey40")
} else {
  cat("\nPlacebo skipped: not enough pre-", placebo_year, " years in panel.\n")
}

# --- 5. Building-stock control ---------------------------------------------------
# med_yearbuilt varies within CD over time (new construction, demolition).
# Aging/renewing stock confounds inspections vs fires; add as control.
dl_ctrl <- feols(
  fidd_rate ~ l(insp_rate, 0:3) + med_yearbuilt | citycouncildistrict + year,
  data = panel, panel.id = ~citycouncildistrict + year,
  cluster = ~citycouncildistrict
)
cat("\nDistributed-lag model + median year-built control:\n")
print(summary(dl_ctrl))

cat("\nLag coefficients, with vs without building-stock control:\n")
print(data.table(
  term      = names(coef(dl_mod)),
  base      = round(coef(dl_mod), 4),
  with_ctrl = round(coef(dl_ctrl)[names(coef(dl_mod))], 4)
))

# --- 6. Dose diagnostic: did combined inspections actually rise? -----------------
# If MIFC fell as RBIS grew, total dose may be flat — a null in the lag models
# would then mean "no dose change," not "inspections don't work."
dose_by_year <- panel[, .(mifc = sum(mifc_n), rbis = sum(rbis_n),
                          total = sum(mifc_n + rbis_n),
                          mean_insp_rate = mean(insp_rate, na.rm = TRUE)), by = year][order(year)]
cat("\nCitywide inspection dose by year:\n")
print(dose_by_year)

dose_long <- melt(dose_by_year, id.vars = "year",
                  measure.vars = c("mifc", "rbis", "total"),
                  variable.name = "source", value.name = "n")

ggplot(dose_long, aes(x = year, y = n, color = source)) +
  geom_vline(xintercept = rbis_year - 0.5, linetype = "dotted", color = "grey40") +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("mifc" = "grey50", "rbis" = "steelblue", "total" = "black"),
                     labels = c("MIFC", "RBIS", "Total")) +
  labs(title = "Citywide Inspection Volume: Did Total Dose Rise After RBIS?",
       subtitle = "If MIFC fell as RBIS grew, total inspection pressure may be flat",
       x = NULL, y = "Inspections", color = NULL) +
  theme_minimal() +
  theme(legend.position = "top")
