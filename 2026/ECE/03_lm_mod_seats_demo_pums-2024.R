## Steps ---------------------------------------------------------

# 1. Load dependencies and model-ready data.
# 2. Fit total seats per child model.
# 3. Prepare coefficient table and plot.
# 4. Build simple model diagnostics.
# 5. Compare race-only and income-adjusted models.

source("00_load_dependencies.R")
dtb <- fread("data/lm_model_data.csv")

seats_child_p <- ggplot(dtb, aes(x=pre_sch_age, y=total_seats)) + 
  geom_point() + geom_smooth(method = "lm", se=FALSE) + 
  theme_bw()
# linear model seats/child ~ income + race/ethn
lmbs <- lm(
  log(total_seats / pre_sch_age) ~
    pct_hispanic10 +
    pct_black10 +
    pct_asian10 +
    median_inc_10k,
  data = dtb
)
results <- as.data.table(
  tidy(lmbs, conf.int = TRUE)
)

results[
  ,
  `:=`(
    percent_change =
      100 * (exp(estimate) - 1),
    
    percent_change_low =
      100 * (exp(conf.low) - 1),
    
    percent_change_high =
      100 * (exp(conf.high) - 1)
  )
]

results[
  ,
  term_label := fcase(
    term == "(Intercept)",
    "Intercept",
    
    term == "pct_hispanic10",
    "10 percentage-point inc Hispanic/Latino",
    
    term == "pct_black10",
    "10 percentage-point inc Black NH",
    
    term == "pct_asian10",
    "10 percentage-point inc Asian/API NH",
    
    term == "median_inc_10k",
    "$10,000 inc Median hh income",
    
    default = term
  )
]

results_table <- results[
  ,
  .(
    Predictor = term_label,
    `Percent change` = round(percent_change, 1),
    `95% CI Lower` = round(percent_change_low, 1),
    `95% CI Upper` = round(percent_change_high, 1),
    `P-value` = signif(p.value, 3)
  )
]

results_tab <- results_table

# plot coeffs

coef_p <- ggplot(
  results[term != "(Intercept)"],
  aes(
    x = percent_change,
    y = reorder(term_label, percent_change)
  )
) +
  geom_vline(
    xintercept = 0,
    linetype = 2,
    colour = "grey60"
  ) +
  geom_errorbar(
    aes(
      xmin = percent_change_low,
      xmax = percent_change_high
    ),
    height = 0.15,
    orientation = "y"
  ) +
  geom_point(size = 3) +
  labs(
    title = "Race/Ethnicity and Median HH Income vs Number of Seats/3-4 year olds",
    subtitle = "Reference: White/NH",
    x = "Percent change in seats per preschool-age child",
    y = NULL
  ) +
  theme_minimal(base_size = 12)


# # fitted v observed  -----------------------------------------------------------
# fitted v observed  -----------------------------------------------------------

dtb[
  ,
  `:=`(
    obs_log_seat_rate = log(total_seats / pre_sch_age),
    pred_log_seat_rate = predict(lmbs),
    resid_log_seat_rate = residuals(lmbs),
    pred_seat_rate = exp(predict(lmbs)),
    obs_seat_rate = total_seats / pre_sch_age
  )
]

fit_stats <- data.table(
  n = nobs(lmbs),
  r_squared = summary(lmbs)$r.squared,
  adj_r_squared = summary(lmbs)$adj.r.squared,
  rmse_log = sqrt(mean(dtb$resid_log_seat_rate^2, na.rm = TRUE)),
  mae_log = mean(abs(dtb$resid_log_seat_rate), na.rm = TRUE)
)

pred_obs_p <- ggplot(
  dtb,
  aes(
    x = obs_seat_rate,
    y = pred_seat_rate
  )
) +
  geom_point(alpha = 0.7) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2,
    colour = "grey50"
  ) +
  labs(
    title = "Predicted vs observed seats per preschool-age child",
    x = "Observed seats per preschool-age child",
    y = "Predicted seats per preschool-age child"
  ) +
  theme_minimal(base_size = 12)

pred_obs_log_p <- ggplot(
  dtb,
  aes(
    x = obs_log_seat_rate,
    y = pred_log_seat_rate
  )
) +
  geom_point(alpha = 0.7) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2,
    colour = "grey50"
  ) +
  labs(
    title = "Predicted vs observed log seat rate",
    x = "Observed log(seats per preschool-age child)",
    y = "Predicted log(seats per preschool-age child)"
  ) +
  theme_minimal(base_size = 12)

resid_fitted_p <- ggplot(
  dtb,
  aes(
    x = pred_log_seat_rate,
    y = resid_log_seat_rate
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = 2,
    colour = "grey50"
  ) +
  geom_point(alpha = 0.7) +
  # geom_smooth(
  #   method = "loess",
  #   se = FALSE
  # ) +
  labs(
    title = "Residuals vs fitted values",
    x = "Fitted log(seats per preschool-age child)",
    y = "Residual"
  ) +
  theme_minimal(base_size = 12)

# robustness - drop income ------------------------------------------------
lm_re <- lm(
  log(total_seats / pre_sch_age) ~
    pct_hispanic10 +
    pct_black10 +
    pct_asian10,
  data = dtb
)

resultsre <- as.data.table(
  tidy(lm_re, conf.int = TRUE)
)
resultsre[
  ,
  `:=`(
    percent_change =
      100 * (exp(estimate) - 1),
    
    percent_change_low =
      100 * (exp(conf.low) - 1),
    
    percent_change_high =
      100 * (exp(conf.high) - 1)
  )
]


results[
  ,
  `:=`(
    percent_change =
      100 * (exp(estimate) - 1),
    
    percent_change_low =
      100 * (exp(conf.low) - 1),
    
    percent_change_high =
      100 * (exp(conf.high) - 1)
  )
]


# ------------------------------------------------------------
# Clean race-only model
# ------------------------------------------------------------

race_only <- copy(resultsre)

race_only[
  ,
  predictor := fcase(
    term == "pct_hispanic10", "Hispanic/Latino",
    term == "pct_black10", "Black NH",
    term == "pct_asian10", "Asian/API NH",
    default = NA_character_
  )
]

race_only <- race_only[
  !is.na(predictor),
  .(
    predictor,
    `Without income` = fmt_est(
      percent_change,
      percent_change_low,
      percent_change_high
    ),
    `P-value without income` = fmt_p(p.value)
  )
]

# ------------------------------------------------------------
# Clean race + income model
# ------------------------------------------------------------

with_income <- copy(results)

with_income[
  ,
  predictor := fcase(
    term == "pct_hispanic10", "Hispanic/Latino",
    term == "pct_black10", "Black NH",
    term == "pct_asian10", "Asian/API NH",
    term == "median_inc_10k", "Median household income",
    default = NA_character_
  )
]

with_income <- with_income[
  !is.na(predictor),
  .(
    predictor,
    `With income` = fmt_est(
      percent_change,
      percent_change_low,
      percent_change_high
    ),
    `P-value with income` = fmt_p(p.value)
  )
]

# ------------------------------------------------------------
# Combine tables
# ------------------------------------------------------------

compare_table <- merge(
  race_only,
  with_income,
  by = "predictor",
  all = TRUE
)

# order rows
compare_table[
  ,
  predictor := factor(
    predictor,
    levels = c(
      "Hispanic/Latino",
      "Black NH",
      "Asian/API NH",
      "Median household income"
    )
  )
]

setorder(compare_table, predictor)

compare_table[
  ,
  predictor := as.character(predictor)
]

setnames(compare_table, "predictor", "Predictor")

# print table 
