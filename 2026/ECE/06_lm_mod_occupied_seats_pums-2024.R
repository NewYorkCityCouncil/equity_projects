## Steps ---------------------------------------------------------

# 1. Load dependencies and model-ready data.
# 2. Calculate occupied seats per child.
# 3. Fit occupied seats per child model.
# 4. Prepare coefficient table and plot.
# 5. Compare race-only and income-adjusted models.

source("00_load_dependencies.R")
dtb <- fread("data/lm_model_data.csv")

dtb[
  ,
  `:=`(occupied_seats = total_seats - total_vacancy,
       occupied_seats_per_child = (total_seats - total_vacancy) /pre_sch_age)]
# linear model
lm_occupied <- lm(
  log(occupied_seats_per_child) ~
    pct_hispanic10 +
    pct_black10 +
    pct_asian10 +
    median_inc_10k,
  data = dtb
)

occupied_results <- as.data.table(
  tidy(lm_occupied, conf.int = TRUE)
)

occupied_results[
  ,
  `:=`(
    percent_change = 100 * (exp(estimate) - 1),
    percent_change_low = 100 * (exp(conf.low) - 1),
    percent_change_high = 100 * (exp(conf.high) - 1),
    term_label = fcase(
      term == "(Intercept)", "Intercept",
      term == "pct_hispanic10", "10 percentage-point inc Hispanic/Latino",
      term == "pct_black10", "10 percentage-point inc Black NH",
      term == "pct_asian10", "10 percentage-point inc Asian/API NH",
      term == "median_inc_10k", "$10,000 inc Median hh income",
      default = term
    )
  )
]

occupied_tab <- occupied_results[
  ,
  .(
    Predictor = term_label,
    `Percent change` = round(percent_change, 1),
    `95% CI Lower` = round(percent_change_low, 1),
    `95% CI Upper` = round(percent_change_high, 1),
    `P-value` = signif(p.value, 3)
  )
]


# plot coeffs

p_coef <- ggplot(
  occupied_results[term != "(Intercept)"],
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
    title = "Race/Ethnicity and Income vs Occupied occupied seats/3-4 Year Olds",
    x = "Percent change in occupied occupied seats per estimated preschool-age child",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

dtb[
  ,
  `:=`(
    log_occupied_seats_per_child = log(occupied_seats_per_child),
    pred_log_occupied_seats_per_child = predict(lm_occupied),
    resid_occupied_seats_per_child = residuals(lm_occupied),
    pred_occupied_seats_per_child = exp(predict(lm_occupied)),
    obs_occupied_seats_per_child = occupied_seats_per_child
  )
]

fit_stats <- data.table(
  n = nobs(lm_occupied),
  r_squared = summary(lm_occupied)$r.squared,
  adj_r_squared = summary(lm_occupied)$adj.r.squared,
  rmse_log = sqrt(mean(dtb$resid_occupied_seats_per_child^2, na.rm = TRUE)),
  mae_log = mean(abs(dtb$resid_occupied_seats_per_child), na.rm = TRUE)
)

pred_obs_p <- ggplot(
  dtb,
  aes(
    x = obs_occupied_seats_per_child,
    y = pred_occupied_seats_per_child
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
    title = "Predicted vs observed occupied occupied seats per preschool-age child",
    x = "Observed occupied occupied seats per preschool-age child",
    y = "Predicted occupied occupied seats per preschool-age child"
  ) +
  theme_minimal(base_size = 12)


resid_fitted_p <- ggplot(
  dtb,
  aes(
    x = pred_log_occupied_seats_per_child,
    y = resid_occupied_seats_per_child
  )
) +
  geom_hline(
    yintercept = 0,
    linetype = 2,
    colour = "grey50"
  ) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Residuals vs fitted values",
    x = "Fitted log(occupied seats per preschool-age child)",
    y = "Residual"
  ) +
  theme_minimal(base_size = 12)

# robustness - drop income ------------------------------------------------
lm_re <- lm(
  log(occupied_seats_per_child) ~
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


occupied_results[
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

with_income <- copy(occupied_results)

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


# pred_obs_log_p <- ggplot(
#   dtb,
#   aes(
#     x = obs_occ_child,
#     y = pred_occ_child
#   )
# ) +
#   geom_point(alpha = 0.7) +
#   geom_abline(
#     slope = 1,
#     intercept = 0,
#     linetype = 2,
#     colour = "grey50"
#   ) +
#   labs(
#     title = "Predicted vs observed log occupied seat rate",
#     x = "Observed log(occupied seats per preschool-age child)",
#     y = "Predicted log(occupied seats per preschool-age child)"
#   ) +
#   theme_minimal(base_size = 12)

