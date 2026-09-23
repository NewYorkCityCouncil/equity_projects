####
# Mohamed 

# June 10, 2026

# Objectives:
#     - Run regressions on arrest rates and demographic variables 
####

library(dplyr)
library(tidyverse)
library(estimatr)
library(arrow)
library(fixest)
library(sf)
library(forcats)
library(patchwork)
library(glmmTMB)
library(modelsummary)

arrests_ct_merged <- read_parquet('data/output/arrests_ct_merged_6_30_26.parquet')

nb_plurality <- arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0) %>%
  fenegbin(
    total_arrests ~ plurality_ethnicity + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'true_boroct',
    data = .
  )

nb_plurality_24 <- arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0 & yr == 2024) %>%
  fenegbin(
    total_arrests ~ plurality_ethnicity + log(pop) + log(B19013_001E) + gini,
    cluster = 'true_boroct',
    data = .
  )


arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0) %>%
  fenegbin(
    total_arrests ~ plurality_ethnicity + log(pop) + log(B19013_001E) + gini| yr,
    cluster = 'true_boroct',
    data = .
  )

nb_plurality_offset <- arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0) %>%
  fenegbin(
    total_arrests ~ plurality_ethnicity + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    offset = ~log(pop),
    data = .
  )

nb_prop <- arrests_ct_merged |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0) %>%
  fenegbin(
    total_arrests ~ black_nh_prop + asian_nh_prop + hispanic_prop + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'true_boroct',
    data = .
  )

nb_interaction_white <- 
  arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('white_nh', 'black_nh') & gini > 0) |>
  mutate(plurality_white = ifelse(plurality_ethnicity == 'white_nh', 1, 0)) %>%
  fenegbin(
    total_arrests ~ black_nh_prop*plurality_white + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'true_boroct',
    data = .
  ) 

  arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('white_nh')) |>
  mutate(plurality_white = ifelse(plurality_ethnicity == 'white_nh', 1, 0)) %>%
  fenegbin(
    total_arrests ~ hispanic_prop + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> summary()

 arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('black_nh')) |>
  mutate(plurality_white = ifelse(plurality_ethnicity == 'white_nh', 1, 0)) %>%
  fenegbin(
    total_arrests ~ I(black_nh_prop*100) + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> summary()

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & yr == 2016) |>
  mutate(plurality_white = ifelse(plurality_ethnicity == 'white_nh', 1, 0)) %>%
  fenegbin(
    total_arrests ~ black_nh_prop*plurality_white + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) 

nb_interaction_black <- 
  arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('white_nh', 'black_nh') & gini > 0) |>
  mutate(plurality_black = ifelse(plurality_ethnicity == 'black_nh', 1, 0)) %>%
  fenegbin(
    total_arrests ~ black_nh_prop*plurality_black + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'true_boroct',
    data = .
  ) 

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0) |>
  mutate(plurality_black = ifelse(plurality_ethnicity == 'black_nh', 1, 0) & yr == 2024) %>%
  fenegbin(
    total_arrests ~ black_nh_prop*plurality_black + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E),
    data = .
  ) |> summary()

nb_mods <- list(
  'Plurality Ethn.' = nb_plurality,
  'Ethn. Props.' = nb_prop,
  'Interaction w/ Plurality White' = nb_interaction_white,
  'Interaction w/ Plurality Black' = nb_interaction_black
)

library(modelsummary)

nb_mods |> 
  modelplot() +  
  geom_vline(
    xintercept = 0,
    linetype = 'dashed',
    color = 'darkred'
  ) + 
  labs(
    title = 'Negative Binomial Model Results',
    color = 'Model \nSpecification'
  ) + 
  theme_nycc() + 
  scale_color_nycc() + 
  scale_y_discrete(
    limits = rev,
    labels = c(
      'plurality_ethnicityasian_nh' = 'Predominantly Asian \n(cat.) ',
      'plurality_ethnicityblack_nh' = 'Predominantly Black \n(cat.) ',
      'plurality_ethnicityhispanic' = 'Predominantly Hispanic \n(cat.) ',
      'log(pop)' = 'Population \n(logged) ',
      'log(B19013_001E)' = 'Median Income \n(logged) ',
      'black_nh_prop' = 'Prop. Black ',
      'asian_nh_prop' = 'Prop. Asian ',
      'hispanic_prop' = 'Prop. Hispanic ',
      'plurality_black' = 'Predominantly Black \n(binary) ',
      'plurality_white' = 'Predominantly White \n(binary) ',
      'black_nh_prop:plurality_white' = 'Plurality White x Prop. Black ',
      'black_nh_prop:plurality_black' = 'Plurality Black x Prop. Black '
    )
  )

# options("modelsummary_format_numeric_latex" = "plain")

# modelsummary(
#   nb_mods,
#   coef_rename = c(
#     'plurality_ethnicityasian_nh' = 'Predominantly Asian',
#     'plurality_ethnicityblack_nh' = 'Predominantly Black',
#     'plurality_ethnicityhispanic' = 'Predominantly Hispanic',
#     'log(pop)' = 'Population (logged)',
#     'log(B19013_001E)' = 'Median Income (logged)',
#     'black_nh_prop' = 'Prop. Black',
#     'asian_nh_prop' = 'Prop Asian',
#     'hispanic_prop' = 'Prop. Hispanic',
#     'plurality_white' = 'Predominantly White (binary)',
#     'black_nh_prop × plurality_white' = 'Prop. Black x Predominantly White',
#     'black_nh_prop:plurality_white' = 'Plurality White x Prop. Black'
#   ),
#   stars = c('*' = 0.05),
#   output = 'nb_regs.tex'
# )

ols_plurality <- 
  arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0) %>%
  feols(
    log(total_arrests) ~ plurality_ethnicity + log(pop) + log(B19013_001E)| yr,
    cluster = 'true_boroct',
    data = .
  )

ols_prop <- 
  arrests_ct_merged |> 
  filter(pop >= 1000 & B19013_001E > 0) %>%
  feols(
    log(total_arrests) ~ black_nh_prop + asian_nh_prop + hispanic_prop + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  )

ols_interaction_white <- arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0) |>
  mutate(plurality_white = ifelse(plurality_ethnicity == 'white_nh', 1, 0)) %>%
  feols(
    log(total_arrests) ~ black_nh_prop*plurality_white + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  )

ols_interaction_black <- arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('black_nh', 'white_nh')) |>
  mutate(plurality_black = ifelse(plurality_ethnicity == 'black_nh', 1, 0)) %>%
  feols(
    log(total_arrests) ~ black_nh_prop*plurality_black + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  )

ols_mods <- list(
  'Plurality Ethn.' = ols_plurality,
  'Ethn. Props.' = ols_prop,
  'Interaction w/ Plurality White' = ols_interaction_white,
  'Interaction w/ Plurality Black' = ols_interaction_black
)

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('black_nh', 'white_nh') & yr == 2024) |>
  mutate(plurality_black = ifelse(plurality_ethnicity == 'black_nh', 1, 0)) %>%
  feols(
    log(total_arrests) ~ black_nh_prop*plurality_black + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E),
    data = .
  ) |> summary()

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('black_nh', 'white_nh') & yr == 2024) |>
  mutate(
    plurality_black = ifelse(plurality_ethnicity == 'black_nh', 1, 0),
    arrest_rate = total_arrests / pop
  ) %>%
  feols(
    arrest_rate ~ black_nh_prop*plurality_black + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E),
    data = .
  ) |> summary()

# saveRDS(
#   nb_mods,
#   'data/nb_models.RDS'
# )

# saveRDS(
#   ols_mods,
#   'data/ols_mods.RDS'
# )

# modelsummary(
#   ols_mods,
#   coef_rename = c(
#     'plurality_ethnicityasian_nh' = 'Predominantly Asian',
#     'plurality_ethnicityblack_nh' = 'Predominantly Black',
#     'plurality_ethnicityhispanic' = 'Predominantly Hispanic',
#     'log(pop)' = 'Population (logged)',
#     'log(B19013_001E)' = 'Median Income (logged)',
#     'black_nh_prop' = 'Prop. Black',
#     'asian_nh_prop' = 'Prop Asian',
#     'hispanic_prop' = 'Prop. Hispanic',
#     'plurality_white' = 'Predominantly White',
#     'black_nh_prop × plurality_white' = 'Prop. Black x Predominantly White'
#   ),
#   stars = c('*' = 0.05)
#   # output = 'ols_regs.tex'
# )

diag_plots <- function(model){
  df_diag <- data.frame(
    fitted    = fitted(model),
    resid     = residuals(model)
  )

  # hist of residuals 
  resid_hist <- df_diag |> 
    ggplot(aes(
      x = resid
    )) + 
    geom_histogram() + 
    labs(
      title = 'Distribution of Residuals',
      x = 'Residuals',
      y = 'Frequency'
    ) + 
    theme_nycc() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.x = element_text(size = 9.5),
      axis.title.y = element_text(size = 9.5)
    )
  
  resid_fitted <- df_diag |> 
    ggplot(aes(
      x = fitted,
      y = resid
    )) + 
    geom_point(alpha = 0.3) + 
    geom_hline(
      yintercept = 0, color = 'red', linetype = 'dashed'
    ) + 
    geom_smooth(
      method = 'loess', se = FALSE, color = 'darkblue'
    ) + 
    labs(
      title = 'Residuals v. Fitted',
      x = 'Fitted Values',
      y = 'Residuals'
    ) + 
    theme_nycc() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.x = element_text(size = 9.5),
      axis.title.y = element_text(size = 9.5)
    )
  
  # QQ Plot 

  qq_plot <- df_diag |> 
    ggplot(aes(
      sample = resid
    )) + 
    stat_qq() + 
    stat_qq_line(color = 'red') + 
    labs(
      title = 'QQ Plot',
      x = 'Theoretical Quantiles',
      y = 'Sample Quantiles'
    ) + 
    theme_nycc() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.x = element_text(size = 9.5),
      axis.title.y = element_text(size = 9.5)
    )
  
  # Scale location plot
  scale_location <- df_diag |> 
    ggplot(aes(
      x = fitted, y = sqrt(abs(resid))
    )) + 
    geom_point(alpha = 0.3) + 
    geom_smooth(method = 'loess', se = FALSE, color = 'darkblue') + 
    labs(
      title = 'Scale-location Plot',
      x = 'Fitted Values',
      y = expression(sqrt("|Residuals|"))
    ) + 
    theme_nycc() + 
    theme(
      plot.title = element_text(hjust = 0.5, size = 12),
      axis.title.x = element_text(size = 9.5),
      axis.title.y = element_text(size = 9.5)
    )

  # return(list(resid_fitted, qq_plot, scale_location))
  (resid_hist | resid_fitted) / (qq_plot | scale_location)
}

# diag_plots(ols_mods[[1]])
# diag_plots(ols_mods[[2]])
# diag_plots(ols_mods[[3]])
# diag_plots(ols_mods[[4]])

diag_plots(nb_plurality)

library(DHARMa)

fitted_vals <- predict(nb_plurality_24, type = "response")
theta <- nb_plurality_24$theta  # check exact name via names(model) or summary(model)

n_sims <- 10000
sim_response <- sapply(1:n_sims, function(i) {
  rnbinom(length(fitted_vals), mu = fitted_vals, size = theta)
})

model_data <- arrests_ct_merged |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0 & !is.na(total_arrests) & yr == 2024)

observed_y <- model_data$total_arrests
length(observed_y)

dharma_res <- createDHARMa(
  simulatedResponse = sim_response,
  observedResponse = observed_y,  # or your df$total_arrests directly
  fittedPredictedResponse = fitted_vals,
  integerResponse = TRUE
)

plot(dharma_res)

# diag_plots(nb_mods[[1]])
# diag_plots(nb_mods[[2]])
# diag_plots(nb_mods[[3]])
# diag_plots(nb_mods[[4]])

# Dealing with extreme values - might be screwing up residuals which is why we're getting the right skew
# remove obs with counts greater than the 95th, 90th percentiles

theta_val <- nb_plurality_24$theta  # or whatever slot name shows up in names(nb_plurality)
theta_val

mu_2024 <- fitted(nb_plurality_24)  # or predict(..., type = "response")
mean(mu_2024)
mean(observed_y)  # your actual 2024 total_arrests vector

# and a direct sanity check: simulate once and compare ranges
test_sim <- rnbinom(length(mu_2024), mu = mu_2024, size = theta_val)
summary(test_sim)
summary(observed_y)

testDispersion(dharma_res)  # gives you the ratio, not just p
testUniformity(dharma_res)

#######################

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & total_arrests < quantile(arrests_ct_merged$total_arrests, 0.85, na.rm = TRUE)[[1]]) %>%
  feols(
    log(total_arrests) ~ plurality_ethnicity + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> diag_plots()

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0) |>
  filter(total_arrests < quantile(arrests_ct_merged$total_arrests, 0.9, na.rm = TRUE)[[1]]) %>%
  feols(
    log(total_arrests) ~ plurality_ethnicity + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> diag_plots()

arrests_ct_merged |> 
  filter(pop >= 1000 & B19013_001E > 0 & total_arrests < quantile(arrests_ct_merged$total_arrests, 0.9, na.rm = TRUE)[[1]]) %>%
  feols(
    log(total_arrests) ~ black_nh_prop + asian_nh_prop + hispanic_prop + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> diag_plots()

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & total_arrests < quantile(arrests_ct_merged$total_arrests, 0.9, na.rm = TRUE)[[1]]) |>
  mutate(plurality_white = ifelse(plurality_ethnicity == 'white_nh', 1, 0)) %>%
  feols(
    log(total_arrests) ~ black_nh_prop*plurality_white + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> diag_plots()

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('black_nh', 'white_nh') & total_arrests < quantile(arrests_ct_merged$total_arrests, 0.9, na.rm = TRUE)[[1]]) |>
  mutate(plurality_black = ifelse(plurality_ethnicity == 'black_nh', 1, 0)) %>%
  feols(
    log(total_arrests) ~ black_nh_prop*plurality_black + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> diag_plots()

#######################

arrests_ct_merged |> 
  mutate(
    eff_groups = 1/ct_hhi
  ) |> 
  filter(
    pop >= 1000 & B19013_001E > 0
  ) %>%
  fenegbin(
    total_arrests ~ diversity*plurality_ethnicity + log(pop) + log(B19013_001E) | yr,
    data = .,
    cluster = 'true_boroct'
  ) |> summary()

arrests_ct_merged |> 
  mutate(
    eff_groups = 1/ct_hhi
  ) |> 
  filter(
    pop >= 1000 & B19013_001E > 0
  ) %>%
  fenegbin(
    total_arrests ~ eff_groups*plurality_ethnicity + log(pop) + log(B19013_001E) | yr,
    data = .,
    cluster = 'true_boroct'
  ) |> summary()

arrests_ct_merged |> 
  filter(
    pop >= 1000 & B19013_001E > 0
  ) |> 
  group_by(plurality_ethnicity) |> 
  summarise(me = mean(diversity, na.rm = TRUE))

arrests_ct_merged |> 
  filter(
    pop >= 1000 & B19013_001E > 0
  ) |> 
  group_by(plurality_ethnicity) |> 
  summarise(me = mean(black_nh_prop, na.rm = TRUE))

arrests_ct_merged |> 
  filter(
    pop >= 1000 & B19013_001E > 0
  ) |> 
  group_by(plurality_ethnicity) |> 
  summarise(me = mean(white_nh_prop, na.rm = TRUE))

arrests_ct_merged |> 
  filter(
    pop >= 1000 & B19013_001E > 0
  ) |> 
  group_by(plurality_ethnicity) |> 
  summarise(me = mean(hispanic_prop, na.rm = TRUE))

arrests_ct_merged |> 
  summarise(
    med_arrests_black = median(arrests_black_prop, na.rm = TRUE),
    med_arrests_white = median(arrests_white_prop, na.rm = TRUE),
    med_arrests_hispanic = median(arrests_hispanic_prop, na.rm = TRUE)
  )

# created lots of new vars, so going to re-save the data 

# write_parquet(
#   arrests_ct_merged,
#   'data/output/arrests_ct_merged_6_30_26.parquet'
# )

# should control for gini - link between inequality and crime 

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0) %>%
  fenegbin(
    total_arrests ~ plurality_ethnicity + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'true_boroct',
    data = .
  )

arrests_ct_merged |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0) %>%
  fenegbin(
    total_arrests ~ black_nh_prop + asian_nh_prop + hispanic_prop + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'true_boroct',
    data = .
  )

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('white_nh', 'black_nh') & gini > 0) |>
  mutate(plurality_white = ifelse(plurality_ethnicity == 'white_nh', 1, 0)) %>%
  fenegbin(
    total_arrests ~ black_nh_prop*plurality_white + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) 

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('white_nh') & gini > 0) |>
  mutate(plurality_white = ifelse(plurality_ethnicity == 'white_nh', 1, 0)) %>%
  fenegbin(
    total_arrests ~ hispanic_prop + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'true_boroct',
    data = .
  ) |> summary()

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('black_nh') & gini > 0) |>
  mutate(plurality_white = ifelse(plurality_ethnicity == 'white_nh', 1, 0)) %>%
  fenegbin(
    total_arrests ~ I(black_nh_prop*100) + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'true_boroct',
    data = .
  ) |> summary()

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & yr == 2016) |>
  mutate(plurality_white = ifelse(plurality_ethnicity == 'white_nh', 1, 0) & gini > 0) %>%
  fenegbin(
    total_arrests ~ black_nh_prop*plurality_white + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'true_boroct',
    data = .
  ) 

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('white_nh', 'black_nh') & gini > 0) |>
  mutate(plurality_black = ifelse(plurality_ethnicity == 'black_nh', 1, 0)) %>%
  fenegbin(
    total_arrests ~ black_nh_prop*plurality_black + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'true_boroct',
    data = .
  ) 

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0) |>
  mutate(plurality_black = ifelse(plurality_ethnicity == 'black_nh', 1, 0) & yr == 2024 & gini > 0) %>%
  fenegbin(
    total_arrests ~ black_nh_prop*plurality_black + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E),
    data = .
  ) |> summary()

# ols_prop <- 
  arrests_ct_merged |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0) %>%
  feols(
    log(total_arrests) ~ black_nh_prop + asian_nh_prop + hispanic_prop + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'true_boroct',
    data = .
  )

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0) |>
  mutate(plurality_white = ifelse(plurality_ethnicity == 'white_nh', 1, 0)) %>%
  feols(
    log(total_arrests) ~ black_nh_prop*plurality_white + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  )

ols_interaction_black <- arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('black_nh', 'white_nh') & gini > 0) |>
  mutate(plurality_black = ifelse(plurality_ethnicity == 'black_nh', 1, 0)) %>%
  feols(
    log(total_arrests) ~ black_nh_prop*plurality_black + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'true_boroct',
    data = .
  )

arrests_ct_merged |> 
  mutate(
    eff_groups = 1/ct_hhi
  ) |> 
  filter(
    pop >= 1000 & B19013_001E > 0
  ) %>%
  fenegbin(
    total_arrests ~ diversity*plurality_ethnicity + log(pop) + log(B19013_001E) + gini | yr,
    data = .,
    cluster = 'true_boroct'
  ) |> summary()

arrests_ct_merged |> 
  mutate(
    eff_groups = 1/ct_hhi
  ) |> 
  filter(
    pop >= 1000 & B19013_001E > 0 & plurality_ethnicity == 'black_nh'
  ) %>%
  fenegbin(
    total_arrests ~ diversity + log(pop) + log(B19013_001E) + gini | yr,
    data = .,
    cluster = 'true_boroct'
  ) |> summary()

arrests_ct_merged |> 
  mutate(
    eff_groups = 1/ct_hhi
  ) |> 
  filter(
    pop >= 1000 & B19013_001E > 0 & plurality_ethnicity == 'white_nh'
  ) %>%
  fenegbin(
    total_arrests ~ diversity + log(pop) + log(B19013_001E) + gini | yr,
    data = .,
    cluster = 'true_boroct'
  ) |> summary()

arrests_ct_merged |> 
  mutate(
    eff_groups = 1/ct_hhi
  ) |> 
  filter(
    pop >= 1000 & B19013_001E > 0 & plurality_ethnicity == 'hispanic'
  ) %>%
  fenegbin(
    total_arrests ~ diversity + log(pop) + log(B19013_001E) + gini | yr,
    data = .,
    cluster = 'true_boroct'
  ) |> summary()


# total arrests = alpha + b diver+ b ethn + b diver*ethn

# ols_trend <- 
  arrests_ct_merged |> 
  mutate(
    post_covid = ifelse(yr >= 2020, 1, 0),
    trend = yr - 2020
  ) |> 
  filter(pop >= 1000 & B19013_001E > 0) |>
  filter(total_arrests <= quantile(total_arrests, 0.85, na.rm = TRUE) & total_arrests >= quantile(total_arrests, 0.05, na.rm = TRUE))  %>%
  feols(
    log(total_arrests) ~ post_covid*trend*plurality_ethnicity + log(pop) + log(B19013_001E) + gini,
    cluster = 'true_boroct',
    data = .
  ) |> diag_plots()

# nb_trend <- 
  arrests_ct_merged |> 
  mutate(
    post_covid = ifelse(yr >= 2020, 1, 0),
    trend = yr - 2020
  ) |> 
  filter(pop >= 1000 & B19013_001E > 0) %>%
  fenegbin(
    total_arrests ~ post_covid*trend*plurality_ethnicity + log(pop) + log(B19013_001E) + gini | true_boroct,
    cluster = 'true_boroct',
    data = .
  )

trend_mods <- list(ols_trend, nb_trend)

# saveRDS(
#   trend_mods,
#   'data/output/trend_mods.RDS'
# )

modelsummary(
  list(ols_trend, nb_trend),
  stars = c('*' = 0.05),
  # coef_rename = c(
  #   'post_covid' = 'Post-Covid',
  #   'trend' = 'Pre-Covid Trend (Years to 2020)',
  #   'plurality_ethnicityasian_nh' = 'Predominantly Asian',
  #   'plurality_ethnicityblack_nh' = 'Predominantly Black',
  #   'plurality_ethnicityhispanic' = 'Predominantly Hispanic',
  #   'log(pop)' = 'Population (Logged)',
  #   'log(B19013_001E)' = 'Median HH Income (Logged)',
  #   'gini' = 'Gini Ratio',
  #   'post_covid x trend' = 'Change in Trend Post-Covid',
  #   'post_covid x plurality_ethnicityasian_nh' = 'Post-Covid x Pred. Asian',
  #   'post_covid x plurality_ethnicityblack_nh' = 'Post-Covid x Pred. Black',
  #   'post_covid x plurality_ethnicityhispanic' = 'Post-Covid x Pred. Hispanic',
  #   'trend x plurality_ethnicityasian_nh' = 'Pre-Covid Trend x Pred. Asian',
  #   'trend x plurality_ethnicityblack_nh' = 'Pre-Covid Trend x Pred. Black',
  #   'trend x plurality_ethnicityhispanic' = 'Pre-Covid Trend x Pred. Hispanic',
  # )
)


# validating diagnostic fixes:

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0) |>
  mutate(
    trimmed_out = total_arrests >= quantile(total_arrests, 0.9, na.rm = TRUE),
  ) |>
  group_by(trimmed_out) |>
  summarise(
    mean_black_prop = mean(black_nh_prop, na.rm = TRUE),
    mean_white_plurality = mean(plurality_ethnicity == 'white_nh', na.rm = TRUE),
    n = n()
  )
# those in the top percentiles of arrests are less White - but results hold across the two specifications 

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0 & is.na(total_arrests)) |>
  count(yr, true_boroct) |>
  arrange(desc(n)) |> 
  print(n = 36)

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0) |>
  mutate(is_na_arrests = is.na(total_arrests)) |>
  group_by(is_na_arrests) |>
  summarise(mean_black_prop = mean(black_nh_prop, na.rm = TRUE), n = n())


# Going to cluster at the level of police precinct, see if that resolves issues with errors 

arrests_ct_merged2 <- read.csv('data/output/arrests_ct_merged_8_4.csv')

arrests_ct_merged2 <- arrests_ct_merged2 |> 
  mutate(plurality_ethnicity = relevel(factor(plurality_ethnicity), ref = "white_nh"))

precinct_negbin <- arrests_ct_merged2 |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0) %>%
  fenegbin(
    total_arrests ~ plurality_ethnicity + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'precinct',
    data = .
  )

fitted_vals <- predict(precinct_negbin, type = "response")
theta <- precinct_negbin$theta  # check exact name via names(model) or summary(model)

n_sims <- 10000
sim_response <- sapply(1:n_sims, function(i) {
  rnbinom(length(fitted_vals), mu = fitted_vals, size = theta)
})

model_data <- arrests_ct_merged2 |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0 & !is.na(total_arrests) & !is.na(pop) & !is.na(B19013_001E) & !is.na(gini) & !is.na(yr) & !is.na(precinct))

observed_y <- model_data$total_arrests
length(observed_y)

dharma_res <- createDHARMa(
  simulatedResponse = sim_response,
  observedResponse = observed_y,  # or your df$total_arrests directly
  fittedPredictedResponse = fitted_vals,
  integerResponse = TRUE
)

plot(dharma_res)


# testing dispersion significance 

theta_val <- precinct_negbin$theta  # or whatever slot name shows up in names(nb_plurality)
theta_val

mu_precinct <- fitted(precinct_negbin)  # or predict(..., type = "response")
mean(mu_precinct)
mean(observed_y)  # your actual 2024 total_arrests vector

# and a direct sanity check: simulate once and compare ranges
test_sim <- rnbinom(length(mu_precinct), mu = mu_precinct, size = theta_val)
summary(test_sim)
summary(observed_y)

testDispersion(dharma_res)  # gives you the ratio, not just p
testUniformity(dharma_res)

# checking grouping

testCategorical(dharma_res, catPred = model_data$plurality_ethnicity)

plotResiduals(dharma_res, form = model_data$plurality_ethnicity)

groups <- unique(model_data$plurality_ethnicity)

disp_by_group <- lapply(groups, function(g) {
  idx <- which(model_data$plurality_ethnicity == g)
  sub_res <- createDHARMa(
    simulatedResponse = sim_response[idx, , drop = FALSE],
    observedResponse = observed_y[idx],
    fittedPredictedResponse = fitted_vals[idx],
    integerResponse = TRUE
  )
  test <- testDispersion(sub_res, plot = FALSE)
  data.frame(group = g, dispersion = test$statistic, p_value = test$p.value)
})

do.call(rbind, disp_by_group)

# Do the same as above for the prop model - check how residuals are dispersed across different values of each ethnicity indicator

precinct_prop <- arrests_ct_merged2 |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0) %>%
  fenegbin(
    total_arrests ~ plurality_ethnicity + log(pop) + log(B19013_001E) + gini | yr,
    cluster = 'precinct',
    data = .
  )

# Can allow dispersion to vary by group 

model_data |> 
  filter(plurality_ethnicity %in% c("asian_nh", "white_nh")) |> 
  ggplot(aes(x = plurality_ethnicity, y = total_arrests)) +
  geom_boxplot() +
  scale_y_log10()

table(model_data$plurality_ethnicity)

m_group_dispers <- glmmTMB(
  total_arrests ~ plurality_ethnicity + log(pop) + log(B19013_001E) + gini,
  dispformula = ~ plurality_ethnicity,
  family = nbinom2,
  data = filter(model_data, yr == 2024)
)

# diagnostics 

sim_res <- simulateResiduals(m_group_dispers, n = 1000)
plot(sim_res)

testUniformity(sim_res)      # overall goodness-of-fit
testDispersion(sim_res)      # over/underdispersion, now that group-varying theta is in the model
testOutliers(sim_res)        # excess of extreme residuals
testZeroInflation(sim_res)   # check whether counts have more zeros than the negbin predicts