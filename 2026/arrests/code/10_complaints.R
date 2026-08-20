###
# Mohamed

# Objectives:
  # - bring in complaints data
  # - find complaints for index crimes
  # - merge onto arrests data
###

library(dplyr)
library(tidyverse)
library(ggplot2)
library(questionr)
library(arrow)
library(patchwork)

# council packages
library(councildown)
library(councilverse)

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

complaints_df <- read_parquet('data/output/complaints_geo.parquet')
arrests_ct_violent <- readRDS('data/output/arrests_ct_violent_7_21_26.RDS')

# get complaints for violent/ index crimes

index_crime_patterns <- c(
  'sexual abuse', 'assault', 'burglary', 'rape', 'robbery', 'sex crimes', 'theft', 'murder', 'vehicle'
)

index_crime_vars <- grep(paste(index_crime_patterns, collapse = '|'), tolower(names(complaints_df)))

complaints_violent <- complaints_df |> 
  mutate(
    index_complaints_count = rowSums(
      across(all_of(index_crime_vars)), na.rm = TRUE
    )
  ) |> 
  mutate(
    index_complaints_prop = index_complaints_count / total_complaints
  ) |> 
  mutate(
    non_index_complaints_count = total_complaints - index_complaints_count,
    non_index_complaints_prop = 1 - index_complaints_prop
  ) |> 
  rename(
    complaints_black = BLACK,
    complaints_black_hisp = `BLACK HISPANIC`,
    complaints_ai = `AMERICAN INDIAN/ALASKAN NATIVE`,
    complaints_api = `ASIAN / PACIFIC ISLANDER`,
    complaints_white = WHITE,
    complaints_white_hisp = `WHITE HISPANIC`,
    complaints_unknown_race = UNKNOWN,
    complaints_other_race = OTHER
  ) |> 
  select(
    true_boroct, yr, total_complaints, complaints_black, complaints_black_hisp, complaints_ai, complaints_api, complaints_white, complaints_white_hisp, complaints_unknown_race, complaints_other_race, index_complaints_count, index_complaints_prop, non_index_complaints_count, non_index_complaints_prop
  )

######

# merge into arrests data 

arrests_complaints_ct_merged <- arrests_ct_violent |> 
  left_join(
    complaints_violent,
    by = c('true_boroct', 'yr')
  )

# looking at where there were more arrests than there were complaints

arrests_complaints_ct_merged <- arrests_complaints_ct_merged |> 
  mutate(
    arrest_complaint_ratio = total_arrests / total_complaints,
    violent_arrests_complaints_ratio = index_crimes_count / index_complaints_count
  )

# merge in geometries

tracts20 <- st_read('https://data.cityofnewyork.us/resource/63ge-mke6.geojson?$limit=9999999')

arrests_complaints_geo <- arrests_complaints_ct_merged |> 
  rename(boroct2020 = true_boroct) |> 
  left_join(
    tracts20,
    by = 'boroct2020'
  )

arrests_complaints_geo <- st_as_sf(arrests_complaints_geo, sf_column_name = 'geometry', crs = '4326')

# saveRDS(
#   arrests_complaints_geo,
#   'data/output/arrests_complaints_geo_7_21.RDS'
# )

# closer to 1 implies more arrests compared to complaints
  ## closer to 0 implies more complaints compared to arrests 

# some regressions

# all complaints / arrests


# Removing the highest values of the DV (95% percentile) because it messes with the error terms - results are substantively similar either way, seems like it actually improves the R squared

mod_violent_plurality <- arrests_complaints_ct_merged |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0 & violent_arrests_complaints_ratio <= quantile(arrests_complaints_ct_merged[arrests_complaints_ct_merged$pop >= 1000 & arrests_complaints_ct_merged$B19013_001E > 0, ]$violent_arrests_complaints_ratio, 0.95, na.rm = TRUE)) %>%
  feols(
    log(violent_arrests_complaints_ratio) ~ plurality_ethnicity + log(B19013_001E) + log(pop) + gini | yr,
    cluster = 'true_boroct',
    data = .
  )

mod_violent_pct <- arrests_complaints_ct_merged |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0 & violent_arrests_complaints_ratio <= quantile(arrests_complaints_ct_merged[arrests_complaints_ct_merged$pop >= 1000 & arrests_complaints_ct_merged$B19013_001E > 0, ]$violent_arrests_complaints_ratio, 0.95, na.rm = TRUE)) %>%
  feols(
    log(violent_arrests_complaints_ratio) ~ black_nh_prop + hispanic_prop + asian_nh_prop + log(B19013_001E) + log(pop) + gini | yr,
    cluster = 'true_boroct',
    data = .
  )

ratio_mods <- list(
  'Arrests:Complaints Ratio' = mod_ratio_plurality, 
  'Arrests:Complaints Ratio' = mod_ratio_pct, 
  'Arrests:Complaints Ratio (Index Only)' = mod_violent_plurality, 
  'Arrests:Complaints Ratio (Index Only)' = mod_violent_pct
)

# saveRDS(
#   ratio_mods, 'data/output/ratio_mods.RDS'
# )

