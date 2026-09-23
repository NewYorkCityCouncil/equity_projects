### 
# Mohamed 

# going through arrests to only get index crimes

###

library(dplyr)
library(ggplot2)
library(patchwork)
library(tidyverse)
library(arrow)
library(sf)
library(fixest)
library(leaflet)

# council packs
library(councildown)
library(councilverse)

# load in arrests data 

arrests_ct_merged <- read_parquet('data/output/arrests_ct_merged_6_30_26.parquet')

index_crime_patterns <- c('murder', 'assault', 'rape', 'sexual abuse', 'robbery', 'theft', 'burglary', 'sex crimes')

index_crime_vars <- grep(paste(index_crime_patterns, collapse = '|'), tolower(names(arrests_ct_merged)))

arrests_ct_violent <- arrests_ct_merged |> 
  mutate(
    index_crimes_count = rowSums(
      across(all_of(index_crime_vars)), na.rm = TRUE
    )
  ) |> 
  mutate(
    index_crimes_prop = index_crimes_count / total_arrests
  ) |> 
  mutate(
    non_index_crimes_count = total_arrests - index_crimes_count,
    non_index_prop = 1 - index_crimes_prop
  ) |> 
  select(
    NAME, yr, total_arrests, pop, black_not_hisp, asian_not_hisp, hispanic, gini, hs_diploma, bachelors_deg, pop25_plus, state, county, tract, non_white_pop, white_nh_prop, black_nh_prop, asian_nh_prop, hispanic_prop, non_white_prop, youth_prop, bachelors_prop, hs_prop, majority_asian_alone, majority_black_alone, majority_hispanic, majority_non_white, majority_white_alone, true_boroct, plurality_ethnicity, arrests_black_prop, arrests_white_prop, arrests_asian_prop, arrests_hispanic_prop, ct_hhi, diversity, index_crimes_count, index_crimes_prop, non_index_crimes_count, non_index_prop, B19013_001E 
  )

# saveRDS(
#   arrests_ct_violent,
#   'data/output/arrests_ct_violent_7_21_26.RDS'
# )

#####

local_e <- new.env()

source('scripts/00_load_dependencies.R', local = local_e)

diag_plots <- local_e$diag_plots

arrests_ct_violent <- readRDS('data/output/arrests_ct_violent_7_21_26.RDS')

tracts20 <- st_read('https://data.cityofnewyork.us/resource/63ge-mke6.geojson?$limit=9999999')

arrests_geo <- arrests_ct_violent |> 
  rename(boroct2020 = true_boroct) |> 
  select(
    NAME, yr, total_arrests, pop, black_not_hisp, asian_not_hisp, hispanic, gini, hs_diploma, bachelors_deg, pop25_plus, state, county, tract, non_white_pop, white_nh_prop, black_nh_prop, asian_nh_prop, hispanic_prop, non_white_prop, youth_prop, bachelors_prop, hs_prop, majority_asian_alone, majority_black_alone, majority_hispanic, majority_non_white, majority_white_alone, boroct2020, plurality_ethnicity, arrests_black_prop, arrests_white_prop, arrests_asian_prop, arrests_hispanic_prop, ct_hhi, diversity, index_crimes_count, index_crimes_prop, non_index_crimes_count, non_index_prop
  ) |> 
  left_join(
    tracts20,
    by = 'boroct2020'
  )

arrests_geo <- st_as_sf(arrests_geo, crs = 4326, sf_column_name = 'geometry')

arrests_geo |> 
  filter(yr == 2024 & pop >= 1000) |> 
  ggplot() + 
  geom_sf(aes(fill = log(index_crimes_count))) + 
  scale_fill_viridis_c()

arrests_geo |> 
  filter(yr == 2024 & pop > 1000) |> 
  ggplot() + 
  geom_sf(aes(fill = index_crimes_prop)) + 
  scale_fill_viridis_c()

arrests_geo |> 
  filter(yr == 2024 & pop >= 1000) |> 
  ggplot() + 
  geom_sf(aes(fill = log(non_index_crimes_count))) + 
  scale_fill_viridis_c()

arrests_geo |> 
  filter(yr == 2024 & pop > 1000) |> 
  ggplot() + 
  geom_sf(aes(fill = non_index_prop)) + 
  scale_fill_viridis_c()

arrests_geo |> 
  filter(yr == 2024 & pop > 1000) |>
  mutate(
    index_crime_quintile = cut(
      total_arrests,
      breaks = quantile(index_crimes_count, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("1st", "2nd", "3rd", "4th", "5th")
    )
  ) |> 
  ggplot() + 
  geom_sf(aes(fill = index_crime_quintile)) +
  scale_fill_nycc() + 
  theme_nycc() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    title = 'Index Crime Distribution, 2024',
    fill = 'Quintile'
  ) +
  coord_sf(datum = NA)

# do we see a greater proportion of index crimes in certain areas?

arrests_ct_violent |> 
  filter(yr == 2024 & pop >= 1000 & B19013_001E > 0) %>%
  lm(
    index_crimes_prop ~ plurality_ethnicity + pop + B19013_001E,
    data = .
  ) |> summary()

arrests_ct_violent |> 
  filter(yr == 2024 & pop >= 1000 & B19013_001E > 0) %>%
  lm(
    index_crimes_prop ~ black_nh_prop + asian_nh_prop + hispanic_prop + pop + B19013_001E,
    data = .
  ) |> summary()

arrests_ct_violent |> 
  filter(yr == 2024 & pop >= 1000 & B19013_001E > 0) %>%
  lm(
    index_crimes_prop ~ I(plurality_ethnicity == 'white_nh') + pop + B19013_001E,
    data = .
  ) |> summary()

# are Black people who were arrested in White areas more likely to be arrested for minor crimes than Black people who were arrested in predominantly Black/ other areas? 

arrests_ct_violent |> 
  filter(yr == 2024 & pop >= 1000 & B19013_001E > 0) %>% 
  lm(
    index_crimes_prop ~ black_overrep,
    data = .
  ) |> summary()

arrests_ct_violent |> 
  filter(yr == 2024 & pop >= 1000 & B19013_001E > 0) %>% 
  lm(
    black_overrep ~ index_crimes_prop,
    data = .
  ) |> summary()

arrests_ct_violent |>
  filter(pop >= 1000 & B19013_001E > 0) %>%
  fenegbin(
    index_crimes_count ~ plurality_ethnicity + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> diag_plots()

arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0) %>%
  fenegbin(
    index_crimes_count ~ black_nh_prop + asian_nh_prop + hispanic_prop + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  )

# looking at index crimes relative to all crimes

arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0) %>%
  feols(
    log(index_crimes_prop) ~ plurality_ethnicity + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> summary()

arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0) %>%
  feols(
    index_crimes_prop ~ plurality_ethnicity + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> summary()

# In Predominantly Black areas, fewer relative index crimes, but very small coefficient
## could be broken window policing - smaller infractions are enforced 

arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0) %>%
  feols(
    index_crimes_prop ~ black_nh_prop + asian_nh_prop + hispanic_prop + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> diag_plots()

# R squared is still low, but is actually higher than the one we were getting by looking at all crimes (0.09)

arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0) %>%
  feols(
    log(index_crimes_count) ~ black_nh_prop + asian_nh_prop + hispanic_prop + log(pop) + log(B19013_001E),
    cluster = 'true_boroct',
    data = .
  ) |> summary()

arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0) |> 
  ggplot(aes(
    x = plurality_ethnicity,
    y = log(index_crimes_prop)
  )) + 
  geom_boxplot()

arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0) |> 
  ggplot(aes(
    x = plurality_ethnicity,
    y = log(index_crimes_prop)
  )) + 
  geom_boxplot()

arrests_ct_violent |> 
  filter(pop > 1000 & B19013_001E > 0) |> 
  group_by(plurality_ethnicity) |> 
  summarise(
    avg_index_prop = mean(index_crimes_prop, na.rm = TRUE),
    med_index_crime_prop = median(index_crimes_prop, na.rm = TRUE),
    sd_index = sd(index_crimes_prop, na.rm = TRUE),
    min_index = min(index_crimes_prop, na.rm = TRUE),
    max_index = max(index_crimes_prop, na.rm = TRUE)
  )

arrests_ct_violent |> 
  filter(pop > 1000 & B19013_001E > 0 & index_crimes_prop != 0) |> 
  group_by(plurality_ethnicity) |> 
  summarise(
    avg_index_prop = mean(log(index_crimes_prop), na.rm = TRUE),
    med_index_crime_prop = median(log(index_crimes_prop), na.rm = TRUE),
    sd_index = sd(log(index_crimes_prop), na.rm = TRUE),
    min_index = min(log(index_crimes_prop), na.rm = TRUE),
    max_index = max(log(index_crimes_prop), na.rm = TRUE)
  )

arrests_ct_violent |> 
  filter(pop > 1000 & B19013_001E > 0) |> 
  group_by(plurality_ethnicity) |> 
  summarise(
    avg_index_prop = mean(index_crimes_prop, na.rm = TRUE),
    med_index_crime_prop = median(index_crimes_prop, na.rm = TRUE),
    sd_index = sd(index_crimes_prop, na.rm = TRUE),
    min_index = min(index_crimes_prop, na.rm = TRUE),
    max_index = max(index_crimes_prop, na.rm = TRUE)
  )

######## main models ########
# Notes:
  # If we choose the level model, we include all observations
  # If we include the log model, we can only include observations where there was at least one index crime

# level model
# index_crimes_plurality_level <- 
arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0) %>%
  feols(
    index_crimes_prop ~ plurality_ethnicity + log(pop) + log(B19013_001E) + log(total_arrests) + log(gini) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> 
  # summary() |> 
  diag_plots()

# log model
arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0 & index_crimes_prop > 0) %>%
  feols(
    log(index_crimes_prop + 0.01) ~ plurality_ethnicity + log(pop) + log(B19013_001E) + log(total_arrests) + log(gini) | yr,
    cluster = 'true_boroct',
    data = .
  ) |> 
  # summary() #|> 
  diag_plots()

# save model to load into write-up

# saveRDS(
#   index_crimes_plurality_level,
#   'data/output/index_crimes_model_plurality_level.RDS'
# )

modelsummary(
  index_crimes_plurality_level,
  title = 'Estimating the Relationship between Tract Demographics and Index Crime Rates',
  output = 'gt',
  stars = FALSE,
  estimate = "{ifelse(p.value < 0.05, paste0('<strong>', sprintf('%.3f', as.numeric(estimate)), '</strong>'), sprintf('%.3f', as.numeric(estimate)))}",
  coef_map = c(
    'plurality_ethnicityasian_nh' = 'Pred. Asian', 
    'plurality_ethnicityblack_nh' = 'Pred. Black', 
    'plurality_ethnicityhispanic' = 'Pred. Hispanic', 
    'log(pop)' = 'Population (logged)',
    'log(B19013_001E)' = 'Median Income (logged)', 
    'log(total_arrests)' = 'Total Arrests (logged)', 
    'log(gini)' = 'Wealth Inequality (logged)'
  ),
  escape = FALSE
) |> 
  gt::fmt_markdown(columns = -1)

###

arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0) %>%
  feols(
    log(index_crimes_prop) ~ plurality_ethnicity + log(pop) + log(total_arrests) + log(B19013_001E) + gini| yr,
    cluster = 'true_boroct',
    data = .
  ) |> diag_plots()

arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0 & gini > 0) %>%
  feols(
    log(index_crimes_prop) ~ plurality_ethnicity + log(pop) + log(B19013_001E) + log(total_arrests) + gini| yr,
    cluster = 'true_boroct',
    data = .
  ) |> summary()

# Predominantly Black neighborhoods tend to have more crimes overall, but fewer of those crimes are violent crimes, even though they have more violent crimes in general 
## People in Predominantly Black neighborhoods largely arrested for less severe crimes

arrests_ct_merged |> 
  filter(pop >= 1000 & B19013_001E > 0) %>%
  feols(
    log(total_arrests) ~ plurality_ethnicity + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct'
  ) |> summary()

arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0) %>%
  feols(
    log(index_crimes_count) ~ plurality_ethnicity + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct'
  ) |> summary()

arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0) %>%
  feols(
    index_crimes_prop ~ total_arrests + plurality_ethnicity + log(pop) + log(B19013_001E) + gini | yr,
    data = .,
    cluster = 'true_boroct'
  ) |> summary()

arrests_ct_violent |> 
  filter(pop >= 1000 & B19013_001E > 0 & !grepl('Census Tract 1; Bronx County; New York', NAME)) %>%
  feols(
    index_crimes_prop ~ black_nh_prop + hispanic_prop + asian_nh_prop + log(pop) + log(B19013_001E) + gini | yr,
    data = .,
    cluster = 'true_boroct'
  ) |> summary()

# leaflet map of violent crime prop 

tracts20 <- st_read('https://data.cityofnewyork.us/resource/63ge-mke6.geojson?$limit=9999999')

violent_geo <- arrests_ct_violent |> 
  rename(boroct2020 = true_boroct) |> 
  left_join(
    tracts20,
    by = 'boroct2020'
  )

violent_geo <- st_as_sf(violent_geo, crs = 4326, sf_column_name = 'geometry')

# saveRDS(
#   violent_geo, 
#   'data/output/violent_geo.RDS'
# )

violent_pal <- colorBin(
  palette = 'YlOrRd',
  domain = violent_geo[violent_geo$yr == 2024, ]$index_crimes_prop
)
  
violent_geo |> 
  filter(yr == 2024) |> 
  leaflet() |> 
  addCouncilStyle() |> 
  add_council_basemaps(
  selection = c(1, 5),
  custom_names = c("Default View", "Satellite View"),
  control_position = "topright",
  control_collapsed = FALSE
  ) |>
  addTiles() |> 
  addPolygons(
    color = '#444444',
    fillColor = ~violent_pal(index_crimes_prop),
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    popup = ~paste0('<b>Census Tract:</b> ', NAME, '<br><b>Pct. Index Crimes: </b>', paste0(round(index_crimes_prop, 3)*100, '%'))
  )


arrests_ct_violent |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('black_nh', 'white_nh')) |> 
  group_by(plurality_ethnicity, yr) |> 
  summarise(
    mean_index_prop = mean(index_crimes_prop, na.rm = TRUE)
  ) |> 
  ggplot(aes(
    x = yr,
    y = mean_index_prop,
    color = plurality_ethnicity
  )) + 
  geom_point() + 
  geom_line()

arrests_ct_violent |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('black_nh', 'white_nh', 'hispanic')) |> 
  group_by(plurality_ethnicity, yr) |> 
  summarise(
    mean_index_prop = mean(index_crimes_count, na.rm = TRUE)
  ) |> 
  mutate(
    plurality_ethnicity = as.character(plurality_ethnicity),
    plurality_ethnicity = str_to_sentence(str_extract(plurality_ethnicity, '^[^_]+'))
  ) |> 
  ggplot(aes(
    x = yr,
    y = mean_index_prop,
    color = plurality_ethnicity
  )) + 
  geom_point() + 
  geom_line() + 
  labs(
    title = 'Average Number of Index Crime Arrests per CT,\nby Predominant Ethnicity, 2012 - 2024',
    x = '',
    y = 'Mean Number of Arrests per CT',
    color = 'Predominant\nEthnicity'
  ) + 
  theme_nycc() + 
  scale_color_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# The overall drop in arrests in the pre-Covid era was especially pronounced in Black communities
## The post-COVID increase seems to have been similar across Black White and Hispanic communities 




