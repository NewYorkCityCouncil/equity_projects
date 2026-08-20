### 
# Mohamed 
# June 15, 2026

# EDA on arrests data 
###

library(dplyr)
library(ggplot2)
library(tidyverse)
library(arrow)
library(fixest)
library(sf)

# council packages
library(councildown)
library(councilverse)

arrests_all <- read_parquet('data/output/nypd_arrests_6_5_26.parquet')

# arrests_all <- read_parquet('data/output/nypd_arrests_6_5_26.parquet')

# arrests_ct_merged <- read_parquet('data/output/arrests_ct_merged_6_15_26.parquet')
# arrests_ct_merged <- as.data.frame(arrests_ct_merged)
# arrests_ct_merged <- mutate(arrests_ct_merged, arrest_rate = total_arrests / pop)

# arrests_ct_merged <- arrests_ct_merged |> 
#   mutate(median_hh_income = B19013_001E)

# use this updated dataset and ignore the above:
arrests_ct_merged <- read_parquet('data/output/arrests_ct_merged_6_30_26.parquet')


arrests_ct_merged <- arrests_ct_merged |> 
  mutate(
    arrests_black_prop = BLACK / total_arrests,
    arrests_white_prop = WHITE / total_arrests,
    arrests_hispanic_prop = (`WHITE HISPANIC` + `BLACK HISPANIC`) / total_arrests,
    arrests_asian_prop = `ASIAN / PACIFIC ISLANDER` / total_arrests
  )

## looking at arrest rate among ethnicities over time

arrests_by_ethnicity <- arrests_ct_merged |> 
  rowwise() |> 
  mutate(
    HISPANIC = sum(`WHITE HISPANIC`, `BLACK HISPANIC`, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  pivot_longer(
    cols = c('BLACK', 'WHITE', 'HISPANIC'),
    names_to = 'ethnicity',
    values_to = 'ethn_arrests'
  ) |> 
  select(NAME, yr, total_arrests, ethnicity, ethn_arrests)

arrests_pop <- arrests_ct_merged |> 
  pivot_longer(
    cols = c(black_not_hisp, white_not_hisp, hispanic),
    names_to = 'ethnicity',
    values_to = 'ethn_count_ct'
  ) |> 
  select(NAME, yr, ethnicity, ethn_count_ct) |> 
  mutate(
    ethnicity = case_when(
      ethnicity == 'black_not_hisp' ~ 'BLACK',
      ethnicity == 'white_not_hisp' ~ 'WHITE',
      ethnicity == 'hispanic' ~ 'HISPANIC'
    )
  )

arrests_ct2 <- arrests_by_ethnicity |>  
  left_join(arrests_pop, by = c('NAME', 'yr', 'ethnicity'))

pop_props <- arrests_ct2 |> 
  group_by(yr, ethnicity) |> 
  summarise(sum_ethn_pop = sum(ethn_count_ct, na.rm = TRUE)) |> 
  mutate(pop_ethn_prop = sum_ethn_pop / sum(sum_ethn_pop, na.rm = TRUE))

arrests_ct2 |> 
  group_by(yr, ethnicity) |> 
  summarise(
    sum_ethn_arrests = sum(ethn_arrests, na.rm = TRUE)
  ) |> 
  mutate(total_arrests = sum(sum_ethn_arrests, na.rm = TRUE)) |> 
  mutate(arrests_ethn_prop = sum_ethn_arrests / total_arrests) |> 
  ggplot(aes(
    x = yr,
    y = arrests_ethn_prop,
    color = ethnicity
  )) + 
  geom_line(aes(linetype = 'Arrests')) + 
  geom_point() + 
  geom_line(
    data = pop_props,
    aes(
      x = yr, y = pop_ethn_prop, color = ethnicity,
      linetype = 'Population'
    ),
  ) + 
  scale_color_nycc() + 
  scale_linetype_manual(
    values = c('Arrests' = 'solid', 'Population' = 'dashed'), name = ''
  ) +
  labs(
    title = 'Arrest Rates by \n Race/ Ethnicity, 2009 - 2024',
    x = '',
    y = 'Proportion Belonging \n to Racial/ Ethnic Category',
    color = 'Ethnicity'
  ) +
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust=0.5)
  )

ggsave(
  'visuals/arrest_rates_population.png',
  height = 5,
  width = 8
)

### Mapping by CT

# merge onto 2020 CTs

tracts20 <- st_read('https://data.cityofnewyork.us/resource/63ge-mke6.geojson?$limit=9999999')

arrests_geo <- arrests_ct_merged |> 
  rename(boroct2020 = true_boroct) |> 
  select(
    NAME, yr, total_arrests, pop, black_not_hisp, asian_not_hisp, hispanic, gini, hs_diploma, bachelors_deg, pop25_plus, state, county, tract, non_white_pop, white_nh_prop, black_nh_prop, asian_nh_prop, hispanic_prop, non_white_prop, youth_prop, bachelors_prop, hs_prop, majority_asian_alone, majority_black_alone, majority_hispanic, majority_non_white, majority_white_alone, boroct2020, plurality_ethnicity, arrests_black_prop, arrests_white_prop, arrests_asian_prop, arrests_hispanic_prop, ct_hhi, diversity, B19013_001E
  ) |> 
  left_join(
    tracts20,
    by = 'boroct2020'
  )

arrests_geo <- st_as_sf(arrests_geo, crs = 4326, sf_column_name = 'geometry')

saveRDS(
  arrests_geo,
  'data/output/arrests_geo.RDS'
)

arrests_geo |> 
  filter(yr == 2024) |> 
  ggplot() + 
  geom_sf(aes(fill = total_arrests)) + 
  scale_fill_viridis_c() + 
  labs(
    title = 'Total Arrests by Census Tract, 2024',
    fill = 'Total Arrests'
  ) + 
  theme_nycc() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_sf(datum = NA) 

# plotting quintiles of arrests instead of counts 

arrests_geo |> 
  filter(yr == 2024 & pop >= 100) |> 
  mutate(
    arrests_quintile = cut(
      total_arrests,
      breaks = quantile(total_arrests, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("1st", "2nd", "3rd", "4th", "5th")
    )
  ) |> 
  ggplot() + 
  geom_sf(aes(fill = arrests_quintile)) + 
  scale_fill_viridis_d() + 
  labs(
    title = 'Total Arrests by Census Tract, 2024',
    fill = 'Quintile'
  ) + 
  theme_nycc() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_sf(datum = NA)

arrests_geo |> 
  filter(yr == 2024) |> 
  mutate(log_arrests = log(total_arrests)) |> 
  ggplot() + 
  geom_sf(aes(fill = log_arrests)) + 
  scale_fill_viridis_c() + 
  labs(
    title = 'Log Arrests by Census Tract, 2024',
    fill = 'Log Arrests'
  ) + 
  theme_nycc() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_sf(datum = NA) 

arrests_geo |> 
  filter(yr == 2024) |> 
  mutate(arrests_per1k = total_arrests / (pop / 1000)) |> 
  ggplot() + 
  geom_sf(aes(fill = arrests_per1k)) + 
  scale_fill_viridis_c() + 
  labs(
    title = 'Arrest Rate by Census Tract, 2024',
    fill = 'Arrests per \n 1k Residents Arrests'
  ) + 
  theme_nycc() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_sf(datum = NA) 

# quintile of rates

arrests_geo |> 
  filter(yr == 2024 & pop >= 100) |> 
  mutate(
    arrests_per1k = total_arrests / (pop / 1000),
    arrests_rate_quintile = cut(
      arrests_per1k,
      breaks = quantile(arrests_per1k, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("1st", "2nd", "3rd", "4th", "5th")
    )
  ) |> 
  ggplot() + 
  geom_sf(aes(fill = arrests_rate_quintile)) + 
  scale_fill_viridis_d() + 
  labs(
    title = 'Arrest Rate by Census Tract, 2024',
    fill = 'Quintile'
  ) + 
  theme_nycc() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_sf(datum = NA)

arrests_geo |> 
  filter(yr == 2024 & pop > 100) |> 
  ggplot() + 
  geom_sf(aes(fill = plurality_ethnicity)) + 
  labs(
    title = 'Plurality Ethnicity by Census Tract, 2024',
    fill = ''
  ) + 
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  ) + 
  scale_fill_nycc(
    labels = c(
      'White, non-Hispanic',
      'Asian',
      'Black, non-Hispanic',
      'Hispanic'
    )
  ) +
  coord_sf(datum = NA)

arrests_geo |> 
  filter(yr == 2024) |> 
  ggplot() + 
  geom_sf(aes(fill = bachelors_prop)) + 
  scale_fill_viridis_c()

arrests_geo |> 
  filter(yr == 2024) |> 
  ggplot() + 
  geom_sf(aes(fill = arrests_black_prop)) + 
  scale_fill_viridis_c() + 
  theme_nycc() + 
  coord_sf(datum = NA)

# load in demographics data to see if we have all borocts 

dems <- read_parquet('data/output/ct_level_demographics_09_24.parquet')

dems <- dems |> 
  rename(
    boroct2020 = true_boroct
  ) |> 
  left_join(
    tracts20, by = 'boroct2020'
  )

dems <- st_as_sf(dems, sf_column_name = 'geometry', crs = 4326)

dems |> 
  filter(yr == 2024) |> 
  ggplot() + 
  geom_sf()

tracts10 <- st_read('https://data.cityofnewyork.us/resource/bmjq-373p.geojson?$limit=99999999')

tracts20 |> 
  ggplot() + 
  geom_sf() + 
  labs(title = '2020 tracts')

tracts10 |> 
  ggplot() + 
  geom_sf() + 
  labs(title = '2010 tracts')

# Looking at Felonies

arrests_geo <- arrests_ct_merged |> 
  rename(boroct2020 = true_boroct) |> 
  select(
    NAME, yr, total_arrests, pop, black_not_hisp, asian_not_hisp, hispanic, gini, hs_diploma, bachelors_deg, pop25_plus, state, county, tract, non_white_pop, white_nh_prop, black_nh_prop, asian_nh_prop, hispanic_prop, non_white_prop, youth_prop, bachelors_prop, hs_prop, majority_asian_alone, majority_black_alone, majority_hispanic, majority_non_white, majority_white_alone, boroct2020, plurality_ethnicity
  ) |> 
  left_join(
    tracts20,
    by = 'boroct2020'
  )

felonies_geo <- felonies_ct_merged |> 
  rename(boroct2020 = true_boroct) |> 
  select(
    NAME, yr, total_arrests, pop, black_not_hisp, asian_not_hisp, hispanic, gini, hs_diploma, bachelors_deg, pop25_plus, state, county, tract, non_white_pop, white_nh_prop, black_nh_prop, asian_nh_prop, hispanic_prop, non_white_prop, youth_prop, bachelors_prop, hs_prop, majority_asian_alone, majority_black_alone, majority_hispanic, majority_non_white, majority_white_alone, boroct2020, plurality_ethnicity
  ) |> 
  left_join(
    tracts20,
    by = 'boroct2020'
  )

felonies_geo <- st_as_sf(felonies_geo, sf_column_name = 'geometry', crs = 4326)

felonies_geo |> 
  filter(yr == 2024) |> 
  ggplot() + 
  geom_sf(aes(fill = total_arrests)) + 
  scale_fill_viridis_c() + 
  labs(
    title = 'Total Felonies by Census Tract, 2024',
    fill = 'Total Felonies'
  ) + 
  theme_nycc() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_sf(datum = NA) 

felonies_geo |> 
  filter(yr == 2024) |> 
  mutate(log_arrests = log(total_arrests)) |> 
  ggplot() + 
  geom_sf(aes(fill = log_arrests)) + 
  scale_fill_viridis_c() + 
  labs(
    title = 'Log Felonies by Census Tract, 2024',
    fill = 'Log Felonies'
  ) + 
  theme_nycc() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_sf(datum = NA) 

felonies_geo |> 
  filter(yr == 2024) |> 
  mutate(arrests_per1k = total_arrests / (pop / 1000)) |> 
  mutate(log_rate = log(arrests_per1k)) |> 
  ggplot() + 
  geom_sf(aes(fill = log_rate)) + 
  scale_fill_viridis_c() + 
  labs(
    title = 'Felony Arrest Rate by Census Tract, 2024',
    fill = 'Felony Arrests per \n 1k Residents'
  ) + 
  theme_nycc() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_sf(datum = NA) 

felonies_geo |> 
  filter(yr == 2024) |> 
  mutate(arrests_per1k = total_arrests / (pop / 1000)) |> 
  mutate(log_rate = log(arrests_per1k)) |> 
  ggplot() + 
  geom_histogram(aes(x = log_rate))

### Plots based on models 

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('black_nh', 'white_nh')) |> 
  ggplot(aes(
    x = black_nh_prop,
    y = total_arrests,
    color = plurality_ethnicity
  )) + 
  geom_point() + 
  geom_smooth(method = 'lm')

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('black_nh', 'white_nh')) |> 
  ggplot(aes(
    x = black_nh_prop,
    y = log(total_arrests),
    color = plurality_ethnicity
  )) + 
  geom_point(alpha = 0.1) + 
  geom_smooth(method = 'lm') + 
  labs(
    title = 'Arrest Rates and Plurality Ethnicity, 2024',
    x = 'Proportion of the Population \n Identifying as Black',
    y = 'Total Arrests (logged)',
    fill = 'Plurality Ethnicity'
  ) +
  theme_nycc() + 
  scale_color_nycc() +
  theme(plot.title = element_text(hjust = 0.5))

arrests_ct_merged |> 
  filter(yr == 2024 & pop >= 1000 & plurality_ethnicity %in% c('white_nh', 'black_nh')) |> 
  ggplot(aes(
    x = black_nh_prop,
    y = log(total_arrests),
    color = plurality_ethnicity
  )) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  labs(
    title = 'Black Share of the Population and Arrest Rates',
    x = 'Black Share of CT Population',
    y = 'Total Arrests (logged)',
    color = 'Plurality Ethnicity'
  ) + 
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  ) + 
  scale_color_nycc(
    labels = c('white_nh' = 'White', 'black_nh' = 'Black')
  )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0) |> 
  ggplot(aes(
    x = black_nh_prop,
    y = log(total_arrests)
  )) + 
  geom_point(alpha = 0.1) + 
  geom_smooth(method = 'lm') + 
  labs(
    title = 'Arrest Rates and Plurality Ethnicity, 2024',
    x = 'Proportion of the Population \n Identifying as Black',
    y = 'Total Arrests (logged)'
  ) +
  theme_nycc() + 
  scale_color_nycc(palette = 'single') +
  theme(plot.title = element_text(hjust = 0.5))

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0) |> 
  ggplot(aes(
    x = plurality_ethnicity,
    y = log(total_arrests),
    color = plurality_ethnicity
  )) + 
  geom_boxplot() + 
  labs(
    title = 'Distribution of Arrests \n by CT Plurality Ethnicity',
    x = 'Predominant Ethnicity in CT',
    y = 'Logged Count of Arrests'
  ) +
  theme_nycc() + 
  theme(
    legend.position = 'none',
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_color_nycc() + 
  scale_x_discrete(
    labels = c('White', 'Asian', 'Black', 'Hispanic/ Latino')
  )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity == 'white_nh' & yr == 2024) |> 
  ggplot(aes(
    x = black_nh_prop,
    y = log(total_arrests)
  )) + 
  geom_point() + 
  geom_smooth(method = 'loess')

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity == 'white_nh' & yr == 2024) |> 
  ggplot(aes(
    x = black_nh_prop,
    y = log(total_arrests)
  )) + 
  geom_point() + 
  geom_smooth(method = 'loess') + 
  labs(
    title = 'Demographic Share and Arrests among Predominantly White CTs',
    x = 'Black share of CT Population',
    y = 'Total Arrests (logged)'
  ) +
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity == 'black_nh' & yr == 2024) |> 
  ggplot(aes(
    x = black_nh_prop,
    y = log(total_arrests)
  )) + 
  geom_point() + 
  geom_smooth(
    method = 'loess'
  ) + 
  labs(
    title = 'Demographic Share and Arrests among Predominantly Black CTs',
    x = 'Black share of CT Population',
    y = 'Total Arrests (logged)'
  ) +
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity == 'hispanic' & yr == 2024) |> 
  ggplot(aes(
    x = black_nh_prop,
    y = log(total_arrests)
  )) + 
  geom_point() + 
  geom_smooth(
    method = 'loess'
  ) + 
  labs(
    title = 'Demographic Share and Arrests among Predominantly Latino CTs',
    x = 'Black share of CT Population',
    y = 'Total Arrests (logged)'
  ) +
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity == 'white_nh' & yr == 2024) |> 
  ggplot(aes(
    x = white_nh_prop,
    y = log(total_arrests)
  )) + 
  geom_point() + 
  geom_smooth(
    method = 'loess'
  ) + 
  labs(
    title = 'Demographic Share and Arrests among Predominantly White CTs',
    x = 'White share of CT Population',
    y = 'Total Arrests (logged)'
  ) +
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity == 'black_nh' & yr == 2024) |> 
  ggplot(aes(
    x = white_nh_prop,
    y = log(total_arrests)
  )) + 
  geom_point() + 
  geom_smooth(
    method = 'loess'
  ) + 
  labs(
    title = 'Demographic Share and Arrests among Predominantly Black CTs',
    x = 'White share of CT Population',
    y = 'Total Arrests (logged)'
  ) +
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity == 'hispanic' & yr == 2024) |> 
  ggplot(aes(
    x = hispanic_prop,
    y = log(total_arrests)
  )) + 
  geom_point() + 
  geom_smooth(
    method = 'loess'
  ) + 
  labs(
    title = 'Demographic Share and Arrests among Predominantly Hispanic CTs',
    x = 'Hispanic share of CT Population',
    y = 'Total Arrests (logged)'
  ) +
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity == 'hispanic' & yr == 2024) |> 
  ggplot(aes(
    x = white_nh_prop,
    y = log(total_arrests)
  )) + 
  geom_point() + 
  geom_smooth(
    method = 'loess'
  ) + 
  labs(
    title = 'Demographic Share and Arrests among Predominantly Hispanic CTs',
    x = 'White share of CT Population',
    y = 'Total Arrests (logged)'
  ) +
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity == 'hispanic' & yr == 2024) |> 
  ggplot(aes(
    x = black_nh_prop,
    y = log(total_arrests)
  )) + 
  geom_point() + 
  geom_smooth(
    method = 'loess'
  ) + 
  labs(
    title = 'Demographic Share and Arrests among Predominantly Hispanic CTs',
    x = 'Black share of CT Population',
    y = 'Total Arrests (logged)'
  ) +
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity == 'white_nh' & yr == 2024) |> 
  ggplot(aes(
    x = hispanic_prop,
    y = log(total_arrests)
  )) + 
  geom_point() + 
  geom_smooth(
    method = 'loess'
  ) + 
  labs(
    title = 'Demographic Share and Arrests among Predominantly White CTs',
    x = 'Hispanic share of CT Population',
    y = 'Total Arrests (logged)'
  ) +
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity == 'black_nh' & yr == 2024) |> 
  ggplot(aes(
    x = black_nh_prop,
    y = log(total_arrests)
  )) + 
  geom_point() + 
  geom_smooth(method = 'loess') + 
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

arrests_by_ethnicity |> 
  filter(yr == 2024) |> 
  group_by(ethnicity) |> 
  summarise(n_arrests = sum(ethn_arrests, na.rm = TRUE)) |> 
  mutate(
    prop_arrests_ethn = n_arrests / sum(n_arrests)
  )


# Look at where the most arrests of Black people are occurring 

arrests_by_ethnicity <- arrests_by_ethnicity |> 
  mutate(
    arrests_ethn_prop = ethn_arrests / total_arrests
  )

# find where the most arrests of Black individuals are

arrests_ethn_dem_merged <- arrests_ct_merged |> 
  select(
    NAME, pop, black_not_hisp, asian_not_hisp, white_not_hisp, B19013_001E, hispanic,
    yr, state, county, tract, non_white_pop, 
    white_nh_prop, black_nh_prop, asian_nh_prop, hispanic_prop, non_white_prop, plurality_ethnicity, true_boroct
  )

# create indicator for whether the share of of people getting arrested from ethnicity i is greater than their share of the population 

arrests_ct_merged <- arrests_ct_merged |> 
  mutate(
    black_overrep = ifelse(arrests_black_prop > black_nh_prop, 1, 0),
    hisp_overrep = ifelse(arrests_hispanic_prop > hispanic_prop, 1, 0),
    white_overrep = ifelse(arrests_white_prop > white_nh_prop, 1, 0),
    asian_overrep = ifelse(arrests_asian_prop > asian_nh_prop, 1, 0),
  )

arrests_ct_merged |> 
  filter(yr == 2024 & !is.na(plurality_ethnicity)) |> 
  group_by(plurality_ethnicity) |> 
  summarise(
    black_overrep = mean(black_overrep, na.rm = TRUE),
    white_overrep = mean(white_overrep, na.rm = TRUE),
    hisp_overrep = mean(hisp_overrep, na.rm = TRUE),
    asian_overrep = mean(asian_overrep, na.rm = TRUE)
    # med_black_prop = median(black_nh_prop, na.rm = TRUE),
    # med_white_prop = median(white_nh_prop, na.rm = TRUE),
    # med_hisp_prop = median(hispanic_prop, na.rm = TRUE),
    # med_asian_prop = median(asian_nh_prop, na.rm = TRUE)
  ) |> 
  pivot_longer(
    cols = c(black_overrep, white_overrep, hisp_overrep, asian_overrep),
    names_to = 'ethnicity',
    values_to = 'prop_overrep'
  ) |> 
  ggplot(aes(
    x = plurality_ethnicity,
    y = prop_overrep,
    fill = ethnicity
  )) + 
  geom_col(position = position_dodge()) + 
  labs(
    title = 'Proportion of CTs in which \n an Ethnic Group is Overrepresented in Arrests',
    x = 'CT Plurality Ethnicity',
    y = 'Proportion of CTs \n in which Ethnicity is Overrepresented',
    fill = 'Ethnicity \nOverrepresented'
  ) +
  theme_nycc() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) + 
  scale_fill_nycc(
    labels = c('Asian', 'Black', 'Hispanic/ Latino', 'White')
  ) + 
  scale_x_discrete(
    labels = c('White', 'Asian', 'Black', 'Hispanic')
  )

# figure displays the proportion of CTs in which a given ethnicity (x axis) is overrepresented in a community of a given ethnic plurality (fill)
arrests_ct_merged |> 
  filter(yr == 2024 & !is.na(plurality_ethnicity)) |> 
  group_by(plurality_ethnicity) |> 
  summarise(
    black_overrep = mean(black_overrep, na.rm = TRUE),
    white_overrep = mean(white_overrep, na.rm = TRUE),
    hisp_overrep = mean(hisp_overrep, na.rm = TRUE),
    asian_overrep = mean(asian_overrep, na.rm = TRUE)
  ) |> 
  pivot_longer(
    cols = c(black_overrep, white_overrep, hisp_overrep, asian_overrep),
    names_to = 'ethnicity',
    values_to = 'prop_overrep'
  ) |> 
  ggplot(aes(
    x = ethnicity,
    y = prop_overrep,
    fill = plurality_ethnicity
  )) + 
  geom_col(position = position_dodge()) + 
  labs(
    title = 'Proportion of CTs in which \n an Ethnic Group is Overrepresented in Arrests',
    x = 'Ethnicity Overrepresented',
    y = 'Proportion of CTs \n in which Ethnicity is Overrepresented',
    fill = 'Plurality Community'
  ) +
  theme_nycc() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) + 
  scale_fill_nycc(
    labels = c('White', 'Asian', 'Black', 'Hispanic/\n Latino')
  ) + 
  scale_x_discrete(
    labels = c('Asian', 'Black', 'Hispanic/\n Latino', 'White')
  )

# Similar proportions of overrepresentation among Black arrestees in both plurality Black and plurality White neighborhoods 

# Looking at it by income

arrests_ct_merged |> 
  filter(yr == 2024 & !is.na(plurality_ethnicity)) |> 
  mutate(
    income_quintile = cut(
      B19013_001E,
      breaks = quantile(B19013_001E, probs = seq(0, 1, 0.2), na.rm = TRUE),
      include.lowest = TRUE,
      labels = c("1st", "2nd", "3rd", "4th", "5th")
    )
  ) |> 
  group_by(income_quintile) |> 
  summarise(
    black_overrep = mean(black_overrep, na.rm = TRUE),
    white_overrep = mean(white_overrep, na.rm = TRUE),
    hisp_overrep = mean(hisp_overrep, na.rm = TRUE),
    asian_overrep = mean(asian_overrep, na.rm = TRUE)
    # med_black_prop = median(black_nh_prop, na.rm = TRUE),
    # med_white_prop = median(white_nh_prop, na.rm = TRUE),
    # med_hisp_prop = median(hispanic_prop, na.rm = TRUE),
    # med_asian_prop = median(asian_nh_prop, na.rm = TRUE)
  ) |> 
  pivot_longer(
    cols = c(black_overrep, white_overrep, hisp_overrep, asian_overrep),
    names_to = 'ethnicity',
    values_to = 'prop_overrep'
  ) |> 
  ggplot(aes(
    x = income_quintile,
    y = prop_overrep,
    fill = ethnicity
  )) + 
  geom_col(position = position_dodge()) + 
  labs(
    title = 'Proportion of CTs in which \n an Ethnic Group is Overrepresented in Arrests',
    x = 'Income Quintile',
    y = 'Proportion of CTs \n in which Ethnicity is Overrepresented',
    fill = 'Ethnicity \nOverrepresented'
  ) +
  theme_nycc() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) #+ 
  # scale_fill_nycc(
  #   labels = c('Asian', 'Black', 'Hispanic/ Latino', 'White')
  # ) + 
  # scale_x_discrete(
  #   labels = c('White', 'Asian', 'Black', 'Hispanic')
  # )

# Diversity and arrests

# arrests_ct_merged <- arrests_ct_merged |> 
#   mutate(
#     ct_hhi = white_nh_prop^2 + black_nh_prop^2 + asian_nh_prop^2 + hispanic_prop^2
#   ) |> 
#   mutate(
#     diversity = 1 - ct_hhi
#   )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & yr == 2024) |> 
  ggplot(aes(
    x = diversity,
    y = log(total_arrests),
    color = plurality_ethnicity
  )) + 
  geom_point(aes(alpha = non_white_prop)) + 
  geom_smooth(method = 'lm') + 
  labs(
    title = 'Diversity Levels and Arrests, 2024',
    x = '1 - HHI',
    y = 'Total Arrests \n (Logged)',
    color = 'Plurality Ethnicity',
    alpha = 'Prop. \n Non-White'
  ) + 
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  ) + 
  scale_color_nycc(
    labels = c('White', 'Asian', 'Black', 'Hispanic/\nLatino')
  )

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & yr == 2024) |> 
  ggplot(aes(
    x = non_white_prop,
    y = log(total_arrests),
    color = plurality_ethnicity
  )) + 
  geom_point(aes(alpha = diversity)) + 
  geom_smooth(method = 'lm') + 
  labs(
    title = 'Non-White Population Share and Arrest Rates, 2024',
    x = 'Non-White Share \n of CT Population',
    y = 'Total Arrests \n (Logged)',
    color = 'Plurality Ethnicity',
    alpha = 'Diversity'
  ) + 
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  ) + 
  scale_color_nycc(
    labels = c('White', 'Asian', 'Black', 'Hispanic/\nLatino')
  )

arrests_ct_merged |>
  filter(pop >= 1000 & B19013_001E > 0) %>%
  feols(
    log(total_arrests) ~ diversity + log(pop) + log(B19013_001E) | yr,
    cluster = 'true_boroct',
    data = .
  )

# leaflet map 

arrests_pal <- colorQuantile(
  palette = 'YlOrRd',
  domain = arrests_geo[arrests_geo$pop > 1000, ]$total_arrests,
  n = 5
)

arrests_breaks <- quantile(arrests_geo$total_arrests, probs = seq(0, 1, 0.2), na.rm = TRUE)

pal_labels <- c(
  paste0("0–20th (", arrests_breaks[1], "–", arrests_breaks[2], ")"),
  paste0("20–40th (", arrests_breaks[2], "–", arrests_breaks[3], ")"),
  paste0("40–60th (", arrests_breaks[3], "–", arrests_breaks[4], ")"),
  paste0("60–80th (", arrests_breaks[4], "–", arrests_breaks[5], ")"),
  paste0("80–100th (", arrests_breaks[5], "–", arrests_breaks[6], ")")
)

arrests_geo |> 
  filter(yr == 2024) |> 
  mutate(total_arrests = ifelse(pop < 1000, NA, total_arrests)) |> 
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
    fillColor = ~arrests_pal(total_arrests),
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    popup = ~paste0('<b>Census Tract:</b> ', NAME, '<br><b>Total Arrests: </b>', total_arrests)
  ) |> 
  addLegend(
    colors   = RColorBrewer::brewer.pal(5, "YlOrRd"),
    values = ~total_arrests,
    labels = pal_labels,
    title = 'Total Arrests',
    position = 'bottomright'
  )

diversity_pal <- colorNumeric(
  palette = 'YlOrRd',
  domain = arrests_geo[arrests_geo$pop > 1000, ]$diversity
)

arrests_geo |> 
  filter(yr == 2024) |> 
  mutate(diversity = ifelse(pop < 1000, NA, diversity)) |> 
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
    fillColor = ~diversity_pal(diversity),
    weight = 1,
    opacity = 1,
    fillOpacity = 0.7,
    popup = ~paste0('<b>Census Tract:</b> ', NAME, '<br><b>Diversity Coefficient: </b>', diversity)
  ) #|> 
  addLegend(
    colors   = 'YlOrRd',
    values = ~diversity,
    title = 'Diversity Coefficient',
    position = 'bottomright'
  )

# figures for dashboard

arrests_ct_merged |> 
  filter(yr == 2024 & pop >= 1000 & plurality_ethnicity %in% c('white_nh', 'hispanic')) |> 
  ggplot(aes(
    x = hispanic_prop,
    y = log(total_arrests),
    color = plurality_ethnicity
  )) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  labs(
    title = 'Hispanic/ Latino Share \nof the Population and Arrest Rates',
    x = 'Hispanic/ Latino Share of CT Population',
    y = 'Total Arrests (logged)',
    color = 'Plurality Ethnicity'
  ) + 
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  ) + 
  scale_color_nycc(
    labels = c('white_nh' = 'White', 'hispanic' = 'Hispanic/ Latino')
  )

arrests_geo |> 
  filter(yr == 2024 & pop >= 1000 & plurality_ethnicity %in% c('white_nh', 'hispanic') & gini > 0) |> 
  ggplot() + 
  geom_sf(aes(fill = gini)) + 
  scale_fill_viridis_c()

arrests_ct_merged |> 
  filter(yr == 2024 & pop >= 1000) |> 
  ggplot(aes(
    x = white_nh_prop,
    y = log(total_arrests)
  )) + 
  geom_point(color = 'darkred') + 
  geom_smooth() + 
  labs(
    title = 'White Share \nof the Population and Arrest Rates',
    x = 'White Share of CT Population',
    y = 'Total Arrests (logged)'
  ) + 
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  ) 

arrests_ct_merged |> 
  filter(yr == 2024 & pop >= 1000 & plurality_ethnicity %in% c('white_nh', 'black_nh')) |> 
  ggplot(aes(
    x = white_nh_prop,
    y = log(total_arrests),
    color = plurality_ethnicity
  )) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  labs(
    title = 'White Share \nof the Population and Arrest Rates',
    x = 'White Share of CT Population',
    y = 'Total Arrests (logged)'
  ) + 
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )  + 
  scale_color_nycc()

arrests_ct_merged |> 
  filter(yr == 2024 & pop >= 1000 & plurality_ethnicity %in% c('hispanic', 'black_nh')) |> 
  ggplot(aes(
    x = black_nh_prop,
    y = log(total_arrests),
    color = plurality_ethnicity
  )) + 
  geom_point() + 
  geom_smooth(method = 'lm') +
  labs(
    title = 'Black Share of the Population \nand Arrest Rates',
    x = 'Black Share of CT Population',
    y = 'Total Arrests (logged)',
    color = 'Plurality Ethnicity'
  ) + 
  theme_nycc() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_nycc()

arrests_ct_merged |> 
  filter(yr == 2024 & pop >= 1000 & plurality_ethnicity %in% c('hispanic', 'black_nh')) |> 
  ggplot(aes(
    x = hispanic_prop,
    y = log(total_arrests),
    color = plurality_ethnicity
  )) + 
  geom_point() + 
  geom_smooth(method = 'lm') 

arrests_ct_merged |> 
  filter(yr == 2024 & pop >= 1000) |> 
  ggplot(aes(
    x = plurality_ethnicity,
    y = log(total_arrests),
    color = plurality_ethnicity
  )) + 
  geom_boxplot() + 
  labs(
    title = 'Distribution of Arrest Rates \nby Plurality Ethnicity',
    x = 'Predominant Race/ Ethnicity',
    y = 'Arrest Rates (logged)',
    color = 'Plurality Ethnicity'
  ) + 
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = 'none'
  ) +
  scale_color_nycc() + 
  scale_x_discrete(
    labels = c(
      'White', 'Asian', 'Black', 'Hispanic'
    )
  )

arrests_ct_merged |> 
  filter(yr == 2024 & pop >= 1000) |> 
  ggplot(aes(
    x = plurality_ethnicity,
    y = ct_hhi,
    color = plurality_ethnicity
  )) + 
  geom_boxplot() + 
  labs(
    title = 'Distribution of Diversity Index \nby Plurality Ethnicity',
    x = 'Predominant Race/ Ethnicity',
    y = 'Diversity Index',
    color = 'Plurality Ethnicity'
  ) + 
  theme_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = 'none'
  ) +
  scale_color_nycc() + 
  scale_x_discrete(
    labels = c(
      'White', 'Asian', 'Black', 'Hispanic'
    )
  )

arrests_geo |> 
  filter(pop >= 1000 & B19013_001E > 0) |> 
  ggplot() + 
  geom_sf(aes(fill = B19013_001E))

arrests_geo |> 
  filter(pop >= 1000 & gini > 0) |> 
  ggplot() + 
  geom_sf(aes(fill = gini))


# Temporal trends 

arrests_ct_merged |> 
  filter(pop > 1000 & B19013_001E > 0 & plurality_ethnicity %in% c('black_nh', 'white_nh', 'hispanic')) |> 
  group_by(plurality_ethnicity, yr) |> 
  summarise(
    mean_arrests = mean(total_arrests, na.rm = TRUE)
  ) |> 
  mutate(
    plurality_ethnicity = as.character(plurality_ethnicity),
    plurality_ethnicity = str_to_sentence(str_extract(plurality_ethnicity, '^[^_]+'))
  ) |> 
  ggplot(aes(
    x = yr,
    y = mean_arrests,
    color = plurality_ethnicity
  )) + 
  geom_point() + 
  geom_line() + 
  labs(
    title = 'Average Number of Arrests per CT,\nby Predominant Ethnicity, 2012 - 2024',
    x = '',
    y = 'Mean Number of Arrests per CT',
    color = 'Predominant\nEthnicity'
  ) + 
  theme_nycc() + 
  scale_color_nycc() + 
  theme(
    plot.title = element_text(hjust = 0.5)
  )

arrests_ct_merged |> 
  group_by(yr) |> 
  summarise(all_arrests = sum(total_arrests, na.rm = TRUE))

# regression to check if the post-pandemic period had differential effects for predominant ethnicity

arrests_ct_merged |> 
  mutate(
    post_covid = ifelse(yr >= 2020, 1, 0),
    trend = yr - 2020
  ) |> 
  filter(pop >= 1000 & B19013_001E > 0) %>%
  feols(
    log(total_arrests) ~ post_covid*trend + log(pop) + log(B19013_001E) + gini | true_boroct,
    cluster = 'true_boroct',
    data = .
  )

arrests_ct_merged |> 
  mutate(
    post_covid = ifelse(yr >= 2020, 1, 0),
    trend = yr - 2020
  ) |> 
  filter(pop >= 1000 & B19013_001E > 0) %>%
  feols(
    log(total_arrests) ~ post_covid*trend*plurality_ethnicity + log(pop) + log(B19013_001E) + gini | true_boroct,
    cluster = 'true_boroct',
    data = .
  ) |> summary()

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
  ) |> summary()

