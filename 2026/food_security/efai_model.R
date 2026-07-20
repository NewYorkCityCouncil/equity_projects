source("../../../tokens.R")
library(tidyverse)
library(tidycensus)
library(sf)
library(jsonlite)
library(readxl)
library(corrplot)
library(leaps)
library(broom)
library(purrr)
census_api_key(census_key)

################################################################################
# read efai + food insecurity data
################################################################################

efai = read_csv("data/df_tt_EFAI.csv")

food_insecurity_url <- "https://data.cityofnewyork.us/resource/4kc9-zrs2.json"
food_insecurity_raw <- fromJSON(food_insecurity_url)

# clean
food_insecurity_nta <- food_insecurity_raw %>%
  filter(year == 2025) %>% 
  mutate(nta = toupper(as.character(nta)), 
         food_insecure_percentage = as.numeric(food_insecure_percentage), 
         supply_gap_lbs = as.numeric(supply_gap_lbs), 
         unemployment_rate = as.numeric(unemployment_rate), 
         vulnerable_population = as.numeric(vulnerable_population)) %>%
  select(nta, food_insecure_percentage, supply_gap_lbs, unemployment_rate, vulnerable_population) %>%
  drop_na(nta) %>% 
  group_by()


################################################################################
# read census data
################################################################################

nyc_counties <- c("Bronx", "Kings", "New York", "Queens", "Richmond")
census_vars <- c(total_pop     = "B03002_001", white_nh      = "B03002_003", 
                 black_nh      = "B03002_004", asian_nh      = "B03002_006",
                 hispanic      = "B03002_012", median_income = "B19013_001",
                 poverty_total = "B17001_001", poverty_below = "B17001_002")

nyc_tract_demographics_raw <- get_acs(
  geography = "tract",
  variables = census_vars,
  state     = "NY",
  county    = nyc_counties,
  year      = 2024,
  survey    = "acs5",
  output    = "wide",
  geometry = TRUE
)

tract_spatial = nyc_tract_demographics_raw %>% select(GEOID)
nyc_tract_demographics_raw = nyc_tract_demographics_raw %>% 
  st_drop_geometry() %>%
  mutate(perc_white = white_nhE/total_popE*100, 
         perc_black = black_nhE/total_popE*100, 
         perc_asian = asian_nhE/total_popE*100, 
         perc_hisp = hispanicE/total_popE*100, 
         perc_pov = poverty_belowE, na.rm=T/poverty_totalE, na.rm=T*100)


################################################################################
# read live xyz data
################################################################################

# bring in Live XYZ data
livexyz <- read_csv("data/LiveXYZ_June242026.csv")
livexyz = livexyz %>% 
  mutate(category = case_when(grepl("Grocery|Supermarket", tagsPrimary.name) ~ "Grocery", 
                              grepl("Deli|Convenience|Gas Station", tagsPrimary.name) ~ "Bodega", 
                              grepl("Smoke|Liquor|Cannabis|Spirits", tagsPrimary.name) ~ "Vices", 
                              grepl( "Burger|Fried Chicken|Fast Food|Hot Dog|Wings|Donut|Candy", tagsPrimary.name) ~ "Fast Food", 
                              T ~ ""))  %>% 
  filter(category != "", 
         !grepl("Delivery", tagsPrimary.name)) %>% 
  select(stateId, resolvedName, address, tagsPrimary.name, category, entrances.main.lat, entrances.main.lon) %>% 
  st_as_sf(coords = c("entrances.main.lon", "entrances.main.lat"), crs = 4326)

tract_buffer = tract_spatial %>% st_buffer(1320) %>% st_transform(4326) # 1/4 mile

# aggregate at tract level 
livexyz_tract <- st_join(livexyz, tract_buffer, join = st_intersects) %>%
  st_drop_geometry() %>% 
  group_by(GEOID, category) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = category, values_from = count, values_fill = 0) %>% 
  mutate(total = Bodega + `Fast Food` + Grocery + Vices,
         bodega_perc = Bodega/total, 
         fastfood_perc = `Fast Food`/total, 
         grocery_perc = Grocery/total, 
         vices_perc = Vices/total)


################################################################################
# combine
################################################################################


cw = read_csv("https://data.cityofnewyork.us/resource/hm78-6dwm.csv?$limit=9999") %>% 
  select(geoid, ntacode)

combined = efai %>% dplyr::select(GEOID, index) %>% 
  merge(cw, by.x="GEOID", by.y="geoid", all=T) %>% 
  merge(food_insecurity_nta, by.x = "ntacode", by.y = "nta", all=T) %>% 
  merge(nyc_tract_demographics_raw, by="GEOID", all=T) %>% 
  merge(livexyz_tract, by="GEOID", all=T) %>% 
  mutate(grocery_10k = Grocery/total_popE*10000, 
         vices_10k = Vices/total_popE*10000, 
         bodega_10k = Bodega/total_popE*10000, 
         fastfood_10k = `Fast Food`/total_popE*10000, 
         supply_gap_lbs_natural = -supply_gap_lbs, 
         supply_gap_lbs_10k = -supply_gap_lbs/10000, 
         majority_race = case_when(perc_white > perc_black & perc_white > perc_asian ~ "white", 
                                   perc_black > perc_white & perc_black > perc_asian ~ "black", 
                                   perc_asian > perc_white & perc_asian > perc_black ~ "asian"), 
         majority_eth = case_when(perc_white > perc_black & perc_white > perc_asian & perc_white > perc_hisp ~ "white", 
                                   perc_black > perc_white & perc_black > perc_asian & perc_black > perc_hisp ~ "black", 
                                   perc_asian > perc_white & perc_asian > perc_black & perc_asian > perc_hisp  ~ "asian", 
                                   perc_hisp > perc_white & perc_hisp > perc_black & perc_hisp > perc_asian  ~ "hisp"), 
         income_quantile = ecdf(median_incomeE)(median_incomeE),
         income_tercile = factor(ntile(median_incomeE, 3), labels = c("low", "mid", "high")),
         majority_eth = relevel(factor(majority_eth), ref = "white"), 
         majority_race = relevel(factor(majority_race), ref = "white"), 
         pop_1k = total_popE/1000) %>% 
  filter(total_popE > 0)


################################################################################
# model
################################################################################

corrplot(cor(combined %>% 
               select(total_popE, perc_white, perc_asian, perc_black, perc_hisp, 
                      food_insecure_percentage, unemployment_rate, supply_gap_lbs_natural,
                      grocery_10k, bodega_10k, vices_10k, fastfood_10k)), 
         method = 'number')

# how much access to emergency food is there (+ = more access)
summary(lm(index ~ total_popE + perc_black + perc_hisp + 
             food_insecure_percentage + supply_gap_lbs_natural, 
           data = combined))

# does the area majority impact the food insecurity index? (+ = more access)
summary(lm(index ~ total_popE + majority_eth + food_insecure_percentage + supply_gap_lbs_natural, 
           data = combined))

# do different majority areas have different responses to food insecurity?
summary(lm(index ~ total_popE + majority_eth*food_insecure_percentage + supply_gap_lbs_natural, 
           data = combined))


# does the area majority change the racial response?
summary(lm(index ~ total_popE + majority_eth*(perc_black + perc_hisp) + 
             food_insecure_percentage + supply_gap_lbs_natural, 
           data = combined))


# test finding best model ------------------------------------------------------
models = regsubsets(index ~ total_popE + median_incomeE + perc_pov + perc_white + 
                perc_black + perc_asian + perc_hisp + food_insecure_percentage + 
                supply_gap_lbs_natural + unemployment_rate + vulnerable_population + 
                grocery_10k + bodega_10k + vices_10k + fastfood_10k, 
               data = combined, nbest = 1, nvmax = 15, method="exhaustive")
summary(models)


################################################################################
# plot models
################################################################################

# run models
formulas = c("index ~ pop_1k + perc_black + perc_hisp + perc_asian + food_insecure_percentage + supply_gap_lbs_10k",
             "index ~ pop_1k + perc_black + perc_hisp + food_insecure_percentage + supply_gap_lbs_10k",
             "index ~ pop_1k + majority_eth + food_insecure_percentage + supply_gap_lbs_10k",
             "index ~ pop_1k + majority_eth*food_insecure_percentage + supply_gap_lbs_10k",
             "index ~ pop_1k + majority_eth*income_tercile + supply_gap_lbs_10k",
             "index ~ pop_1k + majority_eth*supply_gap_lbs_10k")
models = lapply(formulas, function(f) lm(as.formula(f), data = combined))
names(models) = formulas  # use the formula string as the model label

# get coefs
coef_df = imap_dfr(models, function(mod, mod_name) {
  tidy(mod, conf.int = TRUE) %>%
    mutate(model = mod_name)
}) %>% mutate(term = fct_relevel(term, "(Intercept)", "pop_1k", 
                                 "food_insecure_percentage", "supply_gap_lbs_10k", 
                                 "income_tercilemid", "income_tercilehigh",
                                 "perc_black", "majority_ethblack", "majority_ethblack:income_tercilemid", 
                                    "majority_ethblack:income_tercilehigh","majority_ethblack:food_insecure_percentage",
                                    "majority_ethblack:supply_gap_lbs_10k", 
                                 "perc_hisp", "majority_ethhisp", "majority_ethhisp:income_tercilemid", 
                                    "majority_ethhisp:income_tercilehigh", "majority_ethhisp:food_insecure_percentage", 
                                    "majority_ethhisp:supply_gap_lbs_10k", 
                                 "perc_asian", "majority_ethasian", "majority_ethasian:income_tercilemid", 
                                    "majority_ethasian:income_tercilehigh", "majority_ethasian:food_insecure_percentage", 
                                    "majority_ethasian:supply_gap_lbs_10k"))

#coef_df <- coef_df %>% filter(term != "(Intercept)")

# plot
dodge_width <- 0.6
ggplot(coef_df, aes(x = fct_rev(term), y = estimate, color = model, group = model, alpha = `p.value` < 0.05)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = dodge_width), width = 0.15) +
  geom_point(position = position_dodge(width = dodge_width), size = 2.5) +
  coord_flip() +
  labs(x = NULL, y = "Coefficient estimate", color = "Model",
       title = "Model coefficients with 95% CIs") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
  scale_alpha_manual(values = c(`TRUE` = 1, `FALSE` = 0.35))

