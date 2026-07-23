
library(tidyverse)
library(tidycensus)
library(sf)
library(jsonlite)
library(readxl)

################################################################################
# read food insecurity data
################################################################################

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
  year      = 2022,
  survey    = "acs5",
  output    = "wide"
)

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


# join to nta
nta_sf <- st_read("https://data.cityofnewyork.us/resource/9nt8-h7nd.geojson") %>% 
  st_transform(crs = 4326) 

livexyz_nta <- st_join(livexyz, nta_sf, join = st_intersects) %>%
  st_drop_geometry() %>% 
  group_by(nta2020, category) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(names_from = category, values_from = count, values_fill = 0) %>% 
  mutate(total = Bodega + `Fast Food` + Grocery + Vices,
         bodega_perc = Bodega/total, 
         fastfood_perc = `Fast Food`/total, 
         grocery_perc = Grocery/total, 
         vices_perc = Vices/total)



################################################################################
# aggregate to nta level + combine
################################################################################

# crosswalk at: https://www.nyc.gov/content/planning/pages/resources/datasets/neighborhood-tabulation
nta_crosswalk <- read_excel("data/nyc2020census_tract_nta_cdta_relationships.xlsx") %>%
  select(GEOID, NTACode)

# from the food security repo
cfc_locations = readRDS(file.path("..", "..", "..", "food_insecurity", "data", "output", "nta_data.RDS")) %>% 
  select(nta2020, cfc_count, cfc_per100k, cfc_per10k_fi) %>% 
  st_drop_geometry()

# from the NYC food pantry map, https://finder.nyc.gov/foodhelp/locations
food_pantries <- st_read("data/food_pantries.geojson") 
food_pantries_cfc <- readRDS("~/Documents/GitHub/food_insecurity/data/output/cfc_geocoded.RDS")

food_pantries_cfc = st_join(food_pantries_cfc, nta_sf, join = st_intersects) %>% 
  st_drop_geometry() %>% 
  group_by(nta2020) %>%
  summarise(cfc_pantries = n(), .groups = "drop")

food_pantries = st_join(food_pantries, nta_sf, join = st_intersects) %>% 
  st_drop_geometry() %>% 
  group_by(nta2020) %>%
  summarise(all_pantries = n(), .groups = "drop") %>% 
  merge(food_pantries_cfc, by="nta2020", all=T)

combined <- nyc_tract_demographics_raw %>%
  inner_join(nta_crosswalk, by = "GEOID") %>% 
  ungroup() %>%
  group_by(NTACode) %>% 
  summarise(median_incomeE = weighted.mean(median_incomeE, total_popE), 
            total_popE = sum(total_popE, na.rm=T), 
            white_nhE = sum(white_nhE, na.rm=T), 
            black_nhE = sum(black_nhE, na.rm=T), 
            asian_nhE = sum(asian_nhE, na.rm=T), 
            hispanicE = sum(hispanicE, na.rm=T), 
            perc_white = white_nhE/total_popE*100, 
            perc_black = black_nhE/total_popE*100, 
            perc_asian = asian_nhE/total_popE*100, 
            perc_hisp = hispanicE/total_popE*100, 
            perc_pov = sum(poverty_belowE, na.rm=T)/sum(poverty_totalE, na.rm=T)*100)  %>% 
  merge(cfc_locations, by.x="NTACode", by.y="nta2020", all=T) %>% 
  merge(food_insecurity_nta, by.x="NTACode", by.y="nta", all=T) %>% 
  merge(livexyz_nta, by.x="NTACode", by.y="nta2020", all=T) %>% 
  mutate(supply_gap_pp = supply_gap_lbs/total_popE, 
         grocery_10k = Grocery/total_popE*10000, 
         bodega_10k = Bodega/total_popE*10000, 
         vices_10k = Vices/total_popE*10000) %>% 
  filter(total_popE > 0 & !is.na(total_popE), 
         !is.na(food_insecure_percentage))


################################################################################
# run simple lm
################################################################################

# how much is the city providing (+ is GOOD, higher cfc per food insecure person)
summary(lm(cfc_per10k_fi ~ perc_black + perc_hisp + food_insecure_percentage + 
             vulnerable_population + grocery_10k + bodega_10k, 
           data = combined))

# how much need is there right now (+ is GOOD, smaller supply gap)
summary(lm(-supply_gap_pp ~ perc_black + perc_hisp + food_insecure_percentage + 
             vulnerable_population + grocery_10k + bodega_10k, 
           data = combined))


################################################################################
# run intersectional lm
################################################################################

# how much is the city providing (+ is GOOD, higher cfc per food insecure person)
summary(lm(cfc_per10k_fi ~ perc_black*food_insecure_percentage + perc_hisp*food_insecure_percentage + grocery_10k + bodega_10k, 
           data = combined))

# how much need is there right now (+ is GOOD, lower supply gap)
summary(lm(-supply_gap_pp ~ perc_black*food_insecure_percentage + perc_hisp*food_insecure_percentage + grocery_10k + bodega_10k, 
           data = combined))
