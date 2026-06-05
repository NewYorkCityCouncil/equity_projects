library(tidyverse)
library(councilverse)
library(janitor)
library(readxl)
library(vroom)
library(sf)
library(leaflet)
library(zoo)

# ==== download + clean data ====
# ---- community district spatial boundaries & acs  estimates via councildown (2022) 
# councilcount::get_ACS_variables(acs_year = 2022) %>% View()
community_districts <- councilcount::get_geo_estimates(acs_year = 2022, 
                                                       geo = "communitydist", 
                                                       var_codes = c("DP02_0088E", 
                                                                     "DP05_0019E",
                                                                     "DP02_0094E",
                                                                     "DP05_0073E",
                                                                     "DP05_0079E",
                                                                     "DP05_0080E",
                                                                     "DP05_0082E", 
                                                                     "DP02_0001E",
                                                                     "DP03_0074E")) %>%
  select(communitydist, DP02_0088E, DP05_0019E, DP02_0094E, DP05_0073E, DP05_0079E, DP05_0080E, DP05_0082E, DP02_0001E, DP03_0074E) %>%
  rename("total_pop" = "DP02_0088E",
         "n_under_18" = "DP05_0019E",
         "n_foreign_born" = "DP02_0094E",
         "n_hisp_latino" = "DP05_0073E",
         "n_white_alone" = "DP05_0079E",
         "n_black_alone" = "DP05_0080E",
         "n_asian_alone" = "DP05_0082E",
         "total_households" = "DP02_0001E",
         "n_households_snap" = "DP03_0074E") %>%
  mutate(pct_under_18 = n_under_18/total_pop,
         pct_foreign_born = round((n_foreign_born/total_pop)*100,2),
         pct_hisp_latino = round((n_hisp_latino/total_pop)*100,2),
         pct_white_alone = round((n_white_alone/total_pop)*100,2),
         pct_black_alone = round((n_black_alone/total_pop)*100,2),
         pct_asian_alone = round((n_asian_alone/total_pop)*100,2),
         pct_households_snap = round((n_households_snap/total_households)*100,2)) # consider filtering out cds with low pop

# ---- environmental health & data portal (2024): fine particles (pm 2.5)
ehdp <- read.csv("data/NYC EH Data Portal - Fine particles (PM 2.5) (full table).csv") %>%
  filter(TimePeriod == 2024, GeoType == "CD") %>%
  clean_names() %>%
  select(-c(time_period, geo_type, geo_rank))

# ---- new york city community health profiles (2022): demographics + health outcomes
chp <- read_excel("data/2022-chp-pud (1).xlsx", sheet = "CHP_all_data", skip = 1) %>%
  filter(Borough %in% c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")) %>%
  clean_names() %>%
  # rename("dohmh_pop" = "overall_pop") %>%
  select(id,
         child_asthma, # 2018 /updated 2022, rate of er visits per 10k children
         avoidable_child_hosp) # 2000-2016 / updated 2017

# ---- total air quality complaints to 311 (2022-2024)
requests_311 <- vroom("https://data.cityofnewyork.us/resource/erm2-nwe9.csv?agency='DEP'&$where=created_date>'2022-01-01'&$limit=999999999999") 
# **to do** : filter to 2024 end year
requests_311 <- requests_311 %>%
  mutate(created_date = as.Date(created_date),
         date = as.yearmon(paste(year(created_date), month(created_date)), "%Y%m")) %>% 
  filter(date < "Jan 2025",
         complaint_type == "Air Quality", 
         !is.na(latitude), 
         !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(2263)
community_districts$n_311 <- lengths(
  st_intersects(community_districts, requests_311)
)

# ==== merge to final df ====

community_districts <- community_districts %>%
  left_join(ehdp, by = c("communitydist" = "geo_id")) %>%
  left_join(chp %>% mutate(id = as.integer(id)), by = c("communitydist" = "id")) %>%
  mutate(n_311_per10k_res = round(n_311 / (total_pop/10000),2))
community_districts[is.nan(community_districts)] <- NA
community_districts[is.infinite(community_districts)] <- NA
write_sf(community_districts, "data/district_data.geojson")

# ==== ignore below ====
# ---- hospital inpatient discharges (sparcs de-identified) (2024): childhood asthma hospitalizations
# sparcs <- vroom::vroom("https://health.data.ny.gov/resource/sf4k-39ay.csv?ccsr_diagnosis_code='RSP009'&$limit=999999999999") %>% 
#   distinct()
# # --- filter to nyc only hospitalizations, child patients
# sparcs <- sparcs %>%
#   filter(health_service_area == "New York City",
#          age_group == "0-17") # note that the chp does 5-17; but they have the full (identifiable) ds

# # ---- new york city community health profiles (2022): demographics + health outcomes
# chp <- read_excel("data/2022-chp-pud (1).xlsx", sheet = "CHP_all_data", skip = 1) %>%
#   filter(Borough %in% c("Manhattan", "Bronx", "Brooklyn", "Queens", "Staten Island")) %>%
#   clean_names() %>%
#   rename("dohmh_pop" = "overall_pop") %>%
#   select(dohmh_pop,
#          race_white, race_black, race_asian, race_latino, race_other,
#          age0to17, age18to24, age25to44, age45to64, age65plus,
#          born_outside_us,
#          poverty,
#          rent_burden,
#          child_asthma, avoidable_child_hosp) 
