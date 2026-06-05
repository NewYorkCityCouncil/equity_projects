library(tidyverse)
library(councilverse)
library(vroom)
library(sf)
library(leaflet)

# ==== air quality: equity analysis ====

# ---- download all air quality service requests since 2022
requests_311 <- vroom("https://data.cityofnewyork.us/resource/erm2-nwe9.csv?agency='DEP'&$where=created_date>'2022-01-01'&$limit=999999999999") 
requests_311 <- requests_311 %>%
  filter(complaint_type == "Air Quality", 
         !is.na(latitude), 
         !is.na(longitude)) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(2263)

# ---- download council districts, community districts & estimate population
# council_districts <- councildown::nycc_cd_23 %>%
#   select(coun_dist, geometry) %>%
#   st_transform(2263)
# comm_dist <- st_read("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Community_Districts/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson")
# comm_dist <- comm_dist %>% st_transform(2263)

# DP02_0088E = Total Population
council_districts <- councilcount::get_geo_estimates(acs_year = 2022, geo = "councildist", var_codes = "DP02_0088E", boundary_year = 2023) %>%
  # st_drop_geometry() %>%
  select(councildist, DP02_0088E) %>%
  rename("council_pop" = "DP02_0088E") %>%
  st_transform(2263)
# also looking at community district because same geo as nyc community health survey
community_districts <- councilcount::get_geo_estimates(acs_year = 2022, geo = "communitydist", var_codes = "DP02_0088E", boundary_year = 2023) %>%
  # st_drop_geometry() %>%
  select(communitydist, DP02_0088E) %>%
  rename("comm_pop" = "DP02_0088E") %>% 
  st_transform(2263)

# ---- summarize air quality complaints by council districts, community districts
council_districts$n_complaints <- lengths(
  st_intersects(council_districts, requests_311)
)
council_districts <- council_districts %>%
  mutate(nc_per10k_residents = n_complaints / (council_pop/10000))

community_districts$n_complaints <- lengths(
  st_intersects(community_districts, requests_311)
)
community_districts <- community_districts %>%
  mutate(nc_per10k_residents = n_complaints / (comm_pop/10000))
community_districts <- community_districts %>%
  mutate(nc_per10k_residents = na_if(nc_per10k_residents, 0),
         nc_per10k_residents = na_if(nc_per10k_residents, Inf))
# coun_pal = leaflet::colorBin(
#   palette = pal_nycc(palette = "cool"),
#   domain = council_districts$nc_per10k_residents,
#   bins = 5,
#   na.color = "grey"
# )
comm_pal = leaflet::colorBin(
  palette = pal_nycc(palette = "cool"),
  domain = council_districts$nc_per10k_residents,
  bins = 5,
  na.color = "lightgrey"
)

# ---- visualize air quality complaints
# council_districts <- council_districts %>% st_transform(4326)
community_districts <- community_districts %>% st_transform(4326)
# 
# leaflet() %>%
#   addProviderTiles("CartoDB.PositronNoLabels") %>%
#   addCouncilStyle(add_dists = TRUE, dist_year = "2023") %>%
#   addPolygons(data = council_districts,
#               weight = 1.2, 
#               opacity = 1,
#               fillOpacity = .875, 
#               color = ~coun_pal(nc_per10k_residents))
leaflet() %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  # addCouncilStyle(add_dists = TRUE, dist_year = "2023") %>%
  addPolygons(data = community_districts,
              weight = 1.2, 
              opacity = 1,
              fillOpacity = .875, 
              color = ~comm_pal(nc_per10k_residents))
