# 01_speedcam

### Libraries ----------------------------------

library(tidyverse)
library(vroom)
library(jsonlite)
library(htmltools)
library(leaflet)
library(stringr)
library(sf)
library(tidycensus)
library(htmlwidgets)
library(tigris)
library(readxl)
library(mapview)
library(units)
library(councilverse)


#######################################################################################################################################
### Speed camera violations (rl) ----------------------------------

rl <- read_csv("data/rl.csv")

rl_merged_csv <- read_sf("data/rl_merged.csv") %>%
  select(-size, -id)

rl_merged <- rl_merged_csv %>%
  st_as_sf(coords = c("lon", "lat")) %>%
  st_set_crs(4326) %>%
  mutate(
    full_street = if_else(
      full_street == "REMSEN AVE @ LINDEN BLVD",
      "KINGS HWY @ REMSEN AVE",
      full_street
    )
  )

rl_unique <- rl_merged %>%
  mutate(issue_date = ymd_hms(issue_date)) %>%
  group_by(geometry) %>%
  summarise(
    add_count = n(),
    full_street = head(full_street, 1),
    first_appearance = min(issue_date),
    last_appearance = max(issue_date)
  ) %>%
  mutate(
    lon = st_coordinates(.)[, 1],
    lat = st_coordinates(.)[, 2]
  )

#######################################################################################################################################
### Merge with NTA shapefile ----------------------------------

# nta_url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nynta2020_22a.zip"
# x <- read_sf(unzip_sf(nta_url))

nta <- st_read(
  "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Neighborhood_Tabulation_Areas_2020/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson"
) %>%
  st_transform(4326) %>%
  {
    mutate(., area = st_area(.) %>% set_units(km^2) %>% drop_units())
  } %>%
  select(-Shape__Area)

cdta <- st_read("https://data.cityofnewyork.us/resource/xn3r-zk6y.geojson") %>%
  st_transform(4326) %>%
  {
    mutate(., area = st_area(.) %>% set_units(km^2) %>% drop_units())
  } %>%
  select(cdtaname, cdta2020, cdtatype, area, geometry)

#######################################################################################################################################
### Load crash data

### Crash data ----------------------------------

# https://data.cityofnewyork.us/Public-Safety/Motor-Vehicle-Collisions-Crashes/h9gi-nx95
url2 <- "https://data.cityofnewyork.us/resource/h9gi-nx95.json?$where=crash_date%20between%20'2025-01-01T00:00:00.000'%20and%20'2026-01-01T00:00:00.000'&$limit=9999999999999999"

crash_raw <- fromJSON(url2) %>%
  mutate(
    year = format(crash_date, format = "%Y"),
    month = format(crash_date, format = "%m")
  ) %>%
  filter(!is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326)


crash <- crash_raw %>%
  mutate(
    contributing_factor_vehicle_1 = replace_na(
      contributing_factor_vehicle_1,
      ""
    ),
    contributing_factor_vehicle_2 = replace_na(
      contributing_factor_vehicle_2,
      ""
    )
  ) %>%
  mutate(
    related_crash = contributing_factor_vehicle_1 ==
      "Traffic Control Disregarded" |
      contributing_factor_vehicle_2 == "Traffic Control Disregarded"
  )

crash_nta <- st_join(nta, crash) %>%
  st_drop_geometry() %>%
  group_by(NTA2020) %>%
  summarise(crash_count = n(), related_crashes = sum(related_crash)) %>%
  left_join(nta, by = "NTA2020") %>%
  st_as_sf()

crash_cdta <- st_join(cdta, crash) %>%
  st_drop_geometry() %>%
  group_by(cdta2020) %>%
  summarise(crash_count = n(), related_crashes = sum(related_crash)) %>%
  left_join(cdta, by = "cdta2020") %>%
  st_as_sf()

#######################################################################################################################################
### ACS data

# cross walk from census tract to NTA
# download.file("https://www1.nyc.gov/assets/planning/download/office/planning-level/nyc-population/census2020/nyc2020census_tract_nta_cdta_relationships.xlsx?r=092221", destfile = "data/nyc_ct_nta_crosswalk.xlsx")

cross_ct_nta <- fromJSON(
  "https://data.cityofnewyork.us/resource/hm78-6dwm.json?$limit=9999"
)


# acs_vars <- load_variables(2024, "acs5")

census_inc <- get_acs(
  geography = "tract",
  variables = c(
    "income" = "B19013_001",
    "pop" = "B03002_001",
    "pop_nhw" = "B03002_003",
    "pop_nhb" = "B03002_004",
    "pop_nha" = "B03002_006",
    "pop_his" = "B03002_012"
  ),
  year = 2024,
  state = "NY",
  county = c("New York", "Kings", "Queens", "Bronx", "Richmond")
) %>%
  select(-moe) %>%
  pivot_wider(names_from = "variable", values_from = "estimate") %>%
  mutate(pop_oth = pop - pop_nhw - pop_nhb - pop_nha - pop_his)


nta_inc <- cross_ct_nta %>%
  left_join(census_inc, by = c("geoid" = "GEOID")) %>%
  group_by(ntacode, ntaname) %>%
  # weighted average of nested census tracts
  summarise(
    med_inc = weighted.mean(income, pop, na.rm = TRUE),
    total_pop = sum(pop, na.rm = T),
    pop_nhw = sum(pop_nhw, na.rm = T),
    pop_nhb = sum(pop_nhb, na.rm = T),
    pop_nha = sum(pop_nha, na.rm = T),
    pop_his = sum(pop_his, na.rm = T),
    pop_oth = sum(pop_oth, na.rm = T)
  ) %>%
  mutate(
    perc_nhw = pop_nhw / total_pop,
    perc_nhb = pop_nhb / total_pop,
    perc_nha = pop_nha / total_pop,
    perc_his = pop_his / total_pop,
    perc_oth = pop_oth / total_pop
  ) %>%
  rename(nta2020 = ntacode)

cdta_inc <- cross_ct_nta %>%
  left_join(census_inc, by = c("geoid" = "GEOID")) %>%
  group_by(cdtacode, cdtaname) %>%
  # weighted average of nested census tracts
  summarise(
    med_inc = weighted.mean(income, pop, na.rm = TRUE),
    total_pop = sum(pop, na.rm = T),
    pop_nhw = sum(pop_nhw, na.rm = T),
    pop_nhb = sum(pop_nhb, na.rm = T),
    pop_nha = sum(pop_nha, na.rm = T),
    pop_his = sum(pop_his, na.rm = T),
    pop_oth = sum(pop_oth, na.rm = T)
  ) %>%
  mutate(
    perc_nhw = pop_nhw / total_pop,
    perc_nhb = pop_nhb / total_pop,
    perc_nha = pop_nha / total_pop,
    perc_his = pop_his / total_pop,
    perc_oth = pop_oth / total_pop
  ) %>%
  rename(cdta2020 = cdtacode)


# Demographic data from NYC planning

# # download.file("https://www1.nyc.gov/assets/planning/download/office/planning-level/nyc-population/census2020/nyc_decennialcensusdata_2010_2020_change.xlsx?r=1", destfile = "data/nyc_decennialcensusdata.xlsx")
# nta_nhw <- read_xlsx(
#   "data/nyc_decennialcensusdata.xlsx",
#   sheet = "2010, 2020, and Change",
#   skip = 3
# ) %>%
#   filter(GeoType == "nta2020") %>%
#   mutate(perc_nhw = WNH_20 / Pop_20) %>%
#   select(GeoID, Name, perc_nhw, Pop_20) %>%
#   rename(nta2020 = GeoID)

####################################################################
### Traffic Light Locations

lights <- st_read("data/lights.geojson")

# leaflet(lights) %>%
#   addTiles() %>%
#   addCircleMarkers()

clusters <- lights %>%
  st_transform(2263) %>%
  st_buffer(dist = 50) %>%
  st_union() %>%
  st_cast("POLYGON")


intersections_df <- st_centroid(clusters) %>%
  st_transform(4326) %>%
  st_as_sf()

# intersections_df %>%
#   leaflet() %>%
#   addTiles() %>%
#   addCircleMarkers()

nta_lights <- nta %>%
  st_join(intersections_df) %>%
  group_by(NTAName, area, geometry) %>%
  summarize(lights = n()) %>%
  mutate(light_dens = lights / area) %>%
  select(-area)

cdta_lights <- cdta %>%
  st_join(intersections_df) %>%
  group_by(cdtaname, area, geometry) %>%
  summarize(lights = n()) %>%
  mutate(light_dens = lights / area) %>%
  select(-area)


# nta_lights %>%
#   ggplot(aes(fill = lights)) +
#   geom_sf() +
#   scale_fill_distiller(palette = "RdYlBu")

# nta_lights %>%
#   ggplot(aes(fill = light_dens)) +
#   geom_sf() +
#   scale_fill_distiller(palette = "RdYlBu")

########################################################################
####   Final Assembly

df_nta <- rl_unique %>%
  {
    st_join(nta, .)
  } %>%
  group_by(NTA2020, NTAName) %>%
  summarise(
    vios_count = sum(add_count),
    num_cams = sum(!is.na(add_count), na.rm = T),
    med_vios = median(add_count)
  ) %>%
  st_drop_geometry() %>%
  left_join(crash_nta, by = c("NTA2020", "NTAName")) %>%
  mutate(
    vios_count = ifelse(is.na(vios_count), 0, vios_count),
    crash_count = ifelse(is.na(crash_count), 0, crash_count),
    vios_per_crash = vios_count / crash_count,
    cams_per_crash = num_cams / crash_count
  ) %>%
  st_as_sf() %>%
  left_join(nta_inc, by = c("NTA2020" = "nta2020")) %>%
  left_join(nta_lights %>% st_drop_geometry(), by = "NTAName") %>%
  filter(NTAType == 0) %>%
  select(
    -geometry,
    -NTA2020,
    -OBJECTID,
    -BoroCode,
    -CountyFIPS,
    -NTAAbbrev,
    -CDTA2020,
    -CDTAName,
    -Shape__Length,
    -ntaname
  ) %>%
  mutate(
    cams_per_light = num_cams / lights,
    cams_per_area = num_cams / area,
    crash_per_area = round(crash_count / area, digits = 1),
    rel_crash_per_area = round(related_crashes / area, digits = 1),
    cams_per_pop = num_cams / total_pop,
    crash_per_pop = crash_count / total_pop * 1000,
    rel_crash_per_pop = related_crashes / total_pop * 1000
  )

df_cdta <- rl_unique %>%
  {
    st_join(cdta, .)
  } %>%
  group_by(cdta2020, cdtaname) %>%
  summarise(
    vios_count = sum(add_count),
    num_cams = sum(!is.na(add_count), na.rm = T),
    med_vios = median(add_count)
  ) %>%
  st_drop_geometry() %>%
  left_join(crash_cdta, by = c("cdta2020", "cdtaname")) %>%
  mutate(
    vios_count = ifelse(is.na(vios_count), 0, vios_count),
    crash_count = ifelse(is.na(crash_count), 0, crash_count),
    vios_per_crash = vios_count / crash_count,
    cams_per_crash = num_cams / crash_count
  ) %>%
  st_as_sf() %>%
  left_join(cdta_inc, by = c("cdta2020", "cdtaname")) %>%
  left_join(cdta_lights %>% st_drop_geometry(), by = "cdtaname") %>%
  filter(cdtatype == "0") %>%
  mutate(
    cams_per_light = num_cams / lights,
    cams_per_area = num_cams / area,
    crash_per_area = crash_count / area,
    rel_crash_per_area = related_crashes / area,
    cams_per_pop = num_cams / total_pop,
    crash_per_pop = crash_count / total_pop * 1000,
    rel_crash_per_pop = related_crashes / total_pop * 1000
  )

st_write(df_nta, "data/df_nta.gpkg", delete_dsn = T)
st_write(df_cdta, "data/df_cdta.gpkg", delete_dsn = T)
st_write(rl_unique, "data/rl_unique.gpkg", delete_dsn = T)
