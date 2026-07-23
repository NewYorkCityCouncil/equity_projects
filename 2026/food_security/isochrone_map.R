
library(tidyverse)
library(tidycensus)
library(sf)
library(jsonlite)
library(leaflet)
#library(readxl)
library(mapboxapi)
library(councildown)
source(file.path("..", "..", "..", "tokens.R"))



################################################################################
# settings
################################################################################

# set mapbox token
mb_access_token(mapbox_token, install = T, overwrite = T)
readRenviron("~/.Renviron")


################################################################################
# functions 
################################################################################

simplify_points = function(points, cluster_dist = 50) {
  # get distances and cluster
  dist_matrix <- st_distance(points %>% st_transform(crs = 2263)) # use 2263 for feet
  hc <- hclust(dist(dist_matrix), method = "complete")
  clusters <- cutree(hc, h = cluster_dist)
  
  # Add cluster IDs back to original data
  points$cluster_id <- clusters
  points = points %>%
    group_by(cluster_id) %>%
    summarize(geometry = st_union(geometry) %>% st_centroid()) 
}

get_isochrone_list = function(data, t = 15, d) {
  
  if (missing(d)) {
    temp = mb_isochrone(location = as.vector(st_coordinates(data$geometry)),
                        profile = "walking",
                        time = t)
  } else{
    temp = mb_isochrone(location = as.vector(st_coordinates(data$geometry)),
                        profile = "walking",
                        distance = d)
  }
  Sys.sleep(0.25) # to force us not to go over the API limit
  return(temp)
}

get_isochrone = function(data, t = 15, d, path) {
  if (missing(d)) {
    temp = apply(data %>% st_transform(4326), 1, FUN = get_isochrone_list, t = t)
  } else{
    temp = apply(data %>% st_transform(4326), 1, FUN = get_isochrone_list, d = d)
  }
  temp = bind_rows(temp)
  saveRDS(temp, file.path("data", "output", path))
  return(temp)
}


################################################################################
# read cfc data
################################################################################

food_pantries_cfc <- readRDS("~/Documents/GitHub/food_insecurity/data/output/cfc_geocoded.RDS")


# ~3 minutes to run each
isochrone_5min = get_isochrone(food_pantries_cfc %>% st_transform(4326), d = 402) # 1/4 mile in meters (5 min)
isochrone_10min = get_isochrone(food_pantries_cfc %>% st_transform(4326), d = 804) # 1/2 mile in meters (10 min)


################################################################################
# read liveXYZ data
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

grocery = livexyz %>% filter(category == "Grocery")
bodega = livexyz %>% filter(category == "Bodega")
vices = livexyz %>% filter(category == "Vices")
fast_food = livexyz %>% filter(category == "Fast Food")

# ~11 minutes to run, each 
grocery_10min = get_isochrone(grocery %>% st_transform(4326), d = 804) # 1/2 mile in meters (10 min)
grocery_5min = get_isochrone(grocery %>% st_transform(4326), d = 402) # 1/4 mile in meters (5 min)

# ~20 minutes to run
bodega_2min = get_isochrone(bodega %>% st_transform(4326), d = 201) # 1/8 mile in meters (2.5 min)


################################################################################
# plot
################################################################################

boroughs = st_read("https://data.cityofnewyork.us/api/v3/views/gthc-hcne/query.geojson")
parks = st_read("https://data.cityofnewyork.us/api/v3/views/enfh-gkve/query.geojson")

d = grocery_5min %>% st_union()
diff = boroughs %>% st_difference(d) %>% st_difference(parks)

leaflet() %>%
  addCouncilStyle(add_dists = F) %>%
  addProviderTiles("CartoDB")  %>%
  addPolygons(data = d %>% st_cast("MULTIPOLYGON") %>% st_cast("POLYGON"), 
              fillOpacity = 0.2, fillColor="darkgreen", opacity = 0, smoothFactor = 0) %>%
  addPolygons(data = diff %>% st_cast("MULTIPOLYGON")%>% st_cast("POLYGON"),
              col = 'darkorange', opacity = 0, fillOpacity = 0.7)
              
