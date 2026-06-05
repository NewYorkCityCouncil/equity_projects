library(tidyverse)
library(councilverse)
library(janitor)
library(readxl)
library(vroom)
library(sf)
library(leaflet)
library(zoo)

community_districts <- st_read('data/district_data.geojson') %>%
  mutate(pct_under_18 = round((pct_under_18)*100,2))

# ---- project to appropriate crs for visualization 
cd_4326 <- community_districts %>% st_transform(4326)

# ---- set up colors 
# demo_pal = leaflet::colorBin(
#   palette = pal_nycc(palette = "warm"),
#   domain = c(0,100),
#   bins = 5,
#   na.color = "lightgrey",
#   # reverse = TRUE
# )
# pm_pal = leaflet::colorBin(
#   palette = pal_nycc(palette = "cool"),
#   domain = cd_4326$annual_mean_mcg_m3,
#   bins = 5,
#   na.color = "lightgrey",
#   # reverse = TRUE
# )
ca_pal = leaflet::colorBin(
  palette = pal_nycc(palette = "cool"),
  domain = cd_4326$child_asthma,
  bins = 5,
  na.color = "lightgrey",
  # reverse = TRUE
)

# ---- plot map
leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  # addCouncilStyle(add_dists = FALSE) %>%
  # addPolygons(data = cd_4326,
  #             weight = 1.2, 
  #             opacity = 1,
  #             fillOpacity = .65, 
  #             color = ~pm_pal(annual_mean_mcg_m3),
  #             popup = paste0("<strong>", cd_4326$geography, "</strong><br>", 
  #                            "PM 2.5 annual average (mcg/m3): ", cd_4326$annual_mean_mcg_m3),
  #             group = "Fine particles (PM 2.5)") %>%
  addPolygons(data = cd_4326,
              weight = 1.6, 
              opacity = 1,
              fillOpacity = .85, 
              color = ~ca_pal(child_asthma),
              popup = paste0("<strong>", cd_4326$geography, "</strong><br>", 
                             "Emergency department visits per 10k children: ", cd_4326$child_asthma),
              group = "Child asthma") %>%
  # add comm dist outline 
  addPolygons(data = cd_4326,
              weight = 1.6, 
              opacity = .15,
              fillOpacity = 0, 
              color = "black") %>%
  # addPolygons(data = cd_4326,
  #             weight = 1.2, 
  #             opacity = 1,
  #             fillOpacity = .65, 
  #             color = ~demo_pal(pct_under_18),
  #             popup = paste0("<strong>", cd_4326$geography, "</strong><br>", 
  #                            "Percent under 18: ", cd_4326$pct_under_18, "%"),
  #             group = "Under 18") %>%
  # addPolygons(data = cd_4326,
  #             weight = 1.2, 
  #             opacity = 1,
  #             fillOpacity = .65, 
  #             color = ~demo_pal(pct_foreign_born),
  #             popup = paste0("<strong>", cd_4326$geography, "</strong><br>", 
  #                            "Percent foreign born: ", cd_4326$pct_foreign_born, "%"),
  #             group = "Foreign born") %>%
  # addPolygons(data = cd_4326,
  #             weight = 1.2, 
  #             opacity = 1,
  #             fillOpacity = .65, 
  #             color = ~demo_pal(pct_hisp_latino),
  #             popup = paste0("<strong>", cd_4326$geography, "</strong><br>", 
  #                            "Percent Hispanic/Latino: ", cd_4326$pct_hisp_latino, "%"),
  #             group = "Hispanic/Latino") %>%
  # addPolygons(data = cd_4326,
  #             weight = 1.2, 
  #             opacity = 1,
  #             fillOpacity = .65, 
  #             color = ~demo_pal(pct_white_alone),
  #             popup = paste0("<strong>", cd_4326$geography, "</strong><br>", 
  #                            "Percent White alone: ", cd_4326$pct_white_alone, "%"),
  #             group = "White alone") %>%
  # addPolygons(data = cd_4326,
  #             weight = 1.2, 
  #             opacity = 1,
  #             fillOpacity = .65, 
  #             color = ~demo_pal(pct_black_alone),
  #             popup = paste0("<strong>", cd_4326$geography, "</strong><br>", 
  #                            "Percent Black alone: ", cd_4326$pct_black_alone, "%"),
  #             group = "Black alone") %>%
  # addPolygons(data = cd_4326,
  #             weight = 1.2, 
  #             opacity = 1,
  #             fillOpacity = .65, 
  #             color = ~demo_pal(pct_asian_alone),
  #             popup = paste0("<strong>", cd_4326$geography, "</strong><br>", 
  #                            "Percent Asian alone: ", cd_4326$pct_asian_alone, "%"),
  #             group = "Asian alone") %>%
  # addPolygons(data = cd_4326,
  #             weight = 1.2, 
  #             opacity = 1,
  #             fillOpacity = .65, 
  #             color = ~demo_pal(pct_households_snap),
  #             popup = paste0("<strong>", cd_4326$geography, "</strong><br>", 
  #                            "Percent of households receiving SNAP: ", cd_4326$pct_households_snap, "%"),
  #             group = "SNAP households") %>%
  # addLayersControl(
  #   baseGroups = c("Fine particles (PM 2.5)", "Child asthma", "Under 18", "Foreign born", "Hispanic/Latino", "White alone", "Black alone", "Asian alone", "SNAP households"),
  #   options = layersControlOptions(collapsed = FALSE)
  # ) %>%
  # addLegend(
  #   pal = demo_pal, 
  #   values = c(0,100), 
  #   title = "Demographic scale (%)",
  #   position = "bottomright"
  # ) %>%
  # addLegend(
  #   pal = pm_pal, 
  #   values = c(0,100), 
  #   title = "Fine particles (PM 2.5) scale",
  #   position = "topleft"
  # ) %>%
  addLegend(
    pal = ca_pal, 
    opacity = 1,
    values = c(0,100), 
    title = "Child asthma (rate)",
    position = "topleft"
  )
