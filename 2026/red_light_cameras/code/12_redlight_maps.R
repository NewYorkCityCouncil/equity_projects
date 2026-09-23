# maps

# Libraries
library(tidyverse)
library(vroom)
library(jsonlite)
library(htmltools)
library(leaflet)
library(leaflet.extras)
library(stringr)
library(sf)
library(tidycensus)
library(htmlwidgets)
library(tigris)
library(readxl)
library(mapview)
library(units)
library(councilverse)

# load data
df_nta <- st_read("data/df_nta.gpkg")
rl_unique <- st_read("data/rl_unique.gpkg")
lights <- st_read("data/lights.geojson")
df_cdta <- st_read("data/df_cdta.gpkg")

# source("code/11_redlight.R")

options(scipen = 9999999)


######################
## Camera Locations ##
######################

map_export_camera_locs <- leaflet(rl_unique) %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    popup = lapply(
      paste(
        "<p>",
        "Address: ",
        rl_unique$full_street,
        "</p>",
        "<p>",
        paste0("Tickets Issued: ", rl_unique$add_count),
        "</p>"
      ),
      HTML
    ),
    radius = 0.1
  )

map_export_camera_locs

saveWidget(
  map_export_camera_locs,
  file = "figures/rl_maps_html/map_export_camera_locs.html"
)


# Define color palette

pal_crash <- colorNumeric(
  palette = "Reds",
  domain = df_nta$crash_per_pop,
  na.color = "transparent"
)

map_export_camera_vios <- leaflet() %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    fillColor = ~ pal_crash(crash_per_pop),
    fillOpacity = 0.6,
    color = "#444444",
    weight = 1,
    popup = ~ paste0("Crash Count: ", crash_per_pop)
  ) %>%
  addCircleMarkers(
    data = rl_unique,
    popup = lapply(
      paste(
        "<p>Address: ",
        rl_unique$full_street,
        "</p>",
        "<p>Tickets Issued: ",
        rl_unique$add_count,
        "</p>"
      ),
      HTML
    ),
    radius = 5,
    fillOpacity = 0.8,
    color = "#0055ff",
    weight = 1
  ) %>%
  addLegend(
    "bottomleft",
    pal = pal_crash,
    values = df_nta$crash_per_pop,
    title = "Crashes per Pop",
    opacity = 1
  )

map_export_camera_vios

saveWidget(
  map_export_camera_vios,
  file = "figures/rl_maps_html/map_export_camera_vios.html"
)


# Color palette for Red Light Crashes per Sq KM
pal_rel_crash <- colorBin(
  palette = "YlOrRd",
  domain = df_nta$rel_crash_per_area,
  na.color = "transparent",
  pretty = FALSE,
  bins = quantile(
    df_nta$rel_crash_per_area,
    probs = seq(0, 1, length.out = 7),
    na.rm = TRUE
  )
)

map_export_rel_crashes <- leaflet() %>%

  addProviderTiles("CartoDB.Positron") %>%

  addMapPane("nta", zIndex = 300) %>%
  addMapPane("nta_overlay", zIndex = 500) %>%

  setView(
    lng = -73.94,
    lat = 40.70,
    zoom = 11
  ) %>%

  # Choropleth polygons
  addPolygons(
    data = df_nta,
    fillColor = ~ pal_rel_crash(rel_crash_per_area),
    fillOpacity = 0.75,
    color = "black",
    weight = 1,
    options = pathOptions(pane = "nta"),
    popup = ~ paste0(
      "<strong>",
      NTAName,
      "</strong><br>",
      "Red Light Crashes per Sq KM: ",
      round(rel_crash_per_area, 2)
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_rel_crash,
    values = df_nta$rel_crash_per_area,
    title = "Red Light Crashes per Sq KM"
  )
map_export_rel_crashes

saveWidget(
  map_export_rel_crashes,
  file = "figures/rl_maps_html/map_export_rel_crashes.html"
)

###############################################################################################################
### NTA Maps

### Demographics & Number of Schools

# median income
pal_nta_inc <- colorBin(
  palette = "YlOrRd",
  domain = df_nta$med_inc,
  na.color = "transparent"
)

leaflet() %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    weight = 1,
    fillColor = ~ pal_nta_inc(med_inc),
    color = "#E6E6E6",
    fillOpacity = 0.9,
    popup = lapply(
      paste(
        "<p>",
        "NTA: ",
        df_nta$NTAName,
        "</p>",
        "<p>",
        paste0("Count: ", df_nta$med_inc),
        "</p>"
      ),
      HTML
    )
  ) %>%
  #  addCircleMarkers(popup = lapply(paste("<p>","Address: ",rl_unique$full_street,"</p>",
  #                                        "<p>",paste0("Count: ",rl_unique$add_count),"</p>"), HTML)) %>%
  addLegend(
    "bottomright",
    pal = pal_nta_inc,
    values = df_nta$med_inc,
    title = "Median Income",
    opacity = 1
  )

# Non-Hispanic White
pal_nta_nhw <- colorBin(
  palette = "YlOrRd",
  domain = df_nta$perc_nhw,
  na.color = "transparent"
)

leaflet() %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    weight = 1,
    fillColor = ~ pal_nta_nhw(perc_nhw),
    color = "#E6E6E6",
    fillOpacity = 0.9,
    popup = lapply(
      paste(
        "<p>",
        "NTA: ",
        df_nta$NTAName,
        "</p>",
        "<p>",
        paste0("Count: ", df_nta$perc_nhw),
        "</p>"
      ),
      HTML
    )
  ) %>%
  #  addCircleMarkers(popup = lapply(paste("<p>","Address: ",rl_unique$full_street,"</p>",
  #                                        "<p>",paste0("Count: ",rl_unique$add_count),"</p>"), HTML)) %>%
  addLegend(
    "bottomright",
    pal = pal_nta_nhw,
    values = df_nta$perc_nhw,
    title = "% NH-W",
    opacity = 1
  )

####################################################################################################
### Violations

# Violations
quantile(df_nta$vios_count, seq(0, 1, 0.05), na.rm = TRUE)
#~0%, 25%, 50%, 75%, 90%, 100%

pal_nta <- leaflet::colorBin(
  palette = c('#e6dfd3', '#cfbea5', '#b79e7a', '#9e7f4f', '#846126'),
  bins = c(0, 220, 750, 2170, 7095, 20000),
  domain = df_nta$vios_count,
  na.color = "transparent"
)

# map all violations over NTA pal
map_export_vios_overlay <- leaflet(rl_unique) %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    weight = 1,
    fillColor = ~ pal_nta(vios_count),
    color = "#E6E6E6",
    fillOpacity = 0.5,
    popup = lapply(
      paste(
        "<p>",
        "NTA: ",
        df_nta$NTAName,
        "</p>",
        "<p>",
        paste0("Count: ", df_nta$vios_count),
        "</p>"
      ),
      HTML
    )
  ) %>%
  addCircleMarkers(
    popup = lapply(
      paste(
        "<p>",
        "Address: ",
        rl_unique$full_street,
        "</p>",
        "<p>",
        paste0("Count: ", rl_unique$add_count),
        "</p>"
      ),
      HTML
    ),
    radius = ~ rl_unique$add_count / 1000
  ) %>%
  addLegend(
    "bottomright",
    pal = pal_nta,
    values = df_nta$vios_count,
    title = "Number of Red Light Camera Violations",
    opacity = 0.5
  )

map_export_vios_overlay

saveWidget(
  map_export_vios_overlay,
  file = "figures/rl_maps_html/map_export_vios_overlay.html"
)

# map NTA w/out violations overlaid
map_export_vios <- leaflet(rl_unique) %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    weight = 1,
    fillColor = ~ pal_nta(vios_count),
    color = "#E6E6E6",
    fillOpacity = 0.9,
    popup = lapply(
      paste(
        "<p>",
        "NTA: ",
        df_nta$NTAName,
        "</p>",
        "<p>",
        paste0("Count: ", df_nta$vios_count),
        "</p>"
      ),
      HTML
    )
  ) %>%
  # addCircleMarkers(popup = lapply(paste("<p>","Address: ",rl_unique$full_street,"</p>",
  #                                       "<p>",paste0("Count: ",rl_unique$add_count),"</p>"), HTML),
  #                  radius = ~rl_unique$add_count/1000) %>%
  addLegend(
    "bottomright",
    pal = pal_nta,
    values = df_nta$vios_count,
    title = "Number of Red Light Camera Violations",
    opacity = 1
  )

map_export_vios

mapshot(
  map_export_vios,
  file = "figures/rl_maps_png/map_export_vios.png",
  vwidth = 900,
  vheight = 870
)

saveWidget(map_export_vios, file = "figures/rl_maps_html/map_export_vios.html")

# map NTA median violations per light

# Define color palette
pal_med_vios <- colorNumeric(
  palette = "YlOrRd", # choose any palette you like
  domain = df_nta$med_vios
)

map_export_med_vios <- leaflet(df_nta) %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    weight = 1,
    fillColor = ~ pal_med_vios(med_vios),
    color = "#E6E6E6",
    fillOpacity = 0.9,
    popup = lapply(
      paste(
        "<p>",
        "NTA: ",
        df_nta$NTAName,
        "</p>",
        "<p>",
        paste0("Count: ", df_nta$med_vios),
        "</p>"
      ),
      HTML
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal_med_vios,
    values = df_nta$med_vios,
    title = "Median Number of Red Light Camera Violations",
    opacity = 1
  )

map_export_med_vios

mapshot(
  map_export_med_vios,
  file = "figures/rl_maps_png/map_export_med_vios.png",
  vwidth = 900,
  vheight = 870
)


###############################################################################################################
### Cameras

# number of cameras
quantile(df_nta$num_cams, seq(0, 1, 0.05), na.rm = TRUE)
#~0%, 25%, 50%, 75%, 90%, 100%

pal_nta_cams <- leaflet::colorBin(
  palette = c('#e6dfd3', '#cfbea5', '#b79e7a', '#9e7f4f', '#846126'),
  bins = c(0, 1, 2, 3, 5, 13),
  domain = df_nta$num_cams,
  na.color = "transparent"
)

# map number of cameras w/out violations overlaid
map_export_num_cams <- leaflet(rl_unique) %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    weight = 1,
    fillColor = ~ pal_nta_cams(num_cams),
    color = "#E6E6E6",
    fillOpacity = 0.9,
    popup = lapply(
      paste(
        "<p>",
        "NTA: ",
        df_nta$NTAName,
        "</p>",
        "<p>",
        paste0("Count: ", df_nta$num_cams),
        "</p>"
      ),
      HTML
    )
  ) %>%
  # addCircleMarkers(popup = lapply(paste("<p>","Address: ",rl_unique$full_street,"</p>",
  #                                       "<p>",paste0("Count: ",rl_unique$add_count),"</p>"), HTML),
  #                  radius = ~rl_unique$add_count/1000) %>%
  addLegend(
    "bottomright",
    pal = pal_nta_cams,
    values = df_nta$num_cams,
    title = "Number of Red Light Cameras",
    opacity = 1
  )

map_export_num_cams

mapshot(
  map_export_num_cams,
  file = "figures/rl_maps_png/map_export_num_cams.png",
  vwidth = 900,
  vheight = 870
)
# saveWidget(map_export_num_cams, file="figures/maps_html/map_export_num_cams.html")

pal_nta_cams_area <- leaflet::colorBin(
  palette = 'YlOrRd',
  domain = df_nta$cams_per_area,
  na.color = "transparent"
)

# map number of cameras w/out violations overlaid
map_export_num_cams_area <- leaflet(rl_unique) %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    weight = 1,
    fillColor = ~ pal_nta_cams_area(cams_per_area),
    color = "#E6E6E6",
    fillOpacity = 0.9,
    popup = lapply(
      paste(
        "<p>",
        "NTA: ",
        df_nta$NTAName,
        "</p>",
        "<p>",
        paste0("Count: ", df_nta$cams_per_area),
        "</p>"
      ),
      HTML
    )
  ) %>%
  # addCircleMarkers(popup = lapply(paste("<p>","Address: ",rl_unique$full_street,"</p>",
  #                                       "<p>",paste0("Count: ",rl_unique$add_count),"</p>"), HTML),
  #                  radius = ~rl_unique$add_count/1000) %>%
  addLegend(
    "bottomright",
    pal = pal_nta_cams_area,
    values = df_nta$cams_per_area,
    title = "Red Light Cameras per Km^2",
    opacity = 1
  )

map_export_num_cams_area

mapshot(
  map_export_num_cams_area,
  file = "figures/rl_maps_png/map_export_num_cams_area.png",
  vwidth = 900,
  vheight = 870
)
# saveWidget(map_export_num_cams, file="figures/maps_html/map_export_num_cams.html")

#### Cams per light

pal_nta_cams_light <- leaflet::colorBin(
  palette = 'YlOrRd',
  domain = df_nta$cams_per_light,
  na.color = "transparent"
)

# map number of cameras w/out violations overlaid
map_export_num_cams_light <- leaflet(rl_unique) %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    weight = 1,
    fillColor = ~ pal_nta_cams_light(cams_per_light),
    color = "#E6E6E6",
    fillOpacity = 0.9,
    popup = lapply(
      paste(
        "<p>",
        "NTA: ",
        df_nta$NTAName,
        "</p>",
        "<p>",
        paste0("Count: ", df_nta$cams_per_light),
        "</p>"
      ),
      HTML
    )
  ) %>%
  # addCircleMarkers(popup = lapply(paste("<p>","Address: ",rl_unique$full_street,"</p>",
  #                                       "<p>",paste0("Count: ",rl_unique$add_count),"</p>"), HTML),
  #                  radius = ~rl_unique$add_count/1000) %>%
  addLegend(
    "bottomright",
    pal = pal_nta_cams_light,
    values = df_nta$cams_per_light,
    title = "Number of Red Light Cameras per Traffic Light",
    opacity = 1
  )

map_export_num_cams_light

#######################################################################################################################################
### Crashes

# number of crashes
quantile(df_nta$crash_count, seq(0, 1, 0.05), na.rm = TRUE)
#~0%, 25%, 50%, 75%, 90%, 100%

pal_nta_crash <- colorBin(domain = df_nta$crash_count)

map_export_num_crashes <- leaflet() %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    weight = 1,
    fillColor = ~ pal_nta_crash(crash_count),
    color = "#E6E6E6",
    fillOpacity = 0.9,
    popup = lapply(
      paste(
        "<p>",
        "NTA: ",
        df_nta$NTAName,
        "</p>",
        "<p>",
        paste0("Count: ", df_nta$crash_count),
        "</p>"
      ),
      HTML
    )
  ) %>%
  #  addCircleMarkers(popup = lapply(paste("<p>","Address: ",rl_unique$full_street,"</p>",
  #                                        "<p>",paste0("Count: ",rl_unique$add_count),"</p>"), HTML)) %>%
  addLegend(
    "bottomright",
    pal = pal_nta_crash,
    values = df_nta$crash_count,
    title = "Number of Crashes",
    opacity = 1
  )

map_export_num_crashes

mapshot(
  map_export_num_crashes,
  file = "figures/rl_maps_png/map_export_num_crashes.png",
  vwidth = 900,
  vheight = 870
)
# saveWidget(map_export_num_crashes, file="figures/maps_html/map_export_num_crashes.html")

#### Crashes per Area

pal_nta_crash_area <- colorBin(
  palette = "YlOrRd",
  domain = df_nta$crash_per_area,
  na.color = "transparent",
  pretty = FALSE,
  bins = quantile(
    df_nta$crash_per_area,
    probs = seq(0, 1, length.out = 7),
    na.rm = TRUE
  )
)


map_export_num_crashes <- leaflet() %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    weight = 1,
    fillColor = ~ pal_nta_crash_area(crash_per_area),
    color = "#E6E6E6",
    fillOpacity = 0.9,
    popup = lapply(
      paste(
        "<p>",
        "NTA: ",
        df_nta$NTAName,
        "</p>",
        "<p>",
        paste0("Count: ", df_nta$crash_per_area),
        "</p>"
      ),
      HTML
    )
  ) %>%
  #  addCircleMarkers(popup = lapply(paste("<p>","Address: ",rl_unique$full_street,"</p>",
  #                                        "<p>",paste0("Count: ",rl_unique$add_count),"</p>"), HTML)) %>%
  addLegend(
    "bottomright",
    pal = pal_nta_crash_area,
    values = df_nta$crash_per_area,
    title = "Number of Crashes per Area",
    opacity = 1
  )

map_export_num_crashes

#######################################################################################################################################
### Normalized maps

### number of violations per crashes
quantile(df_nta$vios_per_crash, seq(0, 1, 0.05), na.rm = TRUE)
#~0%, 25%, 50%, 75%, 90%, 100%

pal_nta_vios_crash <- leaflet::colorBin(
  palette = c('#e6dfd3', '#cfbea5', '#b79e7a', '#9e7f4f', '#846126'),
  bins = c(0, 0.5, 2.5, 10, 20, 353),
  domain = df_nta$vios_per_crash,
  na.color = "transparent"
)

map_export_vios_crash <- leaflet() %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    weight = 1,
    fillColor = ~ pal_nta_vios_crash(vios_per_crash),
    color = "#E6E6E6",
    fillOpacity = 0.9,
    popup = lapply(
      paste(
        "<p>",
        "NTA: ",
        df_nta$NTAName,
        "</p>",
        "<p>",
        paste0("Count: ", df_nta$vios_per_crash),
        "</p>"
      ),
      HTML
    )
  ) %>%
  #  addCircleMarkers(popup = lapply(paste("<p>","Address: ",rl_unique$full_street,"</p>",
  #                                        "<p>",paste0("Count: ",rl_unique$add_count),"</p>"), HTML)) %>%
  addLegend(
    "bottomright",
    pal = pal_nta_vios_crash,
    values = df_nta$vios_per_crash,
    title = "Number of Red Light Camera</br>Violations Per Crash",
    opacity = 1
  )

map_export_vios_crash

mapshot(
  map_export_vios_crash,
  file = "figures/rl_maps_png/map_export_vios_crash.png",
  vwidth = 900,
  vheight = 870
)

### number of cams per crashes
quantile(df_nta$cams_per_crash, seq(0, 1, 0.05), na.rm = TRUE)
#~0%, 25%, 50%, 75%, 90%, 100%

pal_nta_cams_crash <- leaflet::colorBin(
  palette = c('#e6dfd3', '#cfbea5', '#b79e7a', '#9e7f4f', '#846126'),
  bins = c(0, 0.0025, 0.004, 0.009, 0.015, 0.08),
  domain = df_nta$cams_per_crash,
  na.color = "transparent"
)

leaflet() %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(
    data = df_nta,
    weight = 1,
    fillColor = ~ pal_nta_cams_crash(cams_per_crash),
    color = "#E6E6E6",
    fillOpacity = 0.9,
    popup = lapply(
      paste(
        "<p>",
        "NTA: ",
        df_nta$NTAName,
        "</p>",
        "<p>",
        paste0("Count: ", df_nta$cams_per_crash),
        "</p>"
      ),
      HTML
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal_nta_cams_crash,
    values = df_nta$cams_per_crash,
    title = "Number of Red Light</br>Cameras Per Crash",
    opacity = 1
  )


###################
### Install Map ###
###################

rl_dates <- rl_unique %>%
  mutate(
    cap_date = if_else(
      first_appearance < ymd("2025-12-31"),
      ymd("2025-12-01"),
      first_appearance
    )
  ) %>%
  mutate(months = format(cap_date, format = "%Y-%m")) %>%
  mutate(months = if_else(months == "2025-12", "2025 or before", months))

pal_date <- colorFactor("Set1", domain = rl_dates$months)

leaflet(rl_dates) %>%
  addTiles() %>%
  addCircleMarkers(
    color = ~ pal_date(rl_dates$months),
    popup = paste0(
      rl_dates$full_street,
      " installed ",
      rl_dates$first_appearance
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal_date,
    values = rl_dates$months,
    title = "Date Camera Was Installed",
    opacity = 1
  )

rl_dates %>%
  st_drop_geometry() %>%
  count(months)

## Overlapping heatmap

# Color palette for Red Light Crashes per Sq KM
pal_rel_crash <- colorBin(
  palette = "YlOrRd",
  domain = df_nta$rel_crash_per_area,
  na.color = "transparent",
  pretty = FALSE,
  bins = quantile(
    df_nta$rel_crash_per_area,
    probs = seq(0, 1, length.out = 7),
    na.rm = TRUE
  )
)

overlay_map <- leaflet(
  options = leafletOptions(
    minZoom = 11,
    maxZoom = 11,
    zoomControl = FALSE,
    scrollWheelZoom = FALSE,
    doubleClickZoom = FALSE,
    boxZoom = FALSE,
    keyboard = FALSE,
    touchZoom = FALSE
  )
) %>%

  addProviderTiles("CartoDB.Positron") %>%

  addMapPane("nta", zIndex = 300) %>%
  addMapPane("nta_overlay", zIndex = 500) %>%

  setView(
    lng = -73.94,
    lat = 40.70,
    zoom = 11
  ) %>%

  # Heatmap
  addHeatmap(
    data = rl_unique,
    lng = ~lon,
    lat = ~lat,
    blur = 20,
    radius = 20,
    intensity = 0.125
  ) %>%

  # Choropleth polygons
  addPolygons(
    data = df_nta,
    fillColor = ~ pal_rel_crash(rel_crash_per_area),
    fillOpacity = 0.75,
    color = "black",
    weight = 1,
    options = pathOptions(pane = "nta"),
    popup = ~ paste0(
      "<strong>",
      NTAName,
      "</strong><br>",
      "Red Light Crashes per Sq KM: ",
      round(rel_crash_per_area, 2)
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_rel_crash,
    values = df_nta$rel_crash_per_area,
    title = "Red Light Crashes per Sq KM"
  )
overlay_map


### Pure camera heatmap

camera_heatmap <- leaflet(
  options = leafletOptions(
    minZoom = 11,
    maxZoom = 11,
    zoomControl = FALSE,
    scrollWheelZoom = FALSE,
    doubleClickZoom = FALSE,
    boxZoom = FALSE,
    keyboard = FALSE,
    touchZoom = FALSE
  )
) %>%

  addProviderTiles("CartoDB.Positron") %>%

  setView(
    lng = -73.94,
    lat = 40.70,
    zoom = 11
  ) %>%

  # Heatmap
  addHeatmap(
    data = rl_unique,
    lng = ~lon,
    lat = ~lat,
    blur = 20,
    radius = 20,
    intensity = 0.125
  )

camera_heatmap


# Light locations

clusters <- lights %>%
  st_transform(2263) %>%
  st_buffer(dist = 50) %>%
  st_union() %>%
  st_cast("POLYGON")


intersections_df <- st_centroid(clusters) %>%
  st_transform(4326) %>%
  st_as_sf()

map_export_lights_locs <- leaflet(intersections_df) %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    radius = 0.1,
    opacity = 0.25
  ) %>%
  addTiles(
    urlTemplate = paste0(
      "https://{s}.basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=",
      Sys.getenv("CARTO_API_KEY")
    ),
    attribution = '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, &copy <a href="https://carto.com/attributions">CARTO</a>',
    options = tileOptions(
      subdomains = c("a", "b", "c", "d"),
      maxZoom = 20
    )
  ) %>%
  addCircleMarkers(
    data = rl_unique,
    radius = 3,
    color = "red",
    opacity = 0.9
  ) %>%
  addPolygons(data = df_nta, color = "black", fill = FALSE, weight = 2)

map_export_lights_locs

saveWidget(
  map_export_lights_locs,
  file = "figures/rl_maps_html/map_export_lights_locs.html"
)
