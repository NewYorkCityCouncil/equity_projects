## Code for Quarto

#### Load Packages
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(leaflet.extras)
library(sf)
library(flextable)
library(broom)
library(councilverse)
library(DHARMa)
library(tidyverse)


#### Load Datasets
rl_unique <- st_read("data/rl_unique.gpkg")
df_nta <- st_read("data/df_nta.gpkg")

#####################
#### Camera Heatmap

camera_heatmap <- leaflet(
  options = leafletOptions(
    minZoom = 10.5,
    maxZoom = 10.5,
    zoomControl = FALSE,
    scrollWheelZoom = FALSE,
    doubleClickZoom = FALSE,
    boxZoom = FALSE,
    keyboard = FALSE,
    touchZoom = FALSE,
    zoomSnap = 0
  )
) %>%
  addTiles(
    urlTemplate = paste0(
      "https://{s}.basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=", # pretty sure you just have to know the code to put after rastertiles
      Sys.getenv("CARTO_API_KEY")
    ),
    attribution = '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, © <a href="https://carto.com/attributions">CARTO</a>',
    options = tileOptions(
      subdomains = c("a", "b", "c", "d"),
      maxZoom = 20
    )
  ) %>%
  setView(
    lng = -73.94,
    lat = 40.70,
    zoom = 10.5
  ) %>%
  addHeatmap(
    data = rl_unique,
    lng = ~lon,
    lat = ~lat,
    blur = 10,
    radius = 15,
    intensity = 0.1
  )
camera_heatmap


pal_nta_cams_area <- colorNumeric(
  palette = "YlOrRd",
  domain = df_nta$cams_per_area,
  na.color = "transparent"
)

# map number of cameras w/out violations overlaid
map_num_cams_area <- leaflet(rl_unique) %>%
  setView(
    lng = -73.94,
    lat = 40.70,
    zoom = 10
  ) %>%
  addTiles(
    urlTemplate = paste0(
      "https://{s}.basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=", # pretty sure you just have to know the code to put after rastertiles
      Sys.getenv("CARTO_API_KEY")
    ),
    attribution = '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, © <a href="https://carto.com/attributions">CARTO</a>',
    options = tileOptions(
      subdomains = c("a", "b", "c", "d"),
      maxZoom = 20
    )
  ) %>%
  addPolygons(
    data = df_nta,
    fillColor = ~ pal_nta_cams_area(cams_per_area),
    fillOpacity = 0.75,
    color = "black",
    weight = 1,
    popup = paste(
      "NTA: ",
      df_nta$NTAName,
      paste0("Count: ", df_nta$cams_per_area)
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

map_num_cams_area

############################
#### Related Crashes Map

# Color palette
pal_rel_crash <- colorNumeric(
  palette = "YlOrRd",
  domain = df_nta$rel_crash_per_area,
  na.color = "transparent"
)

map_export_rel_crashes <- leaflet() %>%
  setView(
    lng = -73.94,
    lat = 40.70,
    zoom = 10
  ) %>%
  addTiles(
    urlTemplate = paste0(
      "https://{s}.basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=", # pretty sure you just have to know the code to put after rastertiles
      Sys.getenv("CARTO_API_KEY")
    ),
    attribution = '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, © <a href="https://carto.com/attributions">CARTO</a>',
    options = tileOptions(
      subdomains = c("a", "b", "c", "d"),
      maxZoom = 20
    )
  ) %>%
  # Choropleth polygons
  addPolygons(
    data = df_nta,
    fillColor = ~ pal_rel_crash(rel_crash_per_area),
    fillOpacity = 0.75,
    color = "black",
    weight = 1,
    popup = ~ paste0(
      "<strong>",
      NTAName,
      "</strong><br>",
      "Related Crashes per Sq KM: ",
      round(rel_crash_per_area, 2)
    )
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal_rel_crash,
    values = df_nta$rel_crash_per_area,
    title = "Related Crashes per KM^2"
  )
map_export_rel_crashes


## Install Map

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

install_map <- leaflet(rl_dates) %>%
  addTiles(
    urlTemplate = paste0(
      "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png?key=", # pretty sure you just have to know the code to put after rastertiles
      Sys.getenv("CARTO_API_KEY")
    ),
    attribution = '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, © <a href="https://carto.com/attributions">CARTO</a>',
    options = tileOptions(
      subdomains = c("a", "b", "c", "d"),
      maxZoom = 20
    )
  ) %>%
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
install_map


map_export_camera_locs <- leaflet(rl_unique) %>%
  setView(-73.941281, 40.704103, zoom = 11) %>%
  addTiles(
    urlTemplate = paste0(
      "https://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png?key=", # pretty sure you just have to know the code to put after rastertiles
      Sys.getenv("CARTO_API_KEY")
    ),
    attribution = '© <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>, © <a href="https://carto.com/attributions">CARTO</a>',
    options = tileOptions(
      subdomains = c("a", "b", "c", "d"),
      maxZoom = 20
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
        paste0("Tickets Issued: ", rl_unique$add_count),
        "</p>"
      ),
      HTML
    ),
    radius = 0.1
  )

map_export_camera_locs

############################
#### Cams by race, normalized by area

# rl_model <-
#   glm(
#     num_cams ~
#       # perc_nhw +
#       perc_nhb +
#       perc_his +
#       perc_nha +
#       related_crashes +
#       offset(log(area)),
#     family = poisson(link = "log"),
#     data = df_nta
#   )

# rl_table <- rl_model %>%
#   tidy() %>%
#   mutate(
#     term = recode_values(
#       term,
#       # "perc_nhw" ~ "% White",
#       "perc_nhb" ~ "% Non-Hisp. Black",
#       "perc_nha" ~ "% Non-Hisp. Asian",
#       "perc_his" ~ "% Hispanic",
#       # "perc_oth" ~ "% Other Race",
#       "related_crashes" ~ "Related Crashes",
#       "(Intercept)" ~ "(Intercept)"
#     )
#   ) %>%
#   select(Variable = term, Estimate = estimate, `P-Value` = p.value) %>%
#   flextable() %>%
#   colformat_double(digits = 2) %>%
#   set_formatter(`P-Value` = function(x) {
#     formatC(x, format = "e", digits = 2)
#   }) %>%
#   set_table_properties(layout = "autofit")
# rl_table

# # Simulate residuals
# sim_res <- simulateResiduals(
#   fittedModel = rl_model,
#   n = 1000
# )

# # Basic diagnostic plots
# plot(sim_res)

# testDispersion(sim_res)
# testUniformity(sim_res)
# # testZeroInflation(sim_res)

df_nta_renamed <- df_nta %>%
  mutate(
    perc_white = perc_nhw * 100,
    perc_black = perc_nhb * 100,
    perc_hispanic = perc_his * 100,
    perc_asian = perc_nha * 100,
    perc_other = perc_oth * 100
  ) %>%
  select(
    num_cams,
    perc_white,
    perc_black,
    perc_hispanic,
    perc_asian,
    perc_other,
    related_crashes,
    lights,
    area,
    total_pop
  )

rl_model_nb <- MASS::glm.nb(
  num_cams ~
    perc_black +
    # perc_white +
    perc_hispanic +
    perc_asian +
    perc_other +
    related_crashes +
    log(lights) +
    area,
  # offset(log(area)),
  data = df_nta_renamed
)

rl_model_nb %>% summary()

stargazer::stargazer(rl_model_nb)

# sim_nb <- simulateResiduals(rl_model_nb)

# plot(sim_nb)
# testUniformity(sim_nb)
# testDispersion(sim_nb)

rl_table <- rl_model_nb %>%
  tidy() %>%
  mutate(
    term = recode_values(
      term,
      "perc_white" ~ "% White",
      "perc_black" ~ "% Non-Hisp. Black",
      "perc_asian" ~ "% Non-Hisp. Asian",
      "perc_hispanic" ~ "% Hispanic",
      "perc_other" ~ "% Other Race",
      "related_crashes" ~ "Related Crashes",
      "(Intercept)" ~ "(Intercept)",
      "log(lights)" ~ "# Traffic Lights (log)",
      "area" ~ "Area of NTA (Km^2)"
    )
  ) %>%
  select(
    Variable = term,
    Estimate = estimate,
    `P-Value` = p.value
  ) %>%
  mutate(Significant = if_else(`P-Value` < 0.05, "*", "")) %>%
  flextable() %>%
  colformat_double(digits = 3) %>%
  # set_formatter(`P-Value` = function(x) {
  #   formatC(x, format = "e", digits = 2)
  # }) %>%
  set_table_properties(layout = "autofit")

rl_table

perc_black_table <- data.frame(Perc_Black = seq(0, 100, 10)) %>%
  mutate(`% Increase` = exp(Perc_Black * 0.013) - 1) %>%
  mutate(
    `% Increase` = scales::percent(`% Increase`),
    `% Black` = scales::percent(Perc_Black / 100)
  ) %>%
  select(`% Black`, `% Increase`) %>%
  flextable()
perc_black_table
