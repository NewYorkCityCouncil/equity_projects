## Code for Quarto

#### Load Packages
library(leaflet)
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
  addProviderTiles("CartoDB.Positron") %>%
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


############################
#### Related Crashes Map

# Color palette
pal_rel_crash <- colorNumeric(
  palette = "YlOrRd",
  domain = df_nta$rel_crash_per_area,
  na.color = "transparent"
)

# pal_rel_crash <- colorBin(
#   palette = "YlOrRd",
#   domain = df_nta$rel_crash_per_area,
#   na.color = "transparent",
#   pretty = FALSE,
#   bins = quantile(
#     df_nta$rel_crash_per_area,
#     probs = seq(0, 1, length.out = 7),
#     na.rm = TRUE
#   )
# )

map_export_rel_crashes <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  setView(
    lng = -73.94,
    lat = 40.70,
    zoom = 10
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

rl_model_nb <- MASS::glm.nb(
  num_cams ~
    perc_nhb +
    perc_his +
    perc_nha +
    related_crashes +
    offset(log(area)),
  data = df_nta
)

sim_nb <- simulateResiduals(rl_model_nb)

plot(sim_nb)
testUniformity(sim_nb)
testDispersion(sim_nb)

rl_table <- rl_model_nb %>%
  tidy() %>%
  mutate(
    term = recode_values(
      term,
      # "perc_nhw" ~ "% White",
      "perc_nhb" ~ "% Non-Hisp. Black",
      "perc_nha" ~ "% Non-Hisp. Asian",
      "perc_his" ~ "% Hispanic",
      # "perc_oth" ~ "% Other Race",
      "related_crashes" ~ "Related Crashes",
      "(Intercept)" ~ "(Intercept)"
    )
  ) %>%
  select(Variable = term, Estimate = estimate, `P-Value` = p.value) %>%
  flextable() %>%
  colformat_double(digits = 2) %>%
  set_formatter(`P-Value` = function(x) {
    formatC(x, format = "e", digits = 2)
  }) %>%
  set_table_properties(layout = "autofit")

rl_table
