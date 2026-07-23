## Steps ---------------------------------------------------------

# 1. Load dependencies and model-ready data.
# 2. Calculate occupied seats per child.
# 3. Join ECE data to PUMA geometry.
# 4. Create interactive occupied-seats map.

source("00_load_dependencies.R")

dtb <- fread("data/lm_model_data.csv")
dtb[
  ,
  `:=`(occupied_seats = total_seats - total_vacancy,
       occupied_seats_per_child = (total_seats - total_vacancy) /pre_sch_age)]
dtb$puma <- as.character(dtb$puma)

pumas <- st_read("data/puma_nyc.geojson", quiet = TRUE)
pumas$puma <- sprintf("%04d", as.integer(pumas$puma))

dtb_sf <- pumas %>%
  left_join(dtb, by = "puma") %>%
  st_transform(4326) %>%
  mutate(
    puma_name = NAMELSAD20 %>%
      str_remove("^NYC-") %>%
      str_remove(" PUMA$") %>%
      str_replace("^.*--", "") %>%
      str_squish()
  )

pal_occupied <- leaflet::colorBin(
  palette = if (requireNamespace("councildown", quietly = TRUE)) {
    councildown::pal_nycc("cool", reverse = TRUE)
  } else {
    councilverse::pal_nycc("cool", reverse = TRUE)
  },
  domain = dtb_sf$occupied_seats_per_child,
  bins = pretty(dtb_sf$occupied_seats_per_child, n = 6),
  na.color = "#F5F5F5",
  reverse = TRUE
)

popup <- sprintf(
  "<b>%s</b><br><br>
  <b>PUMA:</b> %s<br>
  <b>Occupied seats per child:</b> %.3f<br>
  <b>Occupied seats:</b> %s<br>
  <b>Total seats:</b> %s<br>
  <b>Total vacancies:</b> %s<br>
  <b>Estimated preschool-age children:</b> %s<br>
  <b>Median household income:</b> %s",
  dtb_sf$puma_name,
  dtb_sf$puma,
  dtb_sf$occupied_seats_per_child,
  comma(dtb_sf$occupied_seats),
  comma(dtb_sf$total_seats),
  comma(dtb_sf$total_vacancy),
  comma(dtb_sf$pre_sch_age),
  dollar(dtb_sf$median_household_income, accuracy = 1)
)

bb <- st_bbox(dtb_sf)

occupied_map <- leaflet(dtb_sf) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  fitBounds(
    lng1 = bb[["xmin"]],
    lat1 = bb[["ymin"]],
    lng2 = bb[["xmax"]],
    lat2 = bb[["ymax"]]
  ) %>%
  addPolygons(
    fillColor = ~pal_occupied(occupied_seats_per_child),
    fillOpacity = 0.85,
    color = "white",
    weight = 1,
    popup = popup,
    label = ~puma_name,
    highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)
  ) %>%
  addLegend(
    pal = pal_occupied,
    values = ~occupied_seats_per_child,
    title = "Occupied seats per child",
    position = "bottomright"
  )

occupied_map_box <- htmltools::div(
  class = "map-box",
  occupied_map
)
