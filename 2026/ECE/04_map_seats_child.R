## Steps ---------------------------------------------------------

# 1. Load dependencies and model-ready data.
# 2. Join ECE data to PUMA geometry.
# 3. Build popup text and color bins.
# 4. Create interactive seats-per-child map.

source("00_load_dependencies.R")

dtb <- fread("data/lm_model_data.csv")
dtb[, puma := sprintf("%04d", as.integer(puma))]

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



popup <- sprintf(
  "<b>%s</b><br><br>
  <b>PUMA:</b> %s<br>
  <b>Seats / child:</b> %.1f<br>
  <b>Median household income:</b> %s<br><br>
  <b>Hispanic:</b> %.1f%%<br>
  <b>Black:</b> %.1f%%<br>
  <b>Asian/API:</b> %.1f%%",
  dtb_sf$puma_name,
  dtb_sf$puma,
  dtb_sf$seats_per_child,
  dollar(dtb_sf$median_household_income, accuracy = 1),
  100 * dtb_sf$pct_hispanic,
  100 * dtb_sf$pct_black_nh,
  100 * dtb_sf$pct_asian_api_nh
)



cool_pal <- if (requireNamespace("councildown", quietly = TRUE)) {
  councildown::pal_nycc("cool", reverse = TRUE)
} else {
  councilverse::pal_nycc("cool", reverse = TRUE)
}

pal_seats <- leaflet::colorBin(
  palette = cool_pal,
  domain = dtb_sf$seats_per_child,
  bins = pretty(dtb_sf$seats_per_child, n = 6),
  na.color = "#F5F5F5", 
  reverse = TRUE
)

pal_income <- leaflet::colorBin(
  palette = cool_pal,
  domain = dtb_sf$median_household_income,
  bins = pretty(dtb_sf$median_household_income, n = 6),
  na.color = "#F5F5F5", 
  reverse = TRUE
)

pal_hisp <- leaflet::colorBin(
  palette = cool_pal,
  domain = 100 * dtb_sf$pct_hispanic,
  bins = pretty(100 * dtb_sf$pct_hispanic, n = 6),
  na.color = "#F5F5F5", 
  reverse = TRUE
)

pal_black <- leaflet::colorBin(
  palette = cool_pal,
  domain = 100 * dtb_sf$pct_black_nh,
  bins = pretty(100 * dtb_sf$pct_black_nh, n = 6),
  na.color = "#F5F5F5", 
  reverse = TRUE
)

pal_asian <- leaflet::colorBin(
  palette = cool_pal,
  domain = 100 * dtb_sf$pct_asian_api_nh,
  bins = pretty(100 * dtb_sf$pct_asian_api_nh, n = 6),
  na.color = "#F5F5F5", 
  reverse = TRUE
)

bb <- st_bbox(dtb_sf)

# map 

map <- leaflet(dtb_sf) %>%
  addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
  fitBounds(
    lng1 = bb[["xmin"]],
    lat1 = bb[["ymin"]],
    lng2 = bb[["xmax"]],
    lat2 = bb[["ymax"]]
  ) %>%
  
  addPolygons(
    fillColor = ~pal_seats(seats_per_child),
    fillOpacity = 0.85,
    color = "white",
    weight = 1,
    popup = popup,
    label = ~puma_name,
    group = "Seats per child",
    highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)
  ) %>%
  
  addPolygons(
    fillColor = ~pal_income(median_household_income),
    fillOpacity = 0.85,
    color = "white",
    weight = 1,
    popup = popup,
    label = ~puma_name,
    group = "Median household income",
    highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)
  ) %>%
  
  addPolygons(
    fillColor = ~pal_hisp(100 * pct_hispanic),
    fillOpacity = 0.85,
    color = "white",
    weight = 1,
    popup = popup,
    label = ~puma_name,
    group = "% Hispanic",
    highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)
  ) %>%
  
  addPolygons(
    fillColor = ~pal_black(100 * pct_black_nh),
    fillOpacity = 0.85,
    color = "white",
    weight = 1,
    popup = popup,
    label = ~puma_name,
    group = "% Black",
    highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)
  ) %>%
  
  addPolygons(
    fillColor = ~pal_asian(100 * pct_asian_api_nh),
    fillOpacity = 0.85,
    color = "white",
    weight = 1,
    popup = popup,
    label = ~puma_name,
    group = "% Asian/API",
    highlightOptions = highlightOptions(weight = 3, color = "black", bringToFront = TRUE)
  ) %>%
  
  hideGroup(c("Median household income", "% Hispanic", "% Black", "% Asian/API")) %>%
  
  addLayersControl(
    baseGroups = c(
      "Seats per child",
      "Median household income",
      "% Hispanic",
      "% Black",
      "% Asian/API"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  addLegend(
    pal = pal_seats,
    values = ~seats_per_child,
    title = "Seats per pre-school aged child",
    position = "bottomright",
    className = "legend seats-legend"
  ) %>%
  addLegend(
    pal = pal_income,
    values = ~median_household_income,
    title = "Median household income",
    labFormat = labelFormat(prefix = "$", big.mark = ","),
    position = "bottomright",
    className = "legend wage-legend"
  ) %>%
  addLegend(
    pal = pal_hisp,
    values = ~100 * pct_hispanic,
    title = "% Hispanic",
    labFormat = labelFormat(suffix = "%"),
    position = "bottomright",
    className = "legend hisp-legend"
  ) %>%
  addLegend(
    pal = pal_black,
    values = ~100 * pct_black_nh,
    title = "% Black",
    labFormat = labelFormat(suffix = "%"),
    position = "bottomright",
    className = "legend black-legend"
  ) %>%
  addLegend(
    pal = pal_asian,
    values = ~100 * pct_asian_api_nh,
    title = "% Asian/API",
    labFormat = labelFormat(suffix = "%"),
    position = "bottomright",
    className = "legend asian-legend"
  ) %>%
  
  onRender("
    function(el, x) {
      function showLegend(group) {
        $('.legend').hide();
        if (group === 'Seats per child') $('.seats-legend').show();
        if (group === 'Median household income') $('.wage-legend').show();
        if (group === '% Hispanic') $('.hisp-legend').show();
        if (group === '% Black') $('.black-legend').show();
        if (group === '% Asian/API') $('.asian-legend').show();
      }

      showLegend('Seats per child');

      this.on('baselayerchange', function(e) {
        showLegend(e.name);
      });
    }
  ")
