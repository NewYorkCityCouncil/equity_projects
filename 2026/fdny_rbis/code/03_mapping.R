source("code/02_plots.R")

# CD boundaries + centroids -----------------------------------------------
cd_sf <- st_read("data/input/nycc_22c/nycc.shp", quiet = TRUE) |>
  st_transform(4326)

cd_centroids <- councildown::nycc_cd_13 |>
  st_drop_geometry() |>
  select(coun_dist, lab_x, lab_y) |>
  as.data.table() |>
  rename('CounDist' = coun_dist, 'lon' = lab_x, 'lat' = lab_y)

# Helper -----------------------------------------------
scale_icon_size <- function(n, min_size = 10, max_size = 50) {
  if (all(is.na(n)) || max(n, na.rm = TRUE) == min(n, na.rm = TRUE))
    return(rep(min_size, length(n)))
  min_size + (n - min(n, na.rm = TRUE)) /
    (max(n, na.rm = TRUE) - min(n, na.rm = TRUE)) * (max_size - min_size)
}

# Interactive Shiny map -----------------------------------------------
ui <- fluidPage(
  titlePanel("NYC Fire Inspections & Incidents by Council District"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("insp_source", "Inspection Data",
                   choices = c("MIFC" = "mifc", "RBIS" = "rbis"),
                   selected = "mifc", inline = TRUE),
      sliderInput("insp_year", "Inspection Year",
                  min = min(insp_years), max = max(insp_years),
                  value = max(insp_years), step = 1, sep = ""),
      hr(),
      sliderInput("fidd_year", "Fire Incident Year",
                  min = min(fidd_years), max = max(fidd_years),
                  value = max(fidd_years), step = 1, sep = ""),
      hr(),
      helpText("Choropleth: inspections per building per CD."),
      helpText("Fire icons: structural fires per building per CD (larger = more fires).")
    ),
    mainPanel(leafletOutput("map", height = "700px"))
  )
)

server <- function(input, output, session) {

  insp_data <- reactive({
    tbl <- if (input$insp_source == "mifc") mifc_by_cd_year else rbis_by_cd_year
    tbl[year == input$insp_year]
  })

  fidd_data <- reactive(fidd_by_cd_year[year == input$fidd_year])

  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      setView(lng = -73.94, lat = 40.71, zoom = 10)
  })

  observe({
    insp_yr <- insp_data()
    fidd_yr  <- fidd_data()

    cd_filled <- cd_sf |>
      left_join(insp_yr, by = c("CounDist" = "citycouncildistrict"))

    pal <- colorNumeric("YlOrRd", domain = cd_filled$rate, na.color = "#e0e0e0")

    cents <- merge(cd_centroids, fidd_yr,
                   by.x = "CounDist", by.y = "citycouncildistrict", all.x = TRUE)
    cents[is.na(fidd_rate), fidd_rate := 0]
    cents[, icon_size := scale_icon_size(fidd_rate)]

    fire_icons_scaled <- icons(
      iconUrl     = "https://cdn-icons-png.flaticon.com/512/785/785116.png",
      iconWidth   = cents$icon_size,
      iconHeight  = cents$icon_size,
      iconAnchorX = cents$icon_size / 2,
      iconAnchorY = cents$icon_size
    )

    source_label <- toupper(input$insp_source)

    leafletProxy("map") |>
      clearShapes() |>
      clearMarkers() |>
      clearControls() |>
      addPolygons(
        data        = cd_filled,
        fillColor   = ~pal(rate),
        fillOpacity = 0.7,
        color       = "transparent",
        weight      = 0,
        label       = ~paste0("CD ", CounDist, ": ",
                               round(replace_na(rate, 0), 3), " inspections/bldg",
                               " | ", replace_na(total_bldgs, "?"), " bldgs",
                               " | Med. built: ", replace_na(med_yearbuilt, "?"))
      ) |>
      addCouncilStyle(add_dists = TRUE, dist_year = "2013") |>
      addLegend(
        position = "bottomright",
        pal      = pal,
        values   = cd_filled$rate,
        title    = paste(source_label, "Inspections/Bldg", input$insp_year),
        layerId  = "legend"
      ) |>
      addMarkers(
        data  = cents,
        lng   = ~lon,
        lat   = ~lat,
        icon  = fire_icons_scaled,
        label = ~paste0("CD ", CounDist, ": ",
                         round(fidd_rate, 3), " fires/bldg (", input$fidd_year, ")",
                         " | ", replace_na(total_bldgs, "?"), " bldgs",
                         " | Med. built: ", replace_na(med_yearbuilt, "?"))
      )
  })
}

shinyApp(ui, server)

# Static presentation map -----------------------------------------------
mifc_map_year <- 2013
rbis_map_year <- 2022
fidd_map_year <- 2013

mifc_yr <- mifc_by_cd_year[year == mifc_map_year]
rbis_yr <- rbis_by_cd_year[year == rbis_map_year]
fidd_yr  <- fidd_by_cd_year[year == fidd_map_year]

cd_mifc <- cd_sf |> left_join(mifc_yr, by = c("CounDist" = "citycouncildistrict"))
cd_rbis <- cd_sf |> left_join(rbis_yr, by = c("CounDist" = "citycouncildistrict"))

pal_mifc <- colorNumeric("Blues", domain = cd_mifc$rate, na.color = "#e0e0e0")
pal_rbis <- colorNumeric("Blues", domain = cd_rbis$rate, na.color = "#e0e0e0")

cents_static <- merge(cd_centroids, fidd_yr,
                      by.x = "CounDist", by.y = "citycouncildistrict", all.x = TRUE)
cents_static[is.na(fidd_rate), fidd_rate := 0]
cents_static[, icon_size := scale_icon_size(fidd_rate)]

fire_icons_static <- icons(
  iconUrl     = "https://cdn-icons-png.flaticon.com/512/785/785116.png",
  iconWidth   = cents_static$icon_size,
  iconHeight  = cents_static$icon_size,
  iconAnchorX = cents_static$icon_size / 2,
  iconAnchorY = cents_static$icon_size
)

mifc_map <- leaflet() |>
  set_council_mapsize() |>
  add_council_basemaps() |>
  addPolygons(
    data        = cd_mifc,
    fillColor   = ~pal_mifc(rate),
    fillOpacity = 0.7,
    color       = "transparent",
    weight      = 0,
    label       = ~paste0("CD ", CounDist, ": ",
                           round(replace_na(rate, 0), 3), " MIFC inspections/bldg (",
                           mifc_map_year, ")",
                           " | ", replace_na(total_bldgs, "?"), " bldgs",
                           " | Med. built: ", replace_na(med_yearbuilt, "?")),
    group       = paste("MIFC", mifc_map_year)
  ) |>
  addCouncilStyle(add_dists = TRUE, dist_year = "2013") |>
  addMarkers(
    data  = cents_static,
    lng   = ~lon,
    lat   = ~lat,
    icon  = fire_icons_static,
    label = ~paste0("CD ", CounDist, ": ",
                    round(fidd_rate, 3), " fires/bldg (", fidd_map_year, ")",
                    " | ", replace_na(total_bldgs, "?"), " bldgs",
                    " | Med. built: ", replace_na(med_yearbuilt, "?")),
    group = paste("Fires", fidd_map_year)
  ) |>
  addLegend(
    position = "bottomright",
    pal      = pal_mifc,
    values   = cd_mifc$rate,
    title    = paste("MIFC Inspections/Bldg", mifc_map_year),
    group    = paste("MIFC", mifc_map_year)
  )

mapshot(mifc_map, file = "visuals/inspections_map_2013mifc_2013fidd.png")


rbis_map <- leaflet() |>
  set_council_mapsize() |>
  add_council_basemaps() |>
  addPolygons(
    data        = cd_rbis,
    fillColor   = ~pal_rbis(rate),
    fillOpacity = 0.7,
    color       = "transparent",
    weight      = 0,
    label       = ~paste0("CD ", CounDist, ": ",
                           round(replace_na(rate, 0), 3), " RBIS inspections/bldg (",
                           rbis_map_year, ")",
                           " | ", replace_na(total_bldgs, "?"), " bldgs",
                           " | Med. built: ", replace_na(med_yearbuilt, "?")),
    group       = paste("RBIS", rbis_map_year)
  ) |>
  addLegend(
    position = "bottomright",
    pal      = pal_rbis,
    values   = cd_rbis$rate,
    title    = paste("RBIS Inspections/Bldg", rbis_map_year),
    group    = paste("RBIS", rbis_map_year)
  ) |>
  addCouncilStyle(add_dists = TRUE, dist_year = "2013") |>
  addMarkers(
    data  = cents_static,
    lng   = ~lon,
    lat   = ~lat,
    icon  = fire_icons_static,
    label = ~paste0("CD ", CounDist, ": ",
                     round(fidd_rate, 3), " fires/bldg (", fidd_map_year, ")",
                     " | ", replace_na(total_bldgs, "?"), " bldgs",
                     " | Med. built: ", replace_na(med_yearbuilt, "?")),
    group = paste("Fires", fidd_map_year)
  )

mapshot(rbis_map, file = "visuals/inspections_map_2022rbis_2013fidd.png")

