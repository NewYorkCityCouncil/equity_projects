# Optional block-level geocoding. NOT required by the council-district analysis
# (02/03/04/report) — RBIS, MIFC, and FIDD all carry citycouncildistrict
# natively, so CD-level work needs none of this. Run this only for block-level
# analysis: it geocodes fires to blocks via alarm-box coordinates joined to the
# nearest PLUTO lot, and enriches the inspection data with PLUTO variables.
# Produces: fidd_with_block, and rbis_dedup/mifc_dedup augmented with
# nearest_borobox + PLUTO block columns.
source("code/01_data-preparation.R")

# Block-level geocoding: fidd → alarm box coordinates → MapPLUTO spatial join --------
mappluto_path <- "data/input/mappluto/MapPLUTO.shp"
mappluto_sf   <- st_read(mappluto_path, quiet = TRUE)

## load alarm box locations; deduplicate to one coordinate per borobox
alarmbox <- fread("https://data.cityofnewyork.us/resource/v57i-gtxb.csv?$limit=999999999",
                  select = c("borobox", "latitude", "longitude"))
alarmbox <- unique(alarmbox)

## convert alarmbox to sf and join to nearest PLUTO lot → each alarm box gets block + PLUTO vars
alarmbox_sf <- st_as_sf(alarmbox, coords = c("longitude", "latitude"), crs = 4326) |>
  st_transform(st_crs(mappluto_sf))

## spatial join: each alarm box point → nearest PLUTO lot
# NOTE: st_nearest_feature guarantees every geocoded point gets a block, but introduces
# measurement error — alarm boxes are at street intersections, so the "nearest" lot may
# not be the lot where the fire occurred. Blocks with many alarm boxes nearby will be
# over-represented; blocks with no nearby alarm boxes may absorb unrelated incidents.
# This is an acceptable approximation for block-level aggregation but not lot-level analysis.
alarmbox_pluto <- st_join(
  alarmbox_sf,
  mappluto_sf[, c("BBL", "BldgClass", "LandUse")],
  join = st_nearest_feature
)
alarmbox_pluto <- setDT(st_drop_geometry(alarmbox_pluto))
alarmbox_pluto[, pluto_block := substr(as.character(BBL), 2, 6)]

## construct borobox key in fidd from borough + alarm box number
boro_letter <- c(
  "MANHATTAN"                = "M",
  "BRONX"                    = "X",
  "BROOKLYN"                 = "B",
  "QUEENS"                   = "Q",
  "RICHMOND / STATEN ISLAND" = "R"
)
fidd_dedup[, borobox := paste0(boro_letter[alarm_box_borough], formatC(alarm_box_number, width = 4, flag = "0"))]

## join fidd to alarm boxes to get coordinates
fidd_with_block <- merge(
  fidd_dedup,
  alarmbox_pluto,
  by     = "borobox",
  all.x  = TRUE
)

# Enrich rbis/mifc with PLUTO variables + nearest alarm box -----------------------------------------------
## find nearest alarm box to each lot centroid, then pull PLUTO vars through that alarm box

## rbis: convert each row to sf using built-in cent_latitude/cent_longitude; assign nearest borobox directly
rbis_sf <- st_as_sf(
  rbis_dedup[!is.na(cent_latitude) & !is.na(cent_longitude)],
  coords = c("cent_longitude", "cent_latitude"), crs = 4326
) |> st_transform(st_crs(alarmbox_sf))

rbis_dedup[!is.na(cent_latitude) & !is.na(cent_longitude),
           nearest_borobox := alarmbox_sf$borobox[st_nearest_feature(rbis_sf, alarmbox_sf)]]

rbis_dedup <- merge(rbis_dedup, alarmbox_pluto, by.x = "nearest_borobox", by.y = "borobox", all.x = TRUE)

## mifc: convert each row to sf using latitude/longitude; assign nearest borobox directly
mifc_sf <- st_as_sf(
  mifc_dedup[!is.na(latitude) & !is.na(longitude)],
  coords = c("longitude", "latitude"), crs = 4326
) |> st_transform(st_crs(alarmbox_sf))

mifc_dedup[!is.na(latitude) & !is.na(longitude),
           nearest_borobox := alarmbox_sf$borobox[st_nearest_feature(mifc_sf, alarmbox_sf)]]

mifc_dedup <- merge(mifc_dedup, alarmbox_pluto, by.x = "nearest_borobox", by.y = "borobox", all.x = TRUE)

## compare block from raw bbl substring vs PLUTO block via nearest alarm box
rbis_dedup[, block_match := block == pluto_block]
mifc_dedup[, block_match := block == pluto_block]

# Percentage of matching
mean(rbis_dedup$block_match, na.rm = TRUE)
mean(mifc_dedup$block_match, na.rm = TRUE)
