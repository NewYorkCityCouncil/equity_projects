source("code/00_load_dependencies.R")

unzip_sf <- function(url) {
  tmp   <- tempfile(fileext = ".zip")
  exdir <- tempfile()
  download.file(url, tmp, quiet = TRUE, mode = "wb")
  unzip(tmp, exdir = exdir)
  # some years nest per-borough zips inside the main zip — unzip those too
  nested_zips <- list.files(exdir, pattern = "\\.zip$", full.names = TRUE, recursive = TRUE)
  for (z in nested_zips) unzip(z, exdir = exdir)
  shps <- list.files(exdir, pattern = "MapPLUTO\\.shp$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  if (length(shps) == 1) {
    st_read(shps, quiet = TRUE)
  } else {
    bind_rows(lapply(shps, st_read, quiet = TRUE))
  }
}

# Load in data -----------------------------------------------

## Risk Based Inspection Data
url <- "https://data.cityofnewyork.us/resource/itd7-gx3g.csv?$limit=999999999"
rbis_csv <- fread(url)

## Mandatory Inspections by Fire Companies
url <- "https://data.cityofnewyork.us/resource/kfgh-h6re.csv?$limit=999999999"
mifc_csv <- fread(url)

## Fire Incident Dispatch Data
url <- "https://data.cityofnewyork.us/resource/8m42-w767.csv?$limit=999999999"
fidd_csv <- fread(url)

## Buildings per CD per year — from cached file or recomputed from MapPLUTO
bldgs_by_cd_path <- "data/input/mappluto_annual_by-cd_2005-2025.csv"

if (file.exists(bldgs_by_cd_path)) {
  bldgs_by_cd <- fread(bldgs_by_cd_path)
} else {
  mappluto_url_2025 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/nyc_mappluto_25v3_1_arc_shp.zip"
  mappluto_url_2024 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/nyc_mappluto_24v4_1_arc_shp.zip"
  mappluto_url_2023 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/nyc_mappluto_23v3_1_arc_shp.zip"
  mappluto_url_2022 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/nyc_mappluto_22v2_arc_shp.zip"
  mappluto_url_2021 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/nyc_mappluto_21v3_arc_shp.zip"
  mappluto_url_2020 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/nyc_mappluto_20v8_arc_shp.zip"
  mappluto_url_2019 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/nyc_mappluto_19v2_arc_shp.zip"
  mappluto_url_2018 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/nyc_mappluto_18v2_1_arc_shp.zip"
  mappluto_url_2017 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_17v1_1.zip"
  mappluto_url_2016 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_16v2.zip"
  mappluto_url_2015 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_15v1.zip"
  mappluto_url_2014 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_14v2.zip"
  mappluto_url_2013 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_13v2.zip"
  mappluto_url_2012 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_12v2.zip"
  mappluto_url_2011 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_11v2.zip"
  mappluto_url_2010 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_10v2.zip"
  mappluto_url_2009 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_09v2.zip"
  mappluto_url_2008 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_08b.zip"
  mappluto_url_2007 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_07c.zip"
  mappluto_url_2006 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_06c.zip"
  mappluto_url_2005 <- "https://s-media.nyc.gov/agencies/dcp/assets/files/zip/data-tools/bytes/mappluto/mappluto_05d.zip"

  mappluto_urls <- list(
    "2005" = mappluto_url_2005, "2006" = mappluto_url_2006, "2007" = mappluto_url_2007,
    "2008" = mappluto_url_2008, "2009" = mappluto_url_2009, "2010" = mappluto_url_2010,
    "2011" = mappluto_url_2011, "2012" = mappluto_url_2012, "2013" = mappluto_url_2013,
    "2014" = mappluto_url_2014, "2015" = mappluto_url_2015, "2016" = mappluto_url_2016,
    "2017" = mappluto_url_2017, "2018" = mappluto_url_2018, "2019" = mappluto_url_2019,
    "2020" = mappluto_url_2020, "2021" = mappluto_url_2021, "2022" = mappluto_url_2022,
    "2023" = mappluto_url_2023, "2024" = mappluto_url_2024, "2025" = mappluto_url_2025
  )

  bldgs_by_cd <- rbindlist(lapply(names(mappluto_urls), function(yr) {
    message("Loading PLUTO: ", yr)
    sf  <- unzip_sf(mappluto_urls[[yr]])
    message("  nrow: ", nrow(sf))
    dt  <- as.data.table(st_drop_geometry(sf))
    message("  cols: ", paste(intersect(c("Council","NumBldgs"), names(dt)), collapse = ", "))
    dt  <- dt[!is.na(Council) & NumBldgs > 0,
              .(total_bldgs   = sum(as.numeric(NumBldgs), na.rm = TRUE),
                med_yearbuilt  = median(as.numeric(YearBuilt)[as.numeric(YearBuilt) > 0], na.rm = TRUE),
                mean_yearbuilt = mean(as.numeric(YearBuilt)[as.numeric(YearBuilt) > 0],   na.rm = TRUE)),
              by = .(citycouncildistrict = as.integer(Council))]
    dt[, year := as.integer(yr)]
    message("  done: ", nrow(dt), " CDs")
    dt
  }))

  fwrite(bldgs_by_cd, bldgs_by_cd_path)
  rm(mappluto_urls,
     mappluto_url_2005, mappluto_url_2006, mappluto_url_2007, mappluto_url_2008,
     mappluto_url_2009, mappluto_url_2010, mappluto_url_2011, mappluto_url_2012,
     mappluto_url_2013, mappluto_url_2014, mappluto_url_2015, mappluto_url_2016,
     mappluto_url_2017, mappluto_url_2018, mappluto_url_2019, mappluto_url_2020,
     mappluto_url_2021, mappluto_url_2022, mappluto_url_2023, mappluto_url_2024,
     mappluto_url_2025)
}

# EDA -----------------------------------------------
range(rbis_csv$insp_inspect_dt)
range(mifc_csv$insp_inspect_dt_fk)
range(fidd_csv$incident_datetime)

# Filter to structural building inspections -----------------------------------------------

# See if there are any patterns to the matching inspections btw
# rbis and mifc
rbis_bin_date <- rbis_csv[, paste(bldg_current_bin_fk, as.Date(insp_inspect_dt))]
mifc_csv_test <- mifc_csv[paste(bldg_current_bin, as.Date(insp_inspect_dt_fk)) %in% rbis_bin_date]
View(mifc_csv_test)

# NOTE: How does mifc not include all of RBIS' inspections? Also, rbis data
# specifically says it doesn't include Construction, Demolition & Abatement or other mandatory inspections,
# yet we see some here?
# ACTION: Do not filter at all

### mifc: keep building/structural types; excludes drills, schools, transit, batteries, admin
#mifc_structural_types <- c(
#  "STRUC_INT", "STRUC_INT_1", "STRUC_INT_2", "STRUC_INT_3",
#  "STRUC_INT_4", "STRUC_INT_5", "STRUC_INT_6",
#  "CONSTR", "ALT", "COFO", "DEMO",
#  "COM_OCPCY", "COMPLAINT", "INSPECT", "STANDARD",
#  "VACATE_SURV", "INC-INSPECT"
#)

#mifc_csv <- mifc_csv[insptn_typ_cd_fk %in% mifc_structural_types]

## fidd: keep structural fires only
fidd_csv <- fidd_csv[incident_classification_group == "Structural Fires"]

# Date standardization + time variables -----------------------------------------------

## rbis
rbis_csv[, date    := as.Date(insp_inspect_dt)]
rbis_csv[, year    := year(date)]
rbis_csv[, month   := month(date)]
rbis_csv[, quarter := quarter(date)]

## mifc
mifc_csv[, date    := as.Date(insp_inspect_dt_fk)]
mifc_csv[, year    := year(date)]
mifc_csv[, month   := month(date)]
mifc_csv[, quarter := quarter(date)]

## fidd
fidd_csv[, date    := as.Date(incident_datetime)]
fidd_csv[, year    := year(date)]
fidd_csv[, month   := month(date)]
fidd_csv[, quarter := quarter(date)]

# Geographic standardization -----------------------------------------------
# boro: consistent 2-letter code; block: from bbl (rbis/mifc);
# fidd has no block-level identifier
# councildistrict: consistent column name (city council district)

boro_from_full <- c(
  "BRONX"                    = "BX",
  "BROOKLYN"                 = "BK",
  "MANHATTAN"                = "MN",
  "QUEENS"                   = "QN",
  "RICHMOND / STATEN ISLAND" = "SI",
  "STATEN ISLAND"            = "SI"
)

## rbis: borough already 2-letter; extract block from bbl
rbis_csv[, boro  := borough]
rbis_csv[, block := substr(as.character(bbl), 2, 6)]

## mifc: same as rbis
mifc_csv[, boro  := borough]
mifc_csv[, block := substr(as.character(bbl), 2, 6)]

## fidd: normalize full borough name; no block available
fidd_csv[, boro           := boro_from_full[incident_borough]]

# Deduplication -----------------------------------------------
rbis_dedup <- unique(rbis_csv)
mifc_dedup <- unique(mifc_csv)
fidd_dedup <- unique(fidd_csv)

# EDA: rbis vs mifc inspection counts over time -----------------------------------------------
rbis_start <- as.Date("2014-01-01")

rbis_monthly <- rbis_dedup[, .(n = .N), by = .(year, month)][, source := "RBIS"]
mifc_monthly  <- mifc_dedup[, .(n = .N), by = .(year, month)][, source := "MIFC"]
monthly_counts <- rbind(rbis_monthly, mifc_monthly)
monthly_counts[, date := as.Date(paste(year, month, "01", sep = "-"))]

ggplot(monthly_counts, aes(x = date, y = n, color = source)) +
  geom_line() +
  geom_vline(xintercept = as.numeric(rbis_start), linetype = "dashed") +
  labs(title = "Monthly inspections: RBIS vs MIFC", x = NULL, y = "Inspections", color = NULL) +
  theme_minimal()

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

## DEMOGRAPHIC VARIABLES
# Create a clean environment
virtualenv_create("councilcount_demo",version="3.13")

virtualenv_install("councilcount_demo", "councilcount")
virtualenv_install("councilcount_demo", "pandas")
virtualenv_install("councilcount_demo", "geopandas")
virtualenv_install("councilcount_demo", "dotenv")
use_virtualenv("councilcount_demo", required = TRUE)

cc <- import("councilcount")
py_config()
cc$available_years()
councilcount_codes <- cc$get_available_councilcount_codes(acs_year='2023')
demo_vars <- councilcount_codes[179:185,]
pe_codes <- sub("E$", "PE", demo_vars$estimate_var_code)

demographies_by_cd_2023 <- cc$get_councilcount_estimates(acs_year = 2023,
                                        geo = "councildist",
                                        boundary_year = 2023,
                                        var_codes = demo_vars |> pull(estimate_var_code)) |>
  dplyr::select(1, any_of(demo_vars$estimate_var_code), any_of(pe_codes)) |>
  rename_with(
    ~ paste0(demo_vars$estimate_description[match(.x, demo_vars$estimate_var_code)], "_num"),
    .cols = any_of(demo_vars$estimate_var_code)
  ) |>
  rename_with(
    ~ paste0(demo_vars$estimate_description[match(sub("PE$", "E", .x), demo_vars$estimate_var_code)], "_percent"),
    .cols = any_of(pe_codes)
  )

demographies_by_cd_2013 <- cc$get_councilcount_estimates(acs_year = 2023,
                                             geo = "councildist",
                                             boundary_year = 2013,
                                             var_codes = demo_vars |> pull(estimate_var_code)) |>
  dplyr::select(1, any_of(demo_vars$estimate_var_code), any_of(pe_codes)) |>
  rename_with(
    ~ paste0(demo_vars$estimate_description[match(.x, demo_vars$estimate_var_code)], "_num"),
    .cols = any_of(demo_vars$estimate_var_code)
  ) |>
  rename_with(
    ~ paste0(demo_vars$estimate_description[match(sub("PE$", "E", .x), demo_vars$estimate_var_code)], "_percent"),
    .cols = any_of(pe_codes)
  )

# Clean up intermediates — keep only analysis-ready datasets -----------------------------------------------
rm(url,
   rbis_csv, mifc_csv, fidd_csv,
   rbis_bin_date, mifc_csv_test,
   rbis_monthly, mifc_monthly, monthly_counts,
   boro_from_full, boro_letter,
   bldgs_by_cd_path,
   mappluto_sf, mappluto_path,
   alarmbox, alarmbox_sf, alarmbox_pluto,
   fidd_dedup, rbis_sf, mifc_sf,
   demo_vars, pe_codes)


# =============================================================================
# IRFC CODE — COMMENTED OUT
# irfc (Incidents Responded to by Fire Companies, tm6d-hbzd) was explored as
# an alternative fire incident source with address-level data for block geocoding.
# However, a large share of fidd structural fires (~65% within the shared date
# range) have no matching irfc record, even when matching on date + borough +
# alarm box number. The source of this discrepancy is unclear — possible causes
# include different system coverage, cancelled dispatches, or data pipeline
# differences between StarFire CAD exports. Keeping irfc code here for reference
# pending further clarification.
# =============================================================================

# ## Incidents Responded to by Fire Companies (has street address for block geocoding)
# url <- "https://data.cityofnewyork.us/resource/tm6d-hbzd.csv?$limit=999999999"
# irfc_csv <- fread(url)
#
# ## irfc: keep building-related fire types
# # 111-112 = structural fires (matches fidd "Structural Fires"); 113-118 = confined building
# # fires (cooking, chimney, boiler, etc.) — 113 cooking fires dominate (222k) and are minor
# # contained incidents, may want to narrow to 111-112 for severity analysis
# irfc_structural_types <- c("111", "112", "113", "114", "115", "116", "117", "118")
# irfc_csv <- irfc_csv[substr(incident_type_desc, 1, 3) %in% irfc_structural_types]
#
# # secondary filter: NFIRS property use 100-899 = buildings; 900-999 = outside/special;
# # 000/UUU = unknown — excludes streets, open land, railroad, undetermined
# irfc_csv <- irfc_csv[
#   suppressWarnings(as.numeric(substr(property_use_desc, 1, 3))) %between% c(100, 899)
# ]
#
# ## PLUTO (Primary Land Use Tax Lot Output) — bbl, address, building class
# url <- "https://data.cityofnewyork.us/resource/64uk-42ks.csv?$limit=999999999"
# pluto_csv <- fread(url)
#
# ## keep only PLUTO columns useful for characterizing fire risk context
# pluto_slim <- pluto_csv[, .(borocode, zipcode, address,
#                             bldgclass, landuse, council,
#                             unitsres, unitstotal,
#                             numfloors, yearbuilt, bldgarea)]
# pluto_slim[, block := substr(as.character(pluto_csv$bbl), 2, 6)]
#
# ## normalize street names to abbreviated form (irfc uses abbrevs; PLUTO uses full words)
# normalize_street <- function(x) {
#   x <- toupper(trimws(x))
#   x <- gsub("\\bSTREET\\b",    "ST",   x)
#   x <- gsub("\\bAVENUE\\b",    "AVE",  x)
#   x <- gsub("\\bBOULEVARD\\b", "BLVD", x)
#   x <- gsub("\\bPLACE\\b",     "PL",   x)
#   x <- gsub("\\bROAD\\b",      "RD",   x)
#   x <- gsub("\\bDRIVE\\b",     "DR",   x)
#   x <- gsub("\\bCOURT\\b",     "CT",   x)
#   x <- gsub("\\bLANE\\b",      "LN",   x)
#   x <- gsub("\\bPARKWAY\\b",   "PKWY", x)
#   x <- gsub("\\bTERRACE\\b",   "TER",  x)
#   x
# }
#
# pluto_slim[, street_norm := normalize_street(trimws(gsub("^[0-9]+(-[0-9]+)?\\s+", "", address)))]
# irfc_csv[,   street_norm := normalize_street(street_highway)]
# irfc_csv[,   borocode    := as.integer(substr(borough_desc, 1, 1))]
# irfc_csv[,   zip_code    := as.integer(zip_code)]
#
# pluto_block <- pluto_slim[, .(
#   n_lots    = .N,
#   bldgclass = names(sort(table(bldgclass), decreasing = TRUE))[1],
#   landuse   = names(sort(table(landuse),   decreasing = TRUE))[1],
#   council   = names(sort(table(council),   decreasing = TRUE))[1],
#   unitsres  = sum(as.numeric(unitsres),   na.rm = TRUE),
#   unitstotal = sum(as.numeric(unitstotal), na.rm = TRUE),
#   bldgarea  = sum(as.numeric(bldgarea),   na.rm = TRUE),
#   numfloors = median(as.numeric(numfloors),                            na.rm = TRUE),
#   yearbuilt = median(as.numeric(yearbuilt)[as.numeric(yearbuilt) > 0], na.rm = TRUE)
# ), by = .(borocode, block, zipcode, street_norm)]
#
# mod_irfc <- merge(
#   irfc_csv,
#   pluto_block,
#   by.x = c("borocode", "zip_code", "street_norm"),
#   by.y = c("borocode", "zipcode",  "street_norm"),
#   all.x = TRUE,
#   allow.cartesian = TRUE
# )
#
# irfc_csv[,  c("borocode", "street_norm") := NULL]
# mod_irfc[,  c("borocode", "street_norm") := NULL]
#
# mod_irfc[, date    := as.Date(incident_date_time)]
# mod_irfc[, year    := year(date)]
# mod_irfc[, month   := month(date)]
# mod_irfc[, quarter := quarter(date)]
#
# boro_from_num <- c("1" = "MN", "2" = "BX", "3" = "BK", "4" = "QN", "5" = "SI")
# mod_irfc[, boro           := boro_from_num[substr(borough_desc, 1, 1)]]
# mod_irfc[, councildistrict := council]
#
# mod_irfc_dedup <- unique(mod_irfc)
#
# # EDA - irfc vs fidd structural fire matching
# fidd_eda <- fread("https://data.cityofnewyork.us/resource/8m42-w767.csv?$limit=999999999")
# fidd_eda <- fidd_eda[incident_classification_group == "Structural Fires"]
# irfc_eda <- fread("https://data.cityofnewyork.us/resource/tm6d-hbzd.csv?$limit=999999999")
# irfc_date_range <- range(as.POSIXct(irfc_eda$incident_date_time), na.rm = TRUE)
# fidd_eda <- fidd_eda[as.POSIXct(incident_datetime) %between% irfc_date_range]
# boro_map_eda <- c(
#   "1 - Manhattan"     = "MANHATTAN",
#   "2 - Bronx"         = "BRONX",
#   "3 - Brooklyn"      = "BROOKLYN",
#   "4 - Queens"        = "QUEENS",
#   "5 - Staten Island" = "RICHMOND / STATEN ISLAND"
# )
# irfc_eda[, borough_norm := boro_map_eda[borough_desc]]
# fidd_keys_eda <- fidd_eda[, paste(as.Date(incident_datetime),  incident_borough, alarm_box_number)]
# irfc_matched  <- irfc_eda[paste(as.Date(incident_date_time), borough_norm, fire_box) %in% fidd_keys_eda]
# fidd_eda[, join_key := paste(as.Date(incident_datetime),  incident_borough, alarm_box_number)]
# irfc_eda[, join_key := paste(as.Date(incident_date_time), borough_norm,     fire_box)]
# eda_merged <- merge(irfc_eda, fidd_eda, by = "join_key", all = FALSE)
# eda_merged[, join_key := NULL]
# View(eda_merged)
# eda_merged[, .N, by = incident_type_desc][order(-N)]
# eda_merged[, .N, by = property_use_desc][order(-N)]
# datetime_mismatch <- eda_merged[as.POSIXct(incident_datetime) != as.POSIXct(incident_date_time)]
# View(datetime_mismatch)
# fidd_unmatched <- fidd_eda[!join_key %in% irfc_eda$join_key]
# irfc_unmatched <- irfc_eda[!join_key %in% fidd_eda$join_key]
# View(head(fidd_unmatched, 100))
# View(head(irfc_unmatched, 100))
