# One-off: build data/output/prepared.rds from the cached raw CSVs in
# all_data.RData, reproducing 01_data-preparation.R's core transforms without
# the Socrata downloads. Demographics still come from the councilcount API.
# Run once; afterwards 02_plots.R loads the cache and the report knits offline.
setwd("/Users/JWu/Desktop/JWu_Projects/equity_projects/2026/fdny_rbis")
source("code/00_load_dependencies.R")
load("all_data.RData")   # rbis_csv, mifc_csv, fidd_csv (raw)

bldgs_by_cd <- fread("data/input/mappluto_annual_by-cd_2005-2025.csv")

## fidd: structural fires only
fidd_csv <- fidd_csv[incident_classification_group == "Structural Fires"]

## Date standardization (mirrors 01)
rbis_csv[, date := as.Date(insp_inspect_dt)]
rbis_csv[, `:=`(year = year(date), month = month(date), quarter = quarter(date))]
mifc_csv[, date := as.Date(insp_inspect_dt_fk)]
mifc_csv[, `:=`(year = year(date), month = month(date), quarter = quarter(date))]
fidd_csv[, date := as.Date(incident_datetime)]
fidd_csv[, `:=`(year = year(date), month = month(date), quarter = quarter(date))]

## Geographic standardization (mirrors 01)
boro_from_full <- c(
  "BRONX" = "BX", "BROOKLYN" = "BK", "MANHATTAN" = "MN",
  "QUEENS" = "QN", "RICHMOND / STATEN ISLAND" = "SI", "STATEN ISLAND" = "SI"
)
rbis_csv[, boro := borough]; rbis_csv[, block := substr(as.character(bbl), 2, 6)]
mifc_csv[, boro := borough]; mifc_csv[, block := substr(as.character(bbl), 2, 6)]
fidd_csv[, boro := boro_from_full[incident_borough]]

## Deduplication
rbis_dedup <- unique(rbis_csv)
mifc_dedup <- unique(mifc_csv)
fidd_dedup <- unique(fidd_csv)
rbis_start <- as.Date("2014-01-01")

## Demographics (councilcount API; virtualenv already exists)
use_virtualenv("councilcount_demo", required = TRUE)
cc <- import("councilcount")
councilcount_codes <- cc$get_available_councilcount_codes(acs_year = '2023')
demo_vars <- councilcount_codes[179:185, ]
pe_codes  <- sub("E$", "PE", demo_vars$estimate_var_code)

fetch_demo <- function(boundary_year) {
  cc$get_councilcount_estimates(acs_year = 2023, geo = "councildist",
      boundary_year = boundary_year,
      var_codes = demo_vars |> pull(estimate_var_code)) |>
    dplyr::select(1, any_of(demo_vars$estimate_var_code), any_of(pe_codes)) |>
    rename_with(~ paste0(demo_vars$estimate_description[match(.x, demo_vars$estimate_var_code)], "_num"),
                .cols = any_of(demo_vars$estimate_var_code)) |>
    rename_with(~ paste0(demo_vars$estimate_description[match(sub("PE$", "E", .x), demo_vars$estimate_var_code)], "_percent"),
                .cols = any_of(pe_codes))
}
demographies_by_cd_2023 <- fetch_demo(2023)
demographies_by_cd_2013 <- fetch_demo(2013)

## Save cache (same object set 02_plots.R expects)
prepared_objs <- c("rbis_dedup", "mifc_dedup", "fidd_dedup", "bldgs_by_cd",
                   "demographies_by_cd_2013", "demographies_by_cd_2023", "rbis_start")
dir.create("data/output", showWarnings = FALSE, recursive = TRUE)
saveRDS(mget(prepared_objs), "data/output/prepared.rds")
cat("Saved data/output/prepared.rds with:", paste(prepared_objs, collapse = ", "), "\n")
cat("file size:", round(file.size("data/output/prepared.rds") / 1e6, 1), "MB\n")
cat("DONE\n")
