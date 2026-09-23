## Steps ---------------------------------------------------------

# 1. Load dependencies.
# 2. Download ACS PUMS race/ethnicity and income data.
# 3. Aggregate PUMS data to NYC PUMAs.
# 4. Merge demographics to ECE births and seats.
# 5. Save model-ready data.

source("00_load_dependencies.R")

census_key <- Sys.getenv("CENSUS_API_KEY")
if (nzchar(census_key)) {
  census_api_key(census_key, install = FALSE, overwrite = TRUE)
}

# download race/ethn/HH income
pums <- get_pums(
  variables = c(
    "PUMA",
    "PWGTP",
    "AGEP",
    "RAC1P",
    "HISP",
    "PINCP",
    "HINCP"
  ),
  state = "NY",
  survey = "acs5",
  year = 2024,
  recode = FALSE
)

setDT(pums)
pumas <- st_read(
  "data/puma_nyc.geojson",
  quiet = TRUE
)

nyc_pumas <- sprintf(
  "%04d",
  as.integer(unique(pumas$puma))
)
pums[
  ,
  puma := str_sub(
    str_pad(
      as.character(PUMA),
      width = 5,
      pad = "0"
    ),
    -4
  )
]
pums <- pums[
  puma %in% nyc_pumas
]



setnames(
  pums,
  old = c(
    "PWGTP",
    "AGEP",
    "RAC1P",
    "HISP",
    "PINCP",
    "HINCP"
  ),
  new = c(
    "person_weight",
    "age",
    "race",
    "hispanic_origin",
    "personal_income",
    "household_income"
  ),
  skip_absent = TRUE
)

# convert downloaded PUMS vars to numeric
pums[
  ,
  `:=`(
    person_weight = as.numeric(person_weight),
    age = as.numeric(age),
    race = as.integer(race),
    hispanic_origin = as.integer(hispanic_origin),
    personal_income = as.numeric(personal_income),
    household_income = as.numeric(household_income)
  )
]


pums[
  ,
  race_ethnicity := fcase(
    hispanic_origin > 1, "Hispanic",
    race == 1, "White",
    race == 2, "Black",
    race == 6, "Asian",
    race %in% c(3, 4, 5, 7), "AIAN_NHPI",
    race %in% c(8, 9), "Other_2plus",
    default = NA_character_
  )
]

puma_race <- pums[
  ,
  .(
    population = sum(person_weight, na.rm = TRUE),
    
    white_nh = sum(person_weight * (race_ethnicity == "White"), na.rm = TRUE),
    black_nh = sum(person_weight * (race_ethnicity == "Black"), na.rm = TRUE),
    hispanic = sum(person_weight * (race_ethnicity == "Hispanic"), na.rm = TRUE),
    asian_nh = sum(person_weight * (race_ethnicity == "Asian"), na.rm = TRUE),
    aian_nhpi = sum(person_weight * (race_ethnicity == "AIAN_NHPI"), na.rm = TRUE),
    other_2plus = sum(person_weight * (race_ethnicity == "Other_2plus"), na.rm = TRUE)
  ),
  by = puma
]

puma_race[
  ,
  `:=`(
    pct_white_nh = white_nh / population,
    pct_black_nh = black_nh / population,
    pct_hispanic = hispanic / population,
    pct_asian_nh = asian_nh / population,
    pct_aian_nhpi = aian_nhpi / population,
    pct_other_2plus = other_2plus / population,
    pct_other_2plus_combined = (aian_nhpi + other_2plus) / population
  )
]



hh <- unique(
  pums[
    !grepl("GQ", SERIALNO) &
      !is.na(household_income) &
      household_income >= 0,
    .(
      SERIALNO,
      puma,
      household_weight = as.numeric(WGTP),
      household_income
    )
  ],
  by = "SERIALNO"
)

puma_income <- hh[
  ,
  .(
    households = sum(household_weight, na.rm = TRUE),
    mean_household_income = weighted.mean(
      household_income,
      household_weight,
      na.rm = TRUE
    ),
    median_household_income = weighted_median(
      household_income,
      household_weight
    )
  ),
  by = puma
]

# ------------------------------------------------------------
# Final PUMA-level file
# ------------------------------------------------------------

puma_demo_income <- merge(
  puma_race,
  puma_income,
  by = "puma",
  all = TRUE
)

fwrite(
  puma_demo_income,
  "data/acs5_2024_pums_nyc_puma_race_ethnicity_household_income.csv"
)

# puma_demo_income[]
# str(puma_demo_income)
bs <- fread("data/actual_births_seats.csv")
bs[, puma := as.character(puma)]
bs_demos <- merge(bs, puma_demo_income, by = "puma")
# length(unique(bs_demos$puma))
fwrite(bs_demos, "data/seats_race_income_puma-2024.csv")
dtb <- bs_demos
dtb[
  ,
  `:=`(
    puma = sprintf("%04d", as.integer(puma)),
    
    seats_per_child =
      total_seats / pre_sch_age,
    
    vacancy_rate =
      total_vacancy / total_seats,
    
    median_inc_10k =
      median_household_income / 10000,
    
    pct_hispanic10 =
      pct_hispanic * 10,
    
    pct_black10 =
      pct_black_nh * 10,
    
    pct_asian10 =
      pct_asian_nh * 10,
    
    pct_aian_nhpi10 =
      pct_aian_nhpi * 10,
    
    pct_other_2plus10 =
      pct_other_2plus * 10,
    
    pct_other_2plus_combined10 =
      pct_other_2plus_combined * 10
  )
]
fwrite(dtb, "data/lm_model_data.csv")
