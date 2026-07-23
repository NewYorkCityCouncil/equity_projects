## Steps ---------------------------------------------------------

# 1. Load dependencies.
# 2. Read PUMA-level births, seats, and vacancies.
# 3. Keep the columns used downstream.
# 4. Save model input for the ACS merge.

source("00_load_dependencies.R")

cap_pums <- fread("data/actual_births_actual_seats_puma.csv")

cap_pums <- cap_pums[
  ,
  .(
    puma = sprintf("%04d", as.integer(puma)),
    total_seats,
    total_vacancy,
    tot_births_22 = births_puma,
    pre_sch_age = births_puma_2yr
  )
]

fwrite(cap_pums, "data/actual_births_seats.csv")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# #
# #                             ---- THIS IS THE END! ----
# #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
