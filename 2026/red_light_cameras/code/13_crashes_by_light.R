library(tidyverse)
library(vroom)
library(jsonlite)
library(sf)
library(leaflet)

url2 <- "https://data.cityofnewyork.us/resource/h9gi-nx95.csv?$where=crash_date%20between%20'2020-01-01T00:00:00.000'%20and%20'2025-01-01T00:00:00.000'&$limit=9999999999999999"

crash_raw_historic <- read_csv(url2) %>%
  mutate(
    year = format(crash_date, format = "%Y"),
    month = format(crash_date, format = "%m")
  ) %>%
  filter(!is.na(latitude)) %>%
  st_as_sf(coords = c("longitude", "latitude")) %>%
  st_set_crs(4326)

nta_demos <- st_read("data/df_nta.gpkg") %>%
  select(perc_nhw, perc_nhb, perc_nha, perc_his, perc_oth)

crash_historic <- crash_raw_historic %>%
  mutate(
    contributing_factor_vehicle_1 = replace_na(
      contributing_factor_vehicle_1,
      ""
    ),
    contributing_factor_vehicle_2 = replace_na(
      contributing_factor_vehicle_2,
      ""
    )
  ) %>%
  mutate(
    related_crash = contributing_factor_vehicle_1 ==
      "Traffic Control Disregarded" |
      contributing_factor_vehicle_2 == "Traffic Control Disregarded"
  )


rl_dates <- st_read("data/rl_unique.gpkg") %>%
  mutate(
    cap_date = if_else(
      first_appearance < ymd("2025-12-31"),
      ymd("2025-12-31"),
      first_appearance
    )
  ) %>%
  mutate(months = format(cap_date, format = "%Y-%m"))

rl_dates$all_light_crashes <- lengths(
  st_is_within_distance(
    rl_dates,
    crash_historic,
    dist = units::set_units(15.24, "m")
  )
)

rl_dates$rel_light_crashes <- lengths(
  st_is_within_distance(
    rl_dates,
    crash_historic %>% filter(related_crash),
    dist = units::set_units(15.24, "m")
  )
)

rl_combine <- rl_dates %>%
  st_join(nta_demos) %>%
  mutate(
    largest_group = names(across(c(
      perc_nha,
      perc_nhb,
      perc_nhw,
      perc_his,
      perc_oth
    )))[
      max.col(
        across(c(perc_nha, perc_nhb, perc_nhw, perc_his, perc_oth)),
        ties.method = "first"
      )
    ]
  )

rl_combine %>%
  ggplot(aes(rel_light_crashes)) +
  geom_histogram()


pal_crash_by_light <- colorNumeric(
  palette = "viridis",
  domain = rl_combine$rel_light_crashes
)

leaflet(rl_combine) %>%
  addTiles() %>%
  addCircleMarkers(
    color = ~ pal_crash_by_light(rl_combine$rel_light_crashes),
    popup = paste0(
      rl_combine$full_street,
      " with ",
      rl_combine$rel_light_crashes,
      " crashes"
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal_crash_by_light,
    values = rl_combine$rel_light_crashes,
    title = "Crashes",
    opacity = 1
  )


rl_combine %>%
  ggplot(aes(perc_nhb, rel_light_crashes)) +
  geom_point()


lm(
  rel_light_crashes ~ perc_nhb + perc_his + perc_oth + perc_nha,
  data = rl_combine
) %>%
  summary()

MASS::glm.nb(
  rel_light_crashes ~
    perc_nhb + perc_his + perc_oth + perc_nha,
  data = rl_combine
) %>%
  summary()

rl_combine %>%
  ggplot(aes(largest_group, rel_light_crashes, color = largest_group)) +
  geom_point(position = position_jitterdodge())


rl_combine %>%
  ggplot(aes(largest_group, rel_light_crashes, color = largest_group)) +
  geom_boxplot(varwidth = TRUE)


##################
### All crashes

rl_combine %>%
  ggplot(aes(all_light_crashes)) +
  geom_histogram()

pal_crash_by_light <- colorNumeric(
  palette = "viridis",
  domain = rl_combine$all_light_crashes
)

leaflet(rl_combine) %>%
  addTiles() %>%
  addCircleMarkers(
    color = ~ pal_crash_by_light(rl_combine$all_light_crashes),
    popup = paste0(
      rl_combine$full_street,
      " with ",
      rl_combine$all_light_crashes,
      " crashes"
    )
  ) %>%
  addLegend(
    "bottomright",
    pal = pal_crash_by_light,
    values = rl_combine$all_light_crashes,
    title = "Crashes",
    opacity = 1
  )


rl_combine %>%
  ggplot(aes(perc_nhb, all_light_crashes)) +
  geom_point()


lm(
  all_light_crashes ~ perc_nhb + perc_his + perc_oth + perc_nha,
  data = rl_combine
) %>%
  summary()


MASS::glm.nb(
  all_light_crashes ~
    perc_nhb + perc_his + perc_oth + perc_nha,
  data = rl_combine
) %>%
  summary()

rl_combine %>%
  ggplot(aes(largest_group, all_light_crashes, color = largest_group)) +
  geom_point(position = position_jitterdodge())

rl_combine %>%
  ggplot(aes(largest_group, all_light_crashes, color = largest_group)) +
  geom_boxplot(varwidth = TRUE)
