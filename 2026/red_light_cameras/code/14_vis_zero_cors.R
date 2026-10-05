library(tidyverse)
library(sf)
library(purrr)
library(broom)
library(leaflet)


#### Load Datasets
rl_unique <- st_read("data/rl_unique.gpkg")
df_nta <- st_read("data/df_nta.gpkg")
vis_zero <- read_csv("data/intersection_intervention_table_final_w_geo.csv")

### Cleaning
vis_zero %>%
  filter(year == 2024) %>%
  filter(!is.na(intersection_geom)) %>%
  st_as_sf(wkt = "intersection_geom", crs = 2263) %>%
  st_transform(4326) %>%
  st_centroid() %>%
  # st_make_valid() %>%
  # st_point_on_surface() %>%
  leaflet() %>%
  addTiles() %>%
  addCircleMarkers()

vis_zero %>% filter(year == 2024) %>% filter(is.na(intersection_geom))

vis_zero_nta <- vis_zero %>%
  filter(year == 2024) %>%
  group_by(ntaname) %>%
  summarize(
    intersections = n(),
    leading_pedestrian_interval_sum = sum(leading_pedestrian_interval_post),
    turn_traffic_calming_sum = sum(turn_traffic_calming_post),
    enhanced_crossing_sum = sum(enhanced_crossing_post),
    signal_retiming_sum = sum(signal_retiming_post),
    speed_humps_sum = sum(speed_humps_post),
    street_improvement_corridors_sum = sum(street_improvement_corridors_post),
    street_improvement_project_sum = sum(street_improvement_project_post)
  ) %>%
  mutate(
    total_vis_zero = leading_pedestrian_interval_sum +
      turn_traffic_calming_sum +
      enhanced_crossing_sum +
      signal_retiming_sum +
      speed_humps_sum +
      street_improvement_corridors_sum +
      street_improvement_project_sum
  ) %>%
  mutate(vis_zero_per_int = total_vis_zero / intersections) %>%
  left_join(
    df_nta,
    by = c("ntaname" = "NTAName")
  ) %>%
  mutate(cameras_per_int = num_cams / intersections) %>%
  st_set_geometry(.$geom)

vis_zero_nta %>%
  ggplot(aes(fill = vis_zero_per_int)) +
  geom_sf() +
  theme_void()

vis_zero_nta %>%
  ggplot(aes(fill = cameras_per_int)) +
  geom_sf() +
  theme_void()

vis_zero_nta %>%
  ggplot(aes(cameras_per_int, vis_zero_per_int)) +
  geom_point() +
  geom_smooth(method = "lm")

glm(
  num_cams ~ leading_pedestrian_interval_sum +
    turn_traffic_calming_sum +
    enhanced_crossing_sum +
    signal_retiming_sum +
    speed_humps_sum +
    street_improvement_corridors_sum +
    street_improvement_project_sum +
    offset(log(intersections)),
  data = vis_zero_nta
) %>%
  summary()

vis_zero_cor_raw <- vis_zero_nta %>%
  mutate(
    leading_pedestrian_interval_per = leading_pedestrian_interval_sum /
      lights,
    turn_traffic_calming_per = turn_traffic_calming_sum / lights,
    enhanced_crossing_per = enhanced_crossing_sum / lights,
    signal_retiming_per = signal_retiming_sum / lights,
    speed_humps_per = speed_humps_sum / lights,
    street_improvement_corridors_per = street_improvement_corridors_sum /
      lights,
    street_improvement_project_per = street_improvement_project_sum /
      lights
  ) %>%
  filter(!is.na(cams_per_light)) %>%
  select(
    cams_per_light,
    leading_pedestrian_interval_per,
    turn_traffic_calming_per,
    enhanced_crossing_per,
    signal_retiming_per,
    speed_humps_per,
    street_improvement_corridors_per,
    street_improvement_project_per
  ) %>%
  st_drop_geometry()


cor.test(
  vis_zero_cor_raw$cams_per_light,
  vis_zero_cor_raw$leading_pedestrian_interval_per
) %>%
  tidy()
# cor.test(
#   vis_zero_cor_raw$cams_per_light,
#   vis_zero_cor_raw$turn_traffic_calming_per
# ) %>%
#   tidy()
cor.test(
  vis_zero_cor_raw$cams_per_light,
  vis_zero_cor_raw$enhanced_crossing_per
) %>%
  tidy()
cor.test(
  vis_zero_cor_raw$cams_per_light,
  vis_zero_cor_raw$signal_retiming_per
) %>%
  tidy()
cor.test(vis_zero_cor_raw$cams_per_light, vis_zero_cor_raw$speed_humps_per) %>%
  tidy()
cor.test(
  vis_zero_cor_raw$cams_per_light,
  vis_zero_cor_raw$street_improvement_corridors_per
) %>%
  tidy()
cor.test(
  vis_zero_cor_raw$cams_per_light,
  vis_zero_cor_raw$street_improvement_project_per
) %>%
  tidy()


vis_zero_cor_raw %>%
  pivot_longer(everything(), names_to = "intervention", values_to = "vals") %>%
  ggplot(aes(vals, intervention)) +
  geom_boxplot()
