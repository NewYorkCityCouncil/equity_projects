library(ggplot2)

################################################################################
#  prep data
################################################################################

combined_sf = readRDS("data/spatial_panel.csv")

boundaries = st_read("https://data.cityofnewyork.us/api/v3/views/gthc-hcne/query.geojson") %>% 
  st_transform(st_crs(combined_sf))

mapping = combined_sf %>% st_intersection(boundaries)


################################################################################
#  quick EFAI map
################################################################################

ggplot(mapping) + 
  geom_sf(aes(fill=index), col="transparent") + 
  theme_map() + 
  scale_fill_continuous(labels = scales::percent) +
  labs(fill = "EFAI")

ggplot(mapping) + 
  geom_sf(aes(fill=food_insecure_percentage), col="transparent") + 
  theme_map() + 
  scale_fill_viridis_c() +
  labs(fill = "% food\ninsecure")

ggplot(mapping) + 
  geom_sf(aes(fill=supply_gap_lbs_natural), col="transparent") + 
  theme_map() + 
  labs(fill = "Supply gap")
  



f = index ~ total_popE + perc_black + perc_hisp + perc_asian + perc_two + 
  perc_other + perc_native + perc_pacific +
  food_insecure_percentage 
ols <- lm(f, data = combined_sf)
summary(ols, vcov = conley(cutoff = 10))




ols <- lm(f, data = combined_sf)
summary(ols, vcov = conley(cutoff = 10))


ols_fe <- feols(f, data = combined_sf)
summary(ols_fe, vcov = conley(cutoff = 10))
