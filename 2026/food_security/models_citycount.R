library(fixest)

################################################################################
#  settings
################################################################################

pantry_buffer = 10
tract_buffer = 800

################################################################################
#  prep data
################################################################################

combined_sf = readRDS("data/spatial_panel.csv")

boundaries = st_read("https://data.cityofnewyork.us/api/v3/views/gthc-hcne/query.geojson") %>% 
  st_transform(st_crs(combined_sf))
combined_sf = combined_sf %>% st_intersection(boundaries)

centroids = combined_sf %>% st_transform(4326) %>% st_centroid() %>% st_coordinates()
combined_sf$lon = centroids[, 1]
combined_sf$lat = centroids[, 2]

coords = st_coordinates(st_centroid(st_geometry(combined_sf)))
nb = make.sym.nb(knn2nb(knearneigh(coords, k = 8)))
listw = nb2listw(nb, style = "W")

nyc_counties <- c("Bronx", "Kings", "New York", "Queens", "Richmond")
nyc_tracts <- get_acs(
  geography = "tract",
  variables = "B03002_001",
  state     = "NY",
  county    = nyc_counties,
  year      = 2022,
  survey    = "acs5",
  output    = "wide", 
  geometry = T) %>% 
  st_transform(4326)


################################################################################
#  add count of pantries
################################################################################

# from the EFAI repo
efai_pantries = st_read(file.path("..", "..", "..", "Emergency-Food-Access-Index", "FP_data", "food_pantry_schedule_CFCs.geojson"))

# from the food security repo
cfc_pantries = readRDS(file.path("..", "..", "..", "food_insecurity", "data", "output", "cfc_geocoded_2024.RDS")) %>% 
  st_as_sf()

# categorize
food_pantries_city = st_intersection(efai_pantries, 
                                     cfc_pantries %>% st_buffer(pantry_buffer) %>% st_geometry())
food_pantries_noncity = efai_pantries %>% filter(!id %in% food_pantries_city$id)

# calculate number of each type within about distance of each tract
tract_city = st_intersection(food_pantries_city, nyc_tracts %>% st_buffer(tract_buffer)) %>% 
  st_drop_geometry() %>% 
  group_by(GEOID) %>% 
  summarise(count_city = n())

tract_noncity = st_intersection(food_pantries_noncity, nyc_tracts %>% st_buffer(tract_buffer)) %>% 
  st_drop_geometry() %>% 
  group_by(GEOID) %>% 
  summarise(count_noncity = n())

combined_sf = combined_sf %>% 
  merge(tract_city, by="GEOID", all.x=T) %>% 
  merge(tract_noncity, by="GEOID", all.x=T) %>% 
  mutate(count_city = ifelse(is.na(count_city), 0, count_city), 
         count_noncity = ifelse(is.na(count_noncity), 0, count_noncity),
         count_tot = count_city + count_noncity,
         perc_city = count_city/(count_city+count_noncity)*100, 
         supply_gap_pp = supply_gap_lbs_natural/total_popE, 
         more_city = case_when(count_tot == 0 ~ NA, 
                               count_city>count_noncity ~ "more city pantries", 
                               count_city<count_noncity ~ "more noncity pantries", 
                               count_city==count_noncity ~ "equal"))


################################################################################
#  quick plots to see differences
################################################################################

ggplot(combined_sf %>% filter(count_city + count_noncity > 0)) + 
  geom_jitter(aes(count_city, count_noncity), alpha=0.2, width=1, height=1) + 
  theme_anne() + xlim(0, 10) + ylim(0, 10)

ggplot(combined_sf) + 
  geom_sf(aes(fill = more_city)) + theme_map()


################################################################################
#  test basic OLS model
################################################################################

# test if we need to deal w/ spatial  ------------------------------------------

f = food_insecure_percentage ~ total_popE + perc_black + perc_hisp + 
  perc_asian + perc_two + perc_other + perc_native + perc_pacific + 
  count_city + count_noncity

f = supply_gap_pp ~ count_tot + perc_city

f = perc_city ~ total_popE + perc_black + perc_hisp + perc_asian + perc_two + 
  perc_other + perc_native + perc_pacific + food_insecure_percentage + 
  supply_gap_pp 

ols <- lm(f, data = combined_sf)
summary(ols)

# is there spatial dependence of resids? yes
lm.morantest(ols, listw)

# test conley cutoff -----------------------------------------------------------
empty = rep(NA, 30)
test_conley = data.frame(conley=empty, intercept=empty, pop=empty, black=empty, 
                         hisp=empty, asian=empty, two=empty, other=empty, 
                         native=empty, pacific=empty, food=empty, supply=empty)

ols_fe <- feols(f, data = combined_sf)
for (i in 1:30) { 
  x = summary(ols_fe, vcov = conley(cutoff = i)) 
  test_conley[i, ] = c(i, unname(x$coeftable[,4]))
}

ggplot(test_conley) + 
  geom_line(aes(conley, native), color = "blue") + 
  geom_line(aes(conley, pacific), color = "purple") + 
  geom_line(aes(conley, food), color = "red") + 
  geom_line(aes(conley, hisp), color = "darkgreen") + 
  geom_hline(yintercept=0.05)

ggplot(test_conley) + 
  geom_line(aes(conley, pop), color = "green") + 
  geom_line(aes(conley, black), color = "blue") + 
  geom_line(aes(conley, asian), color = "red") + 
  geom_line(aes(conley, two), color = "purple") + 
  geom_line(aes(conley, other), color = "darkgreen") + 
  geom_hline(yintercept=0.05) + 
  geom_hline(yintercept=0.1)


################################################################################
#  Run clustered OLS model
################################################################################

# cluster resids using conley w/ a 10km cutoff
ols_fe <- feols(f, data = combined_sf)
summary(ols_fe, vcov = conley(cutoff = 10))

# is there spatial dependence of resids? STILL YES
moran.test(residuals(ols_fe), listw = listw)


################################################################################
#  test spatial error model 
################################################################################

# run with model that will estimate the spatially correlated error instead of
#  just correcting for it. 
sem <- errorsarlm(f, data = combined_sf, listw = listw)
summary(sem, Nagelkerke = TRUE)

# did sptaial model absorb spatial structure? yes
moran.test(residuals(sem), listw)

# comparison
AIC(ols, sem)
