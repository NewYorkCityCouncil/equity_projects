library(fixest)

################################################################################
#  prep data
################################################################################

combined_sf = readRDS("data/spatial_panel.csv")

centroids = combined_sf %>% st_transform(4326) %>% st_centroid() %>% st_coordinates()
combined_sf$lon = centroids[, 1]
combined_sf$lat = centroids[, 2]

coords = st_coordinates(st_centroid(st_geometry(combined_sf)))
nb = make.sym.nb(knn2nb(knearneigh(coords, k = 8)))
listw = nb2listw(nb, style = "W")



################################################################################
#  full model 
################################################################################

f = food_insecure_percentage ~ total_popE + perc_black + perc_hisp + 
  perc_asian + perc_two + perc_other + perc_native + perc_pacific +
  food_insecure_percentage + index

ols_fe <- feols(f, data = combined_sf)
summary(ols_fe, vcov = conley(cutoff = 17))

# test conley cutoff -----------------------------------------------------------
empty = rep(NA, 30)
test_conley = data.frame(conley=empty, intercept=empty, pop=empty, black=empty, 
                         hisp=empty, asian=empty, two=empty, other=empty, 
                         native=empty, pacific=empty, food=empty)

ols_fe <- feols(f, data = combined_sf)
for (i in 1:30) { 
  x = summary(ols_fe, vcov = conley(cutoff = i)) 
  test_conley[i, ] = c(i, unname(x$coeftable[,4]))
}

ggplot(test_conley) + 
  geom_line(aes(conley, hisp), color = "green") + 
  geom_line(aes(conley, pacific), color = "blue") + 
  geom_line(aes(conley, food), color = "purple") + 
  geom_line(aes(conley, intercept), color = "red") + 
  geom_hline(yintercept=0.05)

ggplot(test_conley) + 
  geom_line(aes(conley, pop), color = "green") + 
  geom_line(aes(conley, black), color = "blue") + 
  geom_line(aes(conley, two), color = "purple") + 
  geom_line(aes(conley, asian), color = "red") + 
  geom_line(aes(conley, other), color = "darkgreen") + 
  geom_line(aes(conley, other), color = "darkred") + 
  geom_hline(yintercept=0.05) + 
  geom_hline(yintercept=0.1)


################################################################################
#  model - no index
################################################################################

f = food_insecure_percentage ~ total_popE + perc_black + perc_hisp + 
  perc_asian + perc_two + perc_other + perc_native + perc_pacific

ols_fe <- feols(f, data = combined_sf)
summary(ols_fe, vcov = conley(cutoff = 4))

# test conley cutoff -----------------------------------------------------------
empty = rep(NA, 30)
test_conley = data.frame(conley=empty, intercept=empty, pop=empty, black=empty, 
                         hisp=empty, asian=empty, two=empty, other=empty, 
                         native=empty, pacific=empty)

ols_fe <- feols(f, data = combined_sf)
for (i in 1:30) { 
  x = summary(ols_fe, vcov = conley(cutoff = i)) 
  test_conley[i, ] = c(i, unname(x$coeftable[,4]))
}

ggplot(test_conley) + 
  geom_line(aes(conley, hisp), color = "green") + 
  geom_line(aes(conley, pacific), color = "blue") + 
  geom_line(aes(conley, intercept), color = "red") + 
  geom_line(aes(conley, other), color = "darkgreen") + 
  geom_hline(yintercept=0.05)

ggplot(test_conley) + 
  geom_line(aes(conley, pop), color = "green") + 
  geom_line(aes(conley, black), color = "blue") + 
  geom_line(aes(conley, two), color = "purple") + 
  geom_line(aes(conley, other), color = "red") +
  geom_line(aes(conley, native), color = "darkgreen") +  
  geom_hline(yintercept=0.05) + 
  geom_hline(yintercept=0.1)

