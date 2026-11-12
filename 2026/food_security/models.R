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
#  test basic OLS model
################################################################################

# test if we need to deal w/ spatial  ------------------------------------------

f = index ~ total_popE + perc_black + perc_hisp + perc_asian + perc_two + 
  perc_other + perc_native + perc_pacific +
  food_insecure_percentage + supply_gap_lbs_natural

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
  geom_line(aes(conley, two), color = "green") + 
  geom_line(aes(conley, native), color = "blue") + 
  geom_line(aes(conley, pacific), color = "purple") + 
  geom_line(aes(conley, food), color = "red") + 
  geom_hline(yintercept=0.05)

ggplot(test_conley) + 
  geom_line(aes(conley, pop), color = "green") + 
  geom_line(aes(conley, black), color = "blue") + 
  geom_line(aes(conley, hisp), color = "purple") + 
  geom_line(aes(conley, asian), color = "red") + 
  geom_line(aes(conley, other), color = "darkgreen") + 
  geom_hline(yintercept=0.05)


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

# run with model that will estimate the spatially correlated error instead of just 
# correcting for it. 
sem <- errorsarlm(f, data = combined_sf, listw = listw)
summary(sem, Nagelkerke = TRUE)

# did sptaial model absorb spatial structure? yes
moran.test(residuals(sem), listw)

# comparison
AIC(ols, sem)
