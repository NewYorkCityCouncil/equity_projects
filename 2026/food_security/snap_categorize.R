library(dplyr)
library(stringr)
library(tidytext)
library(tibble)
library(Matrix)
library(irlba)
library(cluster)

# read in data -----------------------------------------------------------------

snap_loc = read_csv('/Users/ADriscoll/Documents/GitHub/equity_projects/2026/food_security/data/Historical SNAP Retailer Locator Data 2005-2025.csv')
names(snap_loc) = c("id", "name", "type", "addr_num", "addr_st", "addr_adl", "city", "state", "zip", "zip4", "county", "lat", "lon", "auth_date", "end_date")
nyc_snap = snap_loc %>% 
  filter(state == "NY", 
         county %in% c("BRONX", "KINGS", "NEW YORK", "QUEENS", "RICHMOND"),
         is.na(end_date)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)


# new category creation -----------------------------------------------------------------

nyc_snap = nyc_snap %>% 
  mutate(text_cat = case_when(!type %in% c("Combination Grocery/Other", "Small Grocery Store", "Delivery Route", "Military Commissary", "Convenience Store") ~ type, 
                              type == "Food Buying Co-op" ~ "Farmers' Market", 
                              grepl("dollar|discount|99|candy|for less|variety|eleven|quick|grab", name, ignore.case=T) ~ "Convenience Store", 
                              grepl("convenience|conveniece|deli|delicatessen|bodega|bodeguita|grill|salad|munchies", name, ignore.case=T) ~ "Deli/Bodega", 
                              grepl("duane reade|cvs|pharmacy|drug|walgreen|rite aid", name, ignore.case=T) ~ "Pharmacy", 
                              grepl("coffee|cafe|juice|beverage|bakery|pastry|bake|sweet|ice cream|cake|bagel", name, ignore.case=T) ~ "Bakery Specialty", 
                              grepl("fruit|vegetable", name, ignore.case=T) ~ "Fruits/Veg Specialty", 
                              grepl("fish|sea", name, ignore.case=T) ~ "Seafood Specialty", 
                              grepl("butcher|meat|pork|poultry|sausage|milk|carniceria", name, ignore.case=T) ~ "Meat/Poultry Specialty", 
                              grepl("supermarket|market|grocery|food", name, ignore.case=T) ~ "Small Grocery Store", 
                              T ~ "Uncategorized"),
         categories = case_when(text_cat %in% c("Fruits/Veg Specialty", "Meat/Poultry Specialty", "Farmers' Market", "Seafood Specialty", "Food Buying Co-op", "Bakery Specialty") ~ "Support Grocery", 
                                text_cat %in% c("Convenience Store", "Pharmacy") ~ "Non-Grocery", 
                                text_cat %in% c("Supermarket", "Super Store", "Large Grocery Store", "Medium Grocery Store", "Small Grocery Store") ~ "Grocery", 
                                text_cat == "Uncategorized" & type == "Convenience Store" ~ "Non-Grocery",
                                T ~ text_cat))
table(nyc_snap$categories)
table(nyc_snap %>% select(type, text_cat) %>% st_drop_geometry())
View(nyc_snap %>% filter(text_cat == "?") %>% st_drop_geometry())

saveRDS(nyc_snap, "data/output/snap_categorized.RDS")
         