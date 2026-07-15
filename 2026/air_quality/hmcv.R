library(tidyverse)
library(vroom)
library(sf)
library(ggpubr)
library(councilverse)
library(broom)

community_districts <- st_read('data/district_data.geojson') %>%
  mutate(pct_under_18 = round((pct_under_18)*100,2))

# ---- download hmcv violations -----
hmcv <- vroom("https://data.cityofnewyork.us/resource/wvxf-dwi5.csv?$where=inspectiondate>'2021-01-01'&$limit=999999999999")

# ---- filter to b & c violations, those not dismissed
hmcv <- hmcv %>%
  filter(class %in% c("B","C"),
         currentstatus != "VIOLATION DISMISSED",
         !(is.na(bbl))) %>%
  distinct() 

# ---- merge to pluto data to get bbl/geo info 
pluto <- vroom("https://data.cityofnewyork.us/resource/64uk-42ks.csv?$limit=9999999999999")
resunits_by_cd <- pluto %>%
  filter(!is.na(cd), !is.na(unitsres)) %>%
  group_by(cd) %>% 
  summarise(tot_resunits = sum(unitsres))
# ---- filter to only bbls in hmcv subset, select id (bbl) + community district (cd)
pluto <- pluto %>%
  filter(bbl %in% hmcv$bbl) %>% # < 1% lost
  select(bbl, cd)

# ---- left join cd to hmcv, summarize over cds
hmcv_summ <- hmcv %>%
  left_join(pluto, by="bbl") %>%
  group_by(cd) %>%
  summarise(n_bc = n()) %>%
  filter(!is.na(cd)) %>% # < 1%
  left_join(resunits_by_cd, by = "cd") %>%
  mutate(hmcv_per_100resunits = round(n_bc/(tot_resunits/100),2))
rm(pluto) ; rm(hmcv)

# ---- join to original cd data
community_districts <- community_districts %>%
  left_join(hmcv_summ,
            by = c("communitydist" = "cd"))

# ---- corr plot: hmcvs x asthma rates
ggplot(data = community_districts, 
       aes(x = child_asthma, 
           y = hmcv_per_100resunits, # color = pct_white_alone,
           label = communitydist,
           data_id = communitydist)) +
  stat_cor(method = "spearman") + 
  labs(x = "Childhood Asthma Emergency Department Visit Rate per 10,000", y = "Housing Maintenance Code Violations per 100 Residential Units") +
  geom_text(nudge_x = 5,
            nudge_y = 5) +
  geom_point() + 
  theme_nycc()
  # geom_point_interactive(size = community_districts$total_pop/40000,
  #                        aes(tooltip = paste0("<strong>Community District: ", geography, "</strong><br>",
  #                                             "Childhood asthma rate: ", child_asthma, "<br>",
  #                                             "Percent white: ", pct_white_alone, "%"))) +
  # scale_color_distiller(direction = 1) +
  # guides(color="none") +
  # scale_x_continuous(labels = scales::percent) +
  # theme_nycc() +
  # labs(color = "Percent white",
  #      x = "Percent white",
  #      y = "Childhood asthma rate")
# 316 : Brownsville (CD16)
# 111 : East Harlem (CD11)
# 110 : Central Harlem (CD10)
# 201 : Mott Haven and Melrose (CD1)

# ---- model relationship: hmcvs x asthma rates
# hist(log(community_districts$child_asthma))
community_districts <- community_districts %>%
  mutate(log_childasthma =  log(child_asthma))
m <- glm(child_asthma ~ hmcv_per_100resunits + pct_households_snap + 
            pct_black_alone + pct_asian_alone + pct_hisp_latino,
          data = community_districts, family = gaussian)
summary(m)
plot(m)
# pct_households_snap, pct_black_alone, pct_hisp_latino stat sig & positive; 
# these demographics were associated with higher rates of child asthma, holding 
# hmcvs constant

# SNAP is only significant when controlling for HMCV (see 03_model.R).
# It could be interesting to examine other poverty metrics instead (<200% FPL?)

coef_m <- tidy(m, conf.int = TRUE)
coef_m <- coef_m %>%
  mutate(fill_col = case_when(
    (conf.low < 0) & (conf.high < 0) ~ "#0073e6",
    (conf.low > 0) & (conf.high > 0) ~ "#f57600",
    .default = "darkgrey"
  )) %>%
  filter(term != "(Intercept)") %>%
  mutate(term_clean = case_when(
    # term == "total_pop" ~ "Total population ",
    # term == "pct_under_18" ~ "Under 18 years (%) ",
    term == "hmcv_per_100resunits" ~ "HMCVs per 100 residential units",
    term == "pct_households_snap" ~ "SNAP households (%) ",
    term == "pct_hisp_latino" ~ "Hispanic/Latino (%) ",
    term == "pct_black_alone" ~ "Black (%) ",
    term == "pct_asian_alone" ~ "Asian (%) "
  ))
ggplot(coef_m, 
       aes(x=term_clean, y=(estimate))) +
  geom_col(fill = coef_m$fill_col,
           alpha = 0.6) +
  geom_errorbar(aes(ymin = conf.low, 
                    ymax = conf.high), 
                width = 0.35,
                size = 1,
                color =  coef_m$fill_col,
                show.legend = FALSE) +
  labs(y = "Change in log rate of child asthma hospitalizations (per 10k children)") + 
  theme_nycc() +
  theme(axis.title.y = element_blank()) +
  coord_flip() 
