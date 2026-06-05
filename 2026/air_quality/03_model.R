library(tidyverse)
library(pROC)
library(broom)
library(sf)
library(ResourceSelection)
library(councilverse)
library(vroom)

options(scipen = 99999)

community_districts <- st_read('data/district_data.geojson') %>%
  mutate(pct_under_18 = round((pct_under_18)*100,2))

# hist(community_districts$child_asthma)

community_districts <- community_districts %>%
  mutate(log_childasthma =  log(child_asthma))
# hist(community_districts$log_childasthma)

# ---- typical ols, with log transformation on child asthma rate
m1 <- glm(log_childasthma ~ total_pop + pct_households_snap + 
            pct_black_alone + pct_asian_alone + pct_hisp_latino,
            data = community_districts, family = gaussian)
summary(m1)
plot(m1)

coef_m1 <- tidy(m1, conf.int = TRUE)
coef_m1 <- coef_m1 %>%
  mutate(fill_col = case_when(
    (conf.low < 0) & (conf.high < 0) ~ "#0073e6",
    (conf.low > 0) & (conf.high > 0) ~ "#f57600",
    .default = "darkgrey"
  )) %>%
  filter(term != "(Intercept)") %>%
  mutate(term_clean = case_when(
    term == "total_pop" ~ "Total population ",
    term == "pct_under_18" ~ "Under 18 years (%) ",
    term == "pct_households_snap" ~ "SNAP households (%) ",
    term == "pct_hisp_latino" ~ "Hispanic/Latino (%) ",
    term == "pct_black_alone" ~ "Black (%) ",
    term == "pct_asian_alone" ~ "Asian (%) "
  ))
ggplot(coef_m1, 
       aes(x=term_clean, y=(estimate))) +
  geom_col(fill = coef_m1$fill_col,
           alpha = 0.6) +
  geom_errorbar(aes(ymin = conf.low, 
                    ymax = conf.high), 
                width = 0.35,
                size = 1,
                color =  coef_m1$fill_col,
                show.legend = FALSE) +
  labs(y = "Change in log rate of child asthma hospitalizations (per 10k children)") + 
  theme_nycc() +
  theme(axis.title.y = element_blank()) +
  coord_flip() 

# ---- logistic regression using deid sparcs data; data is a little funky but results
# ---- corroborate the first regression
sparcs_24 <- vroom("https://health.data.ny.gov/resource/sf4k-39ay.csv?health_service_area='New%20York%20City'&$limit=999999999999") 
sparcs_24 <- sparcs_24 %>%
  # filter(health_service_area == "New York City") %>%
  mutate(
    asthma_0_1 = case_when(
      ccsr_diagnosis_code == 'RSP009' ~ 1,
      .default = 0),
    under_18 = case_when(
      age_group == '0-17' ~ "Under 18 years old",
      .default = "18+ years old"),
    private_insurance_1 = case_when(
      payment_typology_1 == "Blue Cross/Blue Shield" | payment_typology_1 == "Private Health Insurance" ~ "Private insurance",
      payment_typology_1 %in% c("Department of Corrections", "Federal/State/Local/VA", "Medicaid", "Medicare") ~ "Government-assisted insurance",
      .default = "Other"
    )
  )
sparcs_24$race <- relevel(factor(sparcs_24$race), ref = "White")
sparcs_24$ethnicity <- relevel(factor(sparcs_24$ethnicity), ref = "Not Span/Hispanic")
sparcs_24$private_insurance_1 <- relevel(factor(sparcs_24$private_insurance_1), ref = "Private insurance")
m2 <- glm(asthma_0_1 ~ race + ethnicity + under_18 + private_insurance_1,
          data = sparcs_24, family = binomial)
summary(m2)
plot(performance::binned_residuals(m2)) # not great - imbalanced data
pROC::roc(response = m2$y, predictor = fitted(m2)) # decent

# # ---- gamma glm - too hard to explain
# m2 <- glm(child_asthma ~ total_pop + pct_households_snap + 
#       pct_black_alone + pct_asian_alone + pct_hisp_latino,
#     family = Gamma(link = "log"),
#     data = community_districts)
# summary(m2)
