library(tidyverse)
library(councilverse)
library(janitor)
library(sf)
library(vroom)
library(zoo)
library(ggpubr)
library(ggiraph)
library(cowplot)
# library(ggplot2)
library(patchwork)
options(scipen = -1)

community_districts <- st_read('data/district_data.geojson') %>%
  mutate(pct_under_18 = round((pct_under_18)*100,2))

# ---- correlations: childhood asthma rates x race/ethnicity ----
# ---- white alone 
w_plot <- ggplot(data = community_districts, 
       aes(x = pct_white_alone/100, y = child_asthma, color = pct_white_alone,
           data_id = communitydist)) +
  stat_cor(method = "spearman") + 
  geom_point_interactive(size = community_districts$total_pop/40000,
                         aes(tooltip = paste0("<strong>Community District: ", geography, "</strong><br>",
                                              "Childhood asthma rate: ", child_asthma, "<br>",
                                              "Percent white: ", pct_white_alone, "%"))) +
  scale_color_distiller(direction = 1) +
  guides(color="none") +
  scale_x_continuous(labels = scales::percent) +
  theme_nycc() +
  labs(color = "Percent white",
       x = "Percent white",
       y = "Childhood asthma rate")
w_map <- ggplot(NULL) + 
  geom_sf_interactive(data = community_districts, size = 0.1, 
                      aes(fill = pct_white_alone/100,
                          data_id = communitydist)) +
  geom_sf_label(data=community_districts, aes(label = communitydist), 
                label.size  = .05, 
                alpha = .2, 
                size =2,
                color = "navy") +
  scale_fill_distiller(direction = 1,
                       breaks = 0.1*0:10,
                       labels = scales::percent(0.1*0:10)) +
  theme_nycc() + 
  theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) +
  labs(fill = "Percent white",
       x = NULL,
       y = NULL) 
combined <- plot_grid(w_plot, w_map, nrow = 1)
white_asthma_mp <- girafe(ggobj = combined, width_svg = 12, height_svg = 5.5) %>%
  girafe_options(opts_zoom(min = 1, max = 8),
                 opts_selection(
                   css = "fill:moccasin;",
                   only_shiny = FALSE),
                 opts_tooltip(
                   opacity = 0.8,
                   css = "background-color:#4c6061; color:white; padding:10px; border-radius:5px;"),
                 opts_toolbar(pngname = "white_athsma_corr"))

# ---- asian alone
a_plot <- ggplot(data = community_districts, 
                 aes(x = pct_asian_alone/100, y = child_asthma, color = pct_asian_alone,
                     data_id = communitydist)) +
  stat_cor(method = "spearman") + 
  geom_point_interactive(size = community_districts$total_pop/40000,
                         aes(tooltip = paste0("<strong>Community District: ", geography, "</strong><br>",
                                              "Childhood asthma rate: ", child_asthma, "<br>",
                                              "Percent Asian: ", pct_asian_alone, "%"))) +
  scale_color_distiller(direction = 1) +
  guides(color="none") +
  scale_x_continuous(labels = scales::percent) +
  theme_nycc() +
  labs(color = "Percent Asian",
       x = "Percent Asian",
       y = "Childhood asthma rate")
a_map <- ggplot(NULL) + 
  geom_sf_interactive(data = community_districts, size = 0.1, 
                      aes(fill = pct_asian_alone/100,
                          data_id = communitydist)) +
  geom_sf_label(data=community_districts, aes(label = communitydist), 
                label.size  = .05, 
                alpha = .2, 
                size =2,
                color = "navy") +
  scale_fill_distiller(direction = 1,
                       breaks = 0.1*0:10,
                       labels = scales::percent(0.1*0:10)) +
  theme_nycc() + 
  theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) +
  labs(fill = "Percent Asian",
       x = NULL,
       y = NULL) 
combined <- plot_grid(a_plot, a_map, nrow = 1)
asian_asthma_mp <- girafe(ggobj = combined, width_svg = 12, height_svg = 5.5) %>%
  girafe_options(opts_zoom(min = 1, max = 8),
                 opts_selection(
                   css = "fill:moccasin;",
                   only_shiny = FALSE),
                 opts_tooltip(
                   opacity = 0.8,
                   css = "background-color:#4c6061; color:white; padding:10px; border-radius:5px;"),
                 opts_toolbar(pngname = "asian_athsma_corr"))

# ---- hispanic/latino
hl_plot <- ggplot(data = community_districts, 
                 aes(x = pct_hisp_latino/100, y = child_asthma, color = pct_hisp_latino,
                     data_id = communitydist)) +
  stat_cor(method = "spearman") + 
  geom_point_interactive(size = community_districts$total_pop/40000,
                         aes(tooltip = paste0("<strong>Community District: ", geography, "</strong><br>",
                                              "Childhood asthma rate: ", child_asthma, "<br>",
                                              "Percent Hispanic/Latino: ", pct_hisp_latino, "%"))) +
  scale_color_distiller(direction = 1) +
  guides(color="none") +
  scale_x_continuous(labels = scales::percent) +
  theme_nycc() +
  labs(color = "Percent Hispanic/Latino",
       x = "Percent Hispanic/Latino",
       y = "Childhood asthma rate")
hl_map <- ggplot(NULL) + 
  geom_sf_interactive(data = community_districts, size = 0.1, 
                      aes(fill = pct_hisp_latino/100,
                          data_id = communitydist)) +
  geom_sf_label(data=community_districts, aes(label = communitydist), 
                label.size  = .05, 
                alpha = .2, 
                size =2,
                color = "navy") +
  scale_fill_distiller(direction = 1,
                       breaks = 0.1*0:10,
                       labels = scales::percent(0.1*0:10)) +
  theme_nycc() + 
  theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) +
  labs(fill = "Percent Hispanic/Latino",
       x = NULL,
       y = NULL) 
combined <- plot_grid(hl_plot, hl_map, nrow = 1)
hisp_asthma_mp <- girafe(ggobj = combined, width_svg = 12.5, height_svg = 5) %>%
  girafe_options(opts_zoom(min = 1, max = 8),
                 opts_selection(
                   css = "fill:moccasin;",
                   only_shiny = FALSE),
                 opts_tooltip(
                   opacity = 0.8,
                   css = "background-color:#4c6061; color:white; padding:10px; border-radius:5px;"),
                 opts_toolbar(pngname = "hisp_athsma_corr"))

# ---- black alone
b_plot <- ggplot(data = community_districts, 
                  aes(x = pct_black_alone/100, y = child_asthma, color = pct_black_alone,
                      data_id = communitydist)) +
  stat_cor(method = "spearman") + 
  geom_point_interactive(size = community_districts$total_pop/40000,
                         aes(tooltip = paste0("<strong>Community District: ", geography, "</strong><br>",
                                              "Childhood asthma rate: ", child_asthma, "<br>",
                                              "Percent Black: ", pct_black_alone, "%"))) +
  scale_color_distiller(direction = 1) +
  guides(color="none") +
  scale_x_continuous(labels = scales::percent) +
  theme_nycc() +
  labs(color = "Percent Black",
       x = "Percent Black",
       y = "Childhood asthma rate")
b_map <- ggplot(NULL) + 
  geom_sf_interactive(data = community_districts, size = 0.1, 
                      aes(fill = pct_black_alone/100,
                          data_id = communitydist)) +
  geom_sf_label(data=community_districts, aes(label = communitydist), 
                label.size  = .05, 
                alpha = .2, 
                size =2,
                color = "navy") +
  scale_fill_distiller(direction = 1,
                       breaks = 0.1*0:10,
                       labels = scales::percent(0.1*0:10)) +
  theme_nycc() + 
  theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) +
  labs(fill = "Percent Black",
       x = NULL,
       y = NULL) 
combined <- plot_grid(b_plot, b_map, nrow = 1)
black_asthma_mp <- girafe(ggobj = combined, width_svg = 12, height_svg = 5.5) %>%
  girafe_options(opts_zoom(min = 1, max = 8),
                 opts_selection(
                   css = "fill:moccasin;",
                   only_shiny = FALSE),
                 opts_tooltip(
                   opacity = 0.8,
                   css = "background-color:#4c6061; color:white; padding:10px; border-radius:5px;"),
                 opts_toolbar(pngname = "black_athsma_corr"))

# # ---- foreign born
# fb_plot <- ggplot(data = community_districts, 
#                  aes(x = pct_foreign_born/100, y = child_asthma, color = pct_foreign_born,
#                      data_id = communitydist)) +
#   stat_cor(method = "spearman") + 
#   geom_point_interactive(size = community_districts$total_pop/40000,
#                          aes(tooltip = paste0("<strong>Community District: ", geography, "</strong><br>",
#                                               "Childhood asthma rate: ", child_asthma, "<br>",
#                                               "Percent foreign born: ", pct_foreign_born, "%"))) +
#   scale_color_distiller(direction = 1) +
#   guides(color="none") +
#   scale_x_continuous(labels = scales::percent) +
#   theme_nycc() +
#   labs(color = "Percent foreign born",
#        x = "Percent foreign born",
#        y = "Childhood asthma rate")
# fb_map <- ggplot(NULL) + 
#   geom_sf_interactive(data = community_districts, size = 0.1, 
#                       aes(fill = pct_foreign_born/100,
#                           data_id = communitydist)) +
#   geom_sf_label(data=community_districts, aes(label = communitydist), 
#                 label.size  = .05, 
#                 alpha = .2, 
#                 size =2,
#                 color = "navy") +
#   scale_fill_distiller(direction = 1,
#                        breaks = 0.1*0:10,
#                        labels = scales::percent(0.1*0:10)) +
#   theme_nycc() + 
#   theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) +
#   labs(fill = "Percent foreign born",
#        x = NULL,
#        y = NULL) 
# combined <- plot_grid(fb_plot, fb_map, nrow = 1)
# foreign_asthma_mp <- girafe(ggobj = combined, width_svg = 12, height_svg = 5.5) %>%
#   girafe_options(opts_zoom(min = 1, max = 8),
#                  opts_selection(
#                    css = "fill:moccasin;",
#                    only_shiny = FALSE),
#                  opts_tooltip(
#                    opacity = 0.8,
#                    css = "background-color:#4c6061; color:white; padding:10px; border-radius:5px;"),
#                  opts_toolbar(pngname = "foreign_athsma_corr"))

# ---- SNAP households
s_plot <- ggplot(data = community_districts, 
                 aes(x = pct_households_snap/100, y = child_asthma, color = pct_households_snap,
                     data_id = communitydist)) +
  stat_cor(method = "spearman") + 
  geom_point_interactive(size = community_districts$total_pop/40000,
                         aes(tooltip = paste0("<strong>Community District: ", geography, "</strong><br>",
                                              "Childhood asthma rate: ", child_asthma, "<br>",
                                              "Percent SNAP: ", pct_households_snap, "%"))) +
  scale_color_distiller(direction = 1) +
  guides(color="none") +
  scale_x_continuous(labels = scales::percent) +
  theme_nycc() +
  labs(color = "Percent SNAP",
       x = "Percent households receiving SNAP",
       y = "Childhood asthma rate")
s_map <- ggplot(NULL) + 
  geom_sf_interactive(data = community_districts, size = 0.1, 
                      aes(fill = pct_households_snap/100,
                          data_id = communitydist)) +
  geom_sf_label(data=community_districts, aes(label = communitydist), 
                label.size  = .05, 
                alpha = .2, 
                size =2,
                color = "navy") +
  scale_fill_distiller(direction = 1,
                       breaks = 0.1*0:10,
                       labels = scales::percent(0.1*0:10)) +
  theme_nycc() + 
  theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank()) +
  labs(fill = "Percent SNAP",
       x = NULL,
       y = NULL) 
combined <- plot_grid(s_plot, s_map, nrow = 1)
snap_asthma_mp <- girafe(ggobj = combined, width_svg = 12, height_svg = 5.5) %>%
  girafe_options(opts_zoom(min = 1, max = 8),
                 opts_selection(
                   css = "fill:moccasin;",
                   only_shiny = FALSE),
                 opts_tooltip(
                   opacity = 0.8,
                   css = "background-color:#4c6061; color:white; padding:10px; border-radius:5px;"),
                 opts_toolbar(pngname = "snap_athsma_corr"))
