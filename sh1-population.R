# State highway 1 population bar chart

# *****************************************************************************
# Setup ----

library(conflicted)
library(tidyverse)
library(here)
library(janitor)
library(sf)
library(future)
library(geosphere)

conflict_prefer(name = "filter", winner = "dplyr")

# *****************************************************************************


# *****************************************************************************
# Load data ----

pop_2018_by_sa2_ethnicity <- read_csv(file = here("data/pop-2018-by-sa2-ethnicity.csv"), 
                                      col_types = "ccn") %>%
  mutate(area = str_to_title(area)) %>%    # Needed to match some area names in sa2 shapes
  mutate(area = ifelse(area == "Te Atatu South-Mcleod North", "Te Atatu South-McLeod North", area)) %>%
  mutate(area = ifelse(area == "Te Atatu South-Mcleod South", "Te Atatu South-McLeod South", area)) %>%
  mutate(area = ifelse(area == "Ben Mcleod", "Ben McLeod", area)) %>%
  mutate(area = ifelse(area == "Mcjorrow Park", "McJorrow Park", area)) %>%
  mutate(area = ifelse(area == "Mclaren Park", "McLaren Park", area)) %>%
  mutate(area = ifelse(area == "Mclean Park", "McLean Park", area)) %>%
  mutate(area = ifelse(area == "Mcleans Island", "McLeans Island", area)) %>%
  mutate(area = ifelse(area == "Cameron And Soldiers Park", "Cameron and Soldiers Park", area)) %>%
  mutate(area = str_replace(string = area, pattern = "Inland Water", replacement = "Inland water")) %>%
  mutate(area = str_replace(string = area, pattern = "Bay Of Plenty", replacement = "Bay of Plenty"))

sa2_shapes <- read_sf(dsn = here("data/statsnzstatistical-area-2-2020-clipped-generalised-SHP/statistical-area-2-2020-clipped-generalised.shp")) %>%
  clean_names()

sh1 <- read_sf(dsn = here("data/sh1/sh1.shp")) %>%
  st_simplify(dTolerance = 1000) %>%
  st_combine()

# *****************************************************************************


# *****************************************************************************
# Process data ----

# SA2 centroids in lat/lon coordinates
sa2_centroids <- sa2_shapes %>%
  st_centroid() %>%
  st_transform(crs = 4326) %>%
  select(starts_with("sa22"))

# Find nearest point on SH1 for each SA2 centroid
# Use parallel processing to speed up
plan(strategy = "multisession")
p1 <- sa2_centroids[1:500,] %>% as_Spatial()
p2 <- sa2_centroids[501:1000,] %>% as_Spatial()
p3 <- sa2_centroids[1001:1500,] %>% as_Spatial()
p4 <- sa2_centroids[1501:2173,] %>% as_Spatial()
l <- sh1 %>% st_transform(crs = 4326) %>% as_Spatial()

dists1 %<-% dist2Line(p = p1, line = l)
dists2 %<-% dist2Line(p = p2, line = l)
dists3 %<-% dist2Line(p = p3, line = l)
dists4 %<-% dist2Line(p = p4, line = l)

dists <- rbind(dists1, dists2, dists3, dists4)
plan(strategy = "sequential")

# *****************************************************************************


# *****************************************************************************
# Visualise ----

# Visualisation data -- centroids and nearest point on SH1
vis_dat <- bind_cols(
  # SA2 info
  sa2_centroids %>%
    st_drop_geometry(), 
  
  # SA2 centroid points in NZTM
  sa2_centroids %>%
    st_transform(crs = 2193) %>%
    st_coordinates() %>%
    as_tibble %>%
    rename(centroid_x = X, centroid_y = Y), 
  
  # Nearest point on SH1
  dists %>%
    as_tibble() %>%
    st_as_sf(coords = c("lon", "lat"), dim = "XY") %>%
    st_set_crs(value = 4326) %>%
    st_transform(crs = 2193) %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(sh1_x = X, sh1_y = Y)
) %>% 
  mutate(direction = ifelse(centroid_x < sh1_x, "west", "east")) %>%
  right_join(y = pop_2018_by_sa2_ethnicity, 
            by = c("sa22020_2" = "area")) %>%
  mutate(sh1_x_end = ifelse(direction == "west", sh1_x - 50 * value, sh1_x + 50 * value))

# Create visualisation for all ethnicities
vis <- vis_dat %>% 
  filter(ethnic_group == "Total people, ethnic group") %>%
  ggplot() + 
  geom_segment(mapping = aes(x = sh1_x, 
                             xend = sh1_x_end, 
                             y = sh1_y, 
                             yend = sh1_y), 
               size = 0.1) + 
  geom_sf(data = sh1, colour = "firebrick") + 
  coord_sf(crs = 2193, 
           label_axes = "", 
           datum = NA) + 
  theme_minimal() + 
  xlab("") + 
  ylab("")

ggsave(filename = here("outputs/sh1-population.png"), 
       plot = vis, 
       device = "png", 
       width = 10,
       height = 13, 
       units = "cm", 
       dpi = 300)

# Visualise by ethnicity
vis_by_ethnicity <- vis_dat %>%
  filter(ethnic_group != "Total people, ethnic group") %>% 
  mutate(ethnic_group = factor(x = ethnic_group, 
                               levels = c("Asian", 
                                          "European or Other (including New Zealander)", 
                                          "Maori", 
                                          "Middle Eastern/Latin American/African", 
                                          "Pacific"), 
                               labels = c("Asian", 
                                          "European", 
                                          "Māori", 
                                          "MELAA", 
                                          "Pacific"), 
                               ordered = TRUE)) %>%
  ggplot() + 
  geom_segment(mapping = aes(x = sh1_x, 
                             xend = sh1_x_end, 
                             y = sh1_y, 
                             yend = sh1_y), 
               size = 0.1) + 
  geom_sf(data = sh1, colour = "firebrick") + 
  coord_sf(crs = 2193, 
           label_axes = "", 
           datum = NA) + 
  facet_wrap(facets = vars(ethnic_group), nrow = 1) + 
  theme_minimal() + 
  xlab("") + 
  ylab("")

ggsave(filename = here("outputs/sh1-population-by-ethnicity.png"), 
       plot = vis_by_ethnicity, 
       device = "png", 
       width = 40,
       height = 13, 
       units = "cm", 
       dpi = 300)

# *****************************************************************************

