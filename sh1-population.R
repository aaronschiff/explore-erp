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

pop_total_sa2_2018 <- read_csv(file = here("data/pop_total_2018_sa2.csv"), 
                               col_types = "cn")

sa2_shapes <- read_sf(dsn = here("data/statsnzstatistical-area-2-2020-clipped-generalised-SHP/statistical-area-2-2020-clipped-generalised.shp")) %>%
  clean_names()

sh1 <- read_sf(dsn = here("data/sh1/sh1.shp")) %>%
  st_simplify(dTolerance = 1000) %>%
  st_combine()

# *****************************************************************************


# *****************************************************************************
# Process data ----

# Total population for each SA2 area with centroids
pop_dat <- sa2_shapes %>%
  left_join(y = pop_total_sa2_2018, 
            by = c("sa22020_2" = "area")) %>%
  select(starts_with("sa22"), value) %>%
  st_centroid()

# Find nearest point on SH1 for each SA2 centroid
# Use parallel processing to speed up
plan(strategy = "multisession")
p1 <- pop_dat[1:500,] %>% st_transform(crs = 4326) %>% as_Spatial()
p2 <- pop_dat[501:1000,] %>% st_transform(crs = 4326) %>% as_Spatial()
p3 <- pop_dat[1001:1500,] %>% st_transform(crs = 4326) %>% as_Spatial()
p4 <- pop_dat[1501:2173,] %>% st_transform(crs = 4326) %>% as_Spatial()
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

# Visualisation data
vis_dat <- pop_dat %>%
  st_drop_geometry() %>%
  bind_cols(
    # Centroids
    pop_dat %>%
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
  mutate(sh1_x_end = ifelse(direction == "west", sh1_x - 50 * value, sh1_x + 50 * value))

# Create visualisation
vis <- vis_dat %>% 
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

# *****************************************************************************