library(tidyverse)
library(sf)
library(here)
library(nngeo)

parcels <- here("02_data",
                "all-parcels.csv") %>%
  st_read(options = c("X_POSSIBLE_NAMES=x", 
                      "Y_POSSIBLE_NAMES=y")) %>%
  st_set_crs("WGS84")

big_coal <- parcels %>%
  filter(PARID == "0187E00175000000")

bad_parcels <- parcels %>%
  filter(Category == "disamenity") %>%
  filter((x != big_coal$x &
            y != big_coal$y) |
           PARID == big_coal$PARID)

site_data <- here("02_data",
                  "sites.csv") %>%
  read_csv() %>%
  select(PARID)

sites <- right_join(parcels, site_data)

tenth_closest <- st_nn(sites, 
                       bad_parcels, 
                       k = 10, 
                       returnDist = TRUE)$dist 
  

tenth_closest_df <- data.frame(matrix(unlist(tenth_closest), 
                                      nrow = length(tenth_closest),
                                      byrow = TRUE)) %>%
  mutate(log_avg_disamenity_dist = (log(X1+X2+X3+X4+X5+X6+X7+X8+X9+X10)/10))
  
sites <- sites %>%
  mutate(log_avg_disamentiy_dist = tenth_closest_df$log_avg_disamenity_dist) %>%
  select(PARID, log_avg_disamentiy_dist) %>%
  st_drop_geometry()

write_csv(sites, here("02_data",
                      "bad_proximity.csv"))