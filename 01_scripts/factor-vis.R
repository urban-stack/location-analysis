###########

library(here)
library(tidyverse)
library(sf)
library(tigris)
library(ggthemes)
library(cowplot)
library(hexbin)
library(RColorBrewer)
library(tigris)


PA_st_plane <- "+proj=lcc +lat_1=39.93333333333333 +lat_2=40.96666666666667 +lat_0=39.33333333333334 +lon_0=-77.75 +x_0=600000.0000000001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

small_ylims <- c(40.44283, 40.45773)
small_xlims <- c(-79.95425, -79.933813)

small_box_coords <- tibble(x = small_xlims, 
                           y = small_ylims) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs("WGS84") %>%
  st_transform(PA_st_plane)

#get the bounding box of the two x & y coordintates, make sfc
small_bounding_box <- st_bbox(small_box_coords) %>% st_as_sfc()


indices <- here("02_data",
                "indices.csv") %>%
  read_csv()

weights <- here("02_data",
                "model_coefs.csv") %>%
  read_csv() %>%
  filter(variable != "(Intercept)") %>%
  mutate(weight = value / max(value))

sites <- here("02_data",
             "all-parcels.csv") %>%
  st_read(options = c("X_POSSIBLE_NAMES=x", 
                      "Y_POSSIBLE_NAMES=y")) %>%
  right_join(indices) %>%
  select(PARID, 
         f_drivable,
         f_walkable,
         f_dense,
         f_diverse,
         f_amenities) %>%
  st_set_crs("WGS84") %>%
  mutate(combined_index = 
           f_drivable * weights$weight[weights$variable == "f_drivable"] +
           f_walkable * weights$weight[weights$variable == "f_walkable"] +
           f_dense * weights$weight[weights$variable == "f_dense"] +
           f_diverse * weights$weight[weights$variable == "f_diverse"]) %>%
  mutate(f_drivable = case_when(f_drivable > 3 ~ 3,
                                f_drivable < -3 ~ -3,
                                TRUE ~ f_drivable)) %>%
  mutate(f_walkable = case_when(f_walkable > 3 ~ 3,
                                f_walkable < -3 ~ -3,
                                TRUE ~ f_walkable)) %>%
  mutate(f_dense = case_when(f_dense > 3 ~ 3,
                                f_dense < -3 ~ -3,
                                TRUE ~ f_dense)) %>%
  mutate(f_diverse = case_when(f_diverse > 3 ~ 3,
                             f_diverse < -3 ~ -3,
                             TRUE ~ f_diverse)) %>%
  mutate(f_amenities = case_when(f_amenities > 3 ~ 3,
                             f_amenities < -3 ~ -3,
                             TRUE ~ f_amenities))

here("02_data",
     "indices-vis.csv.gz") %>%
  write_csv(x = sites, file=gzfile(.))

sites <- sites %>%
  st_transform(PA_st_plane) 

rm(indices)

inset_sites <- sites[small_bounding_box,]

map_palette <- brewer.pal(9, "Spectral")

# drivable map
drivable_big <- ggplot(sites) +
  geom_sf(aes(color = f_drivable),
          size = 1,
          pch = ".") +
  scale_color_gradient2(
    name = "",
    low = map_palette[1],
    mid = map_palette[5],
    high = map_palette[9],
    midpoint = 0,
    breaks = c(-3, 0, 3),
    labels = c("> 3 standard deviations\nbelow average",
               "Average drivability",
               "> 3 standard deviations\nabove average")) +
  ggthemes::theme_map() +
  theme(legend.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        plot.background = element_rect(fill = 'white', 
                                       colour = 'white')) +
  geom_sf(data = small_bounding_box, 
          fill = NA, 
          color = "black", 
          size = 0.5) 

drivable_big

drivable_small <- ggplot(inset_sites) +
  geom_sf(aes(color = f_drivable),
          size = 1,
          pch = ".") +
  scale_color_gradient2(
    name = "",
    low = map_palette[1],
    mid = map_palette[5],
    high = map_palette[9],
    midpoint = 0) +
  theme_map() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white")) 

drivable_small

drivable_inset = ggdraw() +
  draw_plot(drivable_big) +
  draw_plot(drivable_small, 
            x = 0.02, 
            y = 0.63, 
            width = 0.35, 
            height = 0.35)

drivable_inset

here("04_figures",
     "drivable.png") %>%
  ggsave(plot = drivable_inset, 
         width = 4,
         height = 4,
         units = "in")

here("04_figures",
     "vector-figs",
     "drivable.pdf") %>%
  ggsave(plot = drivable_inset, 
         width = 12,
         height = 12,
         units = "in")

# walkable map
walkable_big <- ggplot(sites) +
  geom_sf(aes(color = f_walkable),
          size = 1,
          pch = ".") +
  scale_color_gradient2(
    name = "",
    low = map_palette[1],
    mid = map_palette[5],
    high = map_palette[9],
    midpoint = 0,
    breaks = c(-3, 0, 3),
    labels = c("> 3 standard deviations\nbelow average",
               "Average walkability",
               "> 3 standard deviations\nabove average")) +
  ggthemes::theme_map() +
  theme(legend.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        plot.background = element_rect(fill = 'white', 
                                       colour = 'white')) +
  geom_sf(data = small_bounding_box, 
          fill = NA, 
          color = "black", 
          size = 0.5) 

walkable_big

walkable_small <- ggplot(inset_sites) +
  geom_sf(aes(color = f_walkable),
          size = 1,
          pch = ".") +
  scale_color_gradient2(
    low = map_palette[1],
    mid = map_palette[5],
    high = map_palette[9],
    midpoint = 0) +
  theme_map() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white")) 

walkable_small

walkable_inset = ggdraw() +
  draw_plot(walkable_big) +
  draw_plot(walkable_small, 
            x = 0.02, 
            y = 0.63, 
            width = 0.35, 
            height = 0.35)

walkable_inset

here("04_figures",
     "walkable.png") %>%
  ggsave(plot = walkable_inset, 
         width = 4,
         height = 4,
         units = "in")

here("04_figures",
     "vector-figs",
     "walkable.pdf") %>%
  ggsave(plot = walkable_inset, 
         width = 12,
         height = 12,
         units = "in")

# dense map
dense_big <- ggplot(sites) +
  geom_sf(aes(color = f_dense),
          size = 1,
          pch = ".") +
  scale_color_gradient2(
    name = "",
    breaks = c(-3, 0, 3),
    labels = c("> 3 standard deviations\nbelow average",
               "Average density",
               "> 3 standard deviations\nabove average"),
    low = map_palette[1],
    mid = map_palette[5],
    high = map_palette[9],
    midpoint = 0) +
  ggthemes::theme_map() +
  theme(legend.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        plot.background = element_rect(fill = 'white', 
                                       colour = 'white')) +
  geom_sf(data = small_bounding_box, 
          fill = NA, 
          color = "black", 
          size = 0.5) 

dense_big

dense_small <- ggplot(inset_sites) +
  geom_sf(aes(color = f_dense),
          size = 1,
          pch = ".") +
  scale_color_gradient2(low = map_palette[1],
                        mid = map_palette[5],
                        high = map_palette[9],
                        midpoint = 0) +
  theme_map() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"))

dense_small

dense_inset = ggdraw() +
  draw_plot(dense_big) +
  draw_plot(dense_small, 
            x = 0.02, 
            y = 0.65, 
            width = 0.35, 
            height = 0.35)

dense_inset

here("04_figures",
     "dense.png") %>%
  ggsave(plot = dense_inset, 
         width = 4,
         height = 4,
         units = "in")

# diverse map
diverse_big <- ggplot(sites) +
  geom_sf(aes(color = f_diverse),
          size = 1,
          pch = ".") +
  scale_color_gradient2(
    name = "",
    breaks = c(-3, 0, 3),
    labels = c("> 3 standard deviations\nbelow average",
               "Average diversity",
               "> 3 standard deviations\nabove average"),
    low = map_palette[1],
    mid = map_palette[5],
    high = map_palette[9],
    midpoint = 0) +
  ggthemes::theme_map() +
  theme(legend.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        plot.background = element_rect(fill = 'white', 
                                       colour = 'white')) +
  geom_sf(data = small_bounding_box, 
          fill = NA, 
          color = "black", 
          size = 0.5) 

diverse_big

diverse_small <- ggplot(inset_sites) +
  geom_sf(aes(color = f_diverse),
          size = 1,
          pch = ".") +
  scale_color_gradient2(low = map_palette[1],
                        mid = map_palette[5],
                        high = map_palette[9],
                        midpoint = 0) +
  theme_map() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white")) 

diverse_small

diverse_inset = ggdraw() +
  draw_plot(diverse_big) +
  draw_plot(diverse_small, 
            x = 0.02, 
            y = 0.65, 
            width = 0.35, 
            height = 0.35)

diverse_inset

here("04_figures",
     "diverse.png") %>%
  ggsave(plot = diverse_inset, 
         width = 4,
         height = 4,
         units = "in")

# amenities map
amenities_big <- ggplot(sites) +
  geom_sf(aes(color = f_amenities),
          size = 1,
          pch = ".") +
  scale_color_gradient2(
    name = "",
    breaks = c(-3, 0, 3),
    labels = c("> 3 standard deviations\nbelow average",
               "Average amenity richness",
               "> 3 standard deviations\nabove average"),
    low = map_palette[1],
    mid = map_palette[5],
    high = map_palette[9],
    midpoint = 0) +
  ggthemes::theme_map() +
  theme(legend.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        plot.background = element_rect(fill = 'white', 
                                       colour = 'white')) +
  geom_sf(data = small_bounding_box, 
          fill = NA, 
          color = "black", 
          size = 0.5) 

amenities_big

amenities_small <- ggplot(inset_sites) +
  geom_sf(aes(color = f_amenities),
          size = 1,
          pch = ".") +
  scale_color_gradient2(low = map_palette[1],
                        mid = map_palette[5],
                        high = map_palette[9],
                        midpoint = 0) +
  ggthemes::theme_map() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white")) 

amenities_small

amenities_inset = ggdraw() +
  draw_plot(amenities_big) +
  draw_plot(amenities_small, 
            x = 0.02, 
            y = 0.65, 
            width = 0.35, 
            height = 0.35)

amenities_inset

here("04_figures",
     "amenities.png") %>%
  ggsave(plot = amenities_inset, 
         width = 4,
         height = 4,
         units = "in")

# Combined map
combined_big <- ggplot(sites) +
  geom_sf(aes(color = combined_index),
          size = 1,
          pch = ".") +
  scale_color_gradient2(
    name = "",
    breaks = c(-1.5446, 0, 3.91),
    labels = c("Minimum development likelihood",
               "Average development likelihood",
               "Maximum development likelihood"),
    low = map_palette[1],
    mid = map_palette[5],
    high = map_palette[9],
    midpoint = 0) +
  ggthemes::theme_map() +
  theme(legend.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        plot.background = element_rect(fill = 'white', 
                                       colour = 'white')) +
  geom_sf(data = small_bounding_box, 
          fill = NA, 
          color = "black", 
          size = 0.5) 

combined_big

combined_small <- ggplot(inset_sites) +
  geom_sf(aes(color = combined_index),
          size = 1,
          pch = ".") +
  scale_color_gradient2(low = map_palette[1],
                        mid = map_palette[5],
                        high = map_palette[9],
                        midpoint = 0) +
  ggthemes::theme_map() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white")) 

combined_small

combined_inset = ggdraw() +
  draw_plot(combined_big) +
  draw_plot(combined_small, 
            x = 0.02, 
            y = 0.65, 
            width = 0.35, 
            height = 0.35)

combined_inset

here("04_figures",
     "combined.png") %>%
  ggsave(plot = combined_inset, 
         width = 4,
         height = 4,
         units = "in")


###############
# Show Pittsburgh permit locations

months <- c("july-2021",
            "august-2021",
            "september-2021",
            "october-2021",
            "november-2021",
            "december-2021",
            "january-2022",
            "february-2022",
            "march-2022",
            "april-2022",
            "may-2022")


permits <- here("02_data",
                "pli-permit-summary",
                "pli-permit-summary-june-2021.xlsx") %>%
  read_xlsx(sheet = 1) %>%
  select(3, 9, 10) %>%
  rename(parcel_number = 1,
         work_type = 2, 
         structure_type = 3)


for (i in 1:11) {
  next_permits <- here("02_data",
                       "pli-permit-summary",
                       paste0("pli-permit-summary-",
                              months[i],
                              ".xlsx")) %>%
    read_xlsx(sheet = 1) %>%
    select(3, 9, 10) %>%
    rename(parcel_number = 1,
           work_type = 2, 
           structure_type = 3)
  
  permits <- rbind(permits, next_permits)
  
}

res_permits <- permits %>%
  mutate(PARID = paste0(substr(parcel_number, 1, 4),
                        substr(parcel_number, 6, 6),
                        substr(parcel_number, 8, 12),
                        substr(parcel_number, 14, 17),
                        substr(parcel_number, 19, 20))) %>%
  filter(work_type == "NEW CONSTRUCTION" |
           work_type == "COMPLETE DEMOLITION" |
           work_type == "NEW" |
           work_type == "NEW CONSTRUCTION" |
           work_type == "REPLACEMENT" |
           work_type == "PARTIAL DEMOLITION") %>%
  filter(structure_type == "Residential" |
           structure_type == "Residential - Two-Family")

permit_sites <- unique(res_permits$PARID)

permit_site_locs <- sites %>%
  filter(PARID %in% permit_sites) %>%
  mutate(constant = "constant")

pitt_boundary <- places(state = "PA") %>% 
  filter(NAME == "Pittsburgh") %>%
  st_transform(PA_st_plane)

pitt_sites <- sites[pitt_boundary,] 

# Combined map (just Pittsburgh)
combined_pitt <- ggplot(pitt_sites) +
  geom_sf(aes(color = combined_index),
          size = 1,
          pch = ".") +
  geom_sf(data = permit_site_locs,
          aes(shape = constant),
          size = 0.5) +
  scale_shape(name = "",
              label = "Building permit for\nnew construction or demolition") +
  scale_color_gradient2(
    name = "",
    breaks = c(-1.15, 1.09, 3.91),
    labels = c("Minimum development likelihood\n(for Pittsburgh)",
               "Average development likelihood\n(for Pittsburgh)",
               "Maximum development likelihood\n(for Pittsburgh)"),
    low = map_palette[1],
    mid = map_palette[5],
    high = map_palette[9],
    midpoint = 1.09) +
  theme_void() +
  theme(legend.background = element_blank(),
        panel.background = element_rect(fill = "white",
                                        color = "white"),
        plot.background = element_rect(fill = 'white', 
                                       colour = 'white'))

combined_pitt

here("04_figures",
     "combined-pitt.png") %>%
  ggsave(plot = combined_pitt, 
         width = 6,
         height = 4,
         units = "in")

here("04_figures",
     "vector-figs",
     "combined-pitt.pdf") %>%
  ggsave(plot = combined_pitt, 
         width = 12,
         height = 8,
         units = "in")

combined_hist <- ggplot(sites,
                        aes(x = combined_index)) +
  geom_histogram(bins = 30,
                 color = "lightgray",
                 fill = "gray") +
  scale_y_continuous(name = "Number of sites") +
  scale_x_continuous(name = "Development likelihood index") +
  theme_minimal()

combined_hist

here("04_figures",
     "combined-hist.png") %>%
  ggsave(plot = combined_hist, 
         width = 4,
         height = 3,
         units = "in")

### Hexbin plots
drive_drive <- ggplot(sites,
                      aes(x = f_drivable)) +
  geom_histogram(bins = 30) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "",
                     sec.axis = dup_axis(name = "Drivable")) +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.title.x.top = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

drive_drive

walk_drive <- ggplot(sites, 
                     aes(x = f_walkable, y = f_drivable)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "",
                     sec.axis = dup_axis(name = "Walkable")) +
  theme_void()+
  theme(legend.position = "none",
        axis.text.y = element_text(),
        axis.title.x.top = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

walk_drive

dense_drive <- ggplot(sites, 
                      aes(x = f_dense, y = f_drivable)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "",
                     sec.axis = dup_axis(name = "Dense")) +
  theme_void()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title.x.top = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

dense_drive

diverse_drive <- ggplot(sites, 
                      aes(x = f_diverse, y = f_drivable)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "",
                     sec.axis = dup_axis(name = "Diverse")) +
  theme_void()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title.x.top = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

diverse_drive

amenities_drive <- ggplot(sites, 
                        aes(x = f_amenities, y = f_drivable)) +
  scale_y_continuous(name = "",
                     sec.axis = dup_axis(name = "Drivable")) +
  scale_x_continuous(name = "",
                     sec.axis = dup_axis(name = "Amenities")) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  theme_void() +
  theme(legend.position = "none",
        axis.title.y.right = element_text(angle = 270),
        axis.title.x.top = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

amenities_drive

drive_walk <- ggplot(sites, 
                     aes(x = f_drivable, y = f_walkable)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "  ") +
  scale_x_continuous(name = "  ") +
  theme_void()+
  theme(legend.position = "none",
        axis.text.y = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

drive_walk

walk_walk <- ggplot(sites,
                    aes(x = f_walkable)) +
  geom_histogram(bins = 30) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "") +
  theme_void() +
  theme(axis.text = element_blank(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

walk_walk

dense_walk <- ggplot(sites, 
                      aes(x = f_dense, y = f_walkable)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "  ") +
  scale_x_continuous(name = "  ") +
  theme_void()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

dense_walk

diverse_walk <- ggplot(sites, 
                        aes(x = f_diverse, y = f_walkable)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "  ") +
  scale_x_continuous(name = "  ") +
  theme_void()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

diverse_walk

amenities_walk <- ggplot(sites, 
                          aes(x = f_amenities, y = f_walkable)) +
  scale_x_continuous(name = "  ") +
  scale_y_continuous(name = "  ",
                     sec.axis = dup_axis(name = "Walkable")) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  theme_void() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title.y.right = element_text(angle = 270),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

amenities_walk

drive_dense <- ggplot(sites, 
                     aes(x = f_drivable, y = f_dense)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "  ") +
  scale_x_continuous(name = "  ") +
  theme_void()+
  theme(legend.position = "none",
        axis.text.y = element_text(),
        plot.margin = margin(2, 2, 2, 2, "pt"))

drive_dense

walk_dense <- ggplot(sites, 
                     aes(x = f_walkable, y = f_dense)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "  ") +
  scale_x_continuous(name = "  ") +
  theme_void()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        plot.margin = margin(2, 2, 2, 2, "pt"))

walk_dense

dense_dense <- ggplot(sites,
                      aes(x = f_dense)) +
  geom_histogram(bins = 30) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "") +
  theme_void() +
  theme(axis.text = element_blank(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

dense_dense

diverse_dense <- ggplot(sites, 
                       aes(x = f_diverse, y = f_dense)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "  ") +
  scale_x_continuous(name = "  ") +
  theme_void()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

diverse_dense

amenities_dense <- ggplot(sites, 
                         aes(x = f_amenities, y = f_dense)) +
  scale_x_continuous(name = "  ") +
  scale_y_continuous(name = "  ",
                     sec.axis = dup_axis(name = "Dense")) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  theme_void() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title.y.right = element_text(angle = 270),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

amenities_dense

drive_diverse <- ggplot(sites, 
                      aes(x = f_drivable, y = f_diverse)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "  ") +
  scale_x_continuous(name = "  ") +
  theme_void()+
  theme(legend.position = "none",
        axis.text.y = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

drive_diverse

walk_diverse <- ggplot(sites, 
                     aes(x = f_walkable, y = f_diverse)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "  ") +
  scale_x_continuous(name = "  ") +
  theme_void()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

walk_diverse

dense_diverse <- ggplot(sites, 
                        aes(x = f_dense, y = f_diverse)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "  ") +
  scale_x_continuous(name = "  ") +
  theme_void()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

dense_diverse

diverse_diverse <- ggplot(sites,
                          aes(x = f_diverse)) +
  geom_histogram(bins = 30) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "") +
  theme_void() +
  theme(axis.text = element_blank(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

diverse_diverse

amenities_diverse <- ggplot(sites, 
                          aes(x = f_amenities, y = f_diverse)) +
  scale_x_continuous(name = "  ") +
  scale_y_continuous(name = "  ",
                     sec.axis = dup_axis(name = "Diverse")) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  theme_void() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title.y.right = element_text(angle = 270),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

amenities_diverse

drive_amenities <- ggplot(sites, 
                        aes(x = f_drivable, y = f_amenities)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "  ") +
  scale_x_continuous(name = "  ") +
  theme_void()+
  theme(legend.position = "none",
        axis.text.x = element_text(),
        axis.text.y = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

drive_amenities

walk_amenities <- ggplot(sites, 
                       aes(x = f_walkable, y = f_amenities)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "  ") +
  scale_x_continuous(name = "") +
  theme_void()+
  theme(legend.position = "none",
        axis.text.x = element_text(),
        axis.text.y = element_blank(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

walk_amenities

dense_amenities <- ggplot(sites, 
                        aes(x = f_dense, y = f_amenities)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "") +
  theme_void()+
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

dense_amenities

diverse_amenities <- ggplot(sites, 
                            aes(x = f_diverse, y = f_amenities)) +
  scale_x_continuous(name = "  ") +
  scale_y_continuous(name = "  ",
                     sec.axis = dup_axis(name = "Amenities")) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  theme_void() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(),
        axis.title.y.right = element_text(angle = 270),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

diverse_amenities

amenities_amenities <- ggplot(sites,
                          aes(x = f_amenities)) +
  geom_histogram(bins = 30) +
  scale_y_continuous(name = "",
                     sec.axis = dup_axis(name = "Amenities")) +
  scale_x_continuous(name = "") +
  theme_void() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"))

amenities_amenities


cor_mat <- ggdraw() +
  draw_plot(drive_drive,
            x = 0, y = 0.8, height = 0.2, width = 0.2) +
  draw_plot(walk_drive,
            x = 0.2, y = 0.8, height = 0.2, width = 0.2) +
  draw_plot(dense_drive,
            x = 0.4, y = 0.8, height = 0.2, width = 0.2) +
  draw_plot(diverse_drive,
            x = 0.6, y = 0.8, height = 0.2, width = 0.2) +
  draw_plot(amenities_drive,
            x = 0.8, y = 0.8, height = 0.2, width = 0.2) +
  draw_plot(drive_walk,
            x = 0, y = 0.6, height = 0.2, width = 0.2) +
  draw_plot(walk_walk,
            x = 0.2, y = 0.6, height = 0.2, width = 0.2) +
  draw_plot(dense_walk,
            x = 0.4, y = 0.6, height = 0.2, width = 0.2) +
  draw_plot(diverse_walk,
            x = 0.6, y = 0.6, height = 0.2, width = 0.2) +
  draw_plot(amenities_walk,
            x = 0.8, y = 0.6, height = 0.2, width = 0.2) +
  draw_plot(drive_dense,
            x = 0, y = 0.4, height = 0.2, width = 0.2) +
  draw_plot(walk_dense,
            x = 0.2, y = 0.4, height = 0.2, width = 0.2) +
  draw_plot(dense_dense,
            x = 0.4, y = 0.4, height = 0.2, width = 0.2) +
  draw_plot(diverse_dense,
            x = 0.6, y = 0.4, height = 0.2, width = 0.2) +
  draw_plot(amenities_dense,
            x = 0.8, y = 0.4, height = 0.2, width = 0.2) +
  draw_plot(drive_diverse,
            x = 0, y = 0.2, height = 0.2, width = 0.2) +
  draw_plot(walk_diverse,
            x = 0.2, y = 0.2, height = 0.2, width = 0.2) +
  draw_plot(dense_diverse,
            x = 0.4, y = 0.2, height = 0.2, width = 0.2) +
  draw_plot(diverse_diverse,
            x = 0.6, y = 0.2, height = 0.2, width = 0.2) +
  draw_plot(amenities_diverse,
            x = 0.8, y = 0.2, height = 0.2, width = 0.2) +
  draw_plot(drive_amenities,
            x = 0, y = 0, height = 0.2, width = 0.2) +
  draw_plot(walk_amenities,
            x = 0.2, y = 0, height = 0.2, width = 0.2) +
  draw_plot(dense_amenities,
            x = 0.4, y = 0, height = 0.2, width = 0.2) +
  draw_plot(diverse_amenities,
            x = 0.6, y = 0, height = 0.2, width = 0.2) +
  draw_plot(amenities_amenities,
            x = 0.8, y = 0, height = 0.2, width = 0.2)

legend_plot <- ggplot(sites, aes(x = f_amenities, y = f_diverse)) +
  geom_hex() +
  scale_fill_viridis_c(name = "Number of\nparcels",
                       trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  theme_minimal()

legend <- get_legend(legend_plot)

cor_w_legend <- ggdraw() +
  draw_plot(cor_mat, x = 0, y = 0, width = 0.8, height = 1) +
  draw_plot(legend, x = 0.8, y= 0, width = 0.2, height = 1)

here("04_figures",
     "factor-cor.png") %>%
  ggsave(plot = cor_w_legend,
       height = 5.2,
       width = 6.5,
       units = "in")

here("04_figures",
     "vector-figs",
     "factor-cor.pdf") %>%
  ggsave(plot = cor_w_legend,
         height = 9.5,
         width = 12,
         units = "in")
