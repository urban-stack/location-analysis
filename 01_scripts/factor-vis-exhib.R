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
  read_csv() %>%
  right_join(indices) %>%
  select(PARID, 
         f_drivable,
         f_walkable,
         f_dense,
         f_diverse,
         f_amenities) %>%
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

### Hexbin plots
drive_drive <- ggplot(sites,
                      aes(x = f_drivable)) +
  geom_histogram(bins = 30) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "",
                     sec.axis = dup_axis(name = "Accessibility by driving")) +
  theme_void() +
  theme(axis.text = element_blank(),
        axis.title.x.top = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt")) +
  theme(text = element_text(size = 24))

drive_drive

walk_drive <- ggplot(sites, 
                     aes(x = f_walkable, y = f_drivable)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "",
                     sec.axis = dup_axis(name = "Accessibility by walking")) +
  theme_void()+
  theme(legend.position = "none",
        axis.text.y = element_text(),
        axis.title.x.top = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt")) +
  theme(text = element_text(size = 24))

walk_drive

dense_drive <- ggplot(sites, 
                      aes(x = f_dense, y = f_drivable)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "",
                     sec.axis = dup_axis(name = "Accessibility by cycling")) +
  theme_void()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title.x.top = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

dense_drive

diverse_drive <- ggplot(sites, 
                      aes(x = f_diverse, y = f_drivable)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "",
                     sec.axis = dup_axis(name = "Diversity of people and buildings")) +
  theme_void()+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title.x.top = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

diverse_drive

amenities_drive <- ggplot(sites, 
                        aes(x = f_amenities, y = f_drivable)) +
  scale_y_continuous(name = "",
                     sec.axis = dup_axis(name = "Accessibility by driving")) +
  scale_x_continuous(name = "",
                     sec.axis = dup_axis(name = "Accessibility to shopping")) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  theme_void() +
  theme(legend.position = "none",
        axis.title.y.right = element_text(angle = 270),
        axis.title.x.top = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

drive_walk

walk_walk <- ggplot(sites,
                    aes(x = f_walkable)) +
  geom_histogram(bins = 30) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "") +
  theme_void() +
  theme(axis.text = element_blank(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, "pt"),
        text = element_text(size = 24))

walk_dense

dense_dense <- ggplot(sites,
                      aes(x = f_dense)) +
  geom_histogram(bins = 30) +
  scale_y_continuous(name = "") +
  scale_x_continuous(name = "") +
  theme_void() +
  theme(axis.text = element_blank(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

diverse_dense

amenities_dense <- ggplot(sites, 
                         aes(x = f_amenities, y = f_dense)) +
  scale_x_continuous(name = "  ") +
  scale_y_continuous(name = "  ",
                     sec.axis = dup_axis(name = "Accessibility by cycling")) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  theme_void() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title.y.right = element_text(angle = 270),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
                     sec.axis = dup_axis(name = "Diversity of people and buildings")) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  theme_void() +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title.y.right = element_text(angle = 270),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

dense_amenities

diverse_amenities <- ggplot(sites, 
                            aes(x = f_diverse, y = f_amenities)) +
  scale_x_continuous(name = "  ") +
  scale_y_continuous(name = "  ",
                     sec.axis = dup_axis(name = "Accessibility to shopping")) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  theme_void() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.text.x = element_text(),
        axis.title.y.right = element_text(angle = 270),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

diverse_amenities

amenities_amenities <- ggplot(sites,
                          aes(x = f_amenities)) +
  geom_histogram(bins = 30) +
  scale_y_continuous(name = "",
                     sec.axis = dup_axis(name = "Accessibility to shopping")) +
  scale_x_continuous(name = "") +
  theme_void() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(),
        plot.margin = margin(2, 2, 2, 2, unit = "pt"),
        text = element_text(size = 24))

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
  scale_fill_viridis_c(name = "Number of parcels",
                       trans = "log",
                       limits = c(1, 15000),
                       breaks = 10^seq(-1, 4, by=1)) +
  theme_minimal() +
  theme(text = element_text(size = 24))

legend <- get_legend(legend_plot)

cor_w_legend <- ggdraw() +
  draw_plot(cor_mat, x = 0, y = 0, width = 0.8, height = 1) +
  draw_plot(legend, x = 0.8, y= 0, width = 0.2, height = 1) +
  theme(text = element_text(size = 18))


here("exhibition",
     "factor-cor.pdf") %>%
  ggsave(plot = cor_w_legend,
         height = 24,
         width = 30,
         units = "in")
