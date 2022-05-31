library(tidycensus)
library(tidyverse)
library(sf)

PA_st_plane <- "+proj=lcc +lat_1=39.93333333333333 +lat_2=40.96666666666667 +lat_0=39.33333333333334 +lon_0=-77.75 +x_0=600000.0000000001 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

sites <- here("data",
              "sites.csv") %>%
  read_csv(show_col_types = FALSE)

##### Sampling (for quick testing - remove later)
sample_index <- seq(1, 500000, by = 250)

sites <- sites[sample_index,]
############

# parcel_uses <- read_csv('https://data.wprdc.org/dataset/2b3df818-601e-4f06-b150-643557229491/resource/f2b8d575-e256-4718-94ad-1e12239ddb92/download/assessments.csv',
#                         show_col_types = FALSE) %>%
#   select(PARID, USEDESC) 

# parcel_locs <- st_read('https://data.wprdc.org/dataset/6bb2a968-761d-48cf-ac5b-c1fc80b4fe6a/resource/42231cab-8341-48d6-b695-47612dd6514a/download/parcelcoords.csv',
#                        options = c("X_POSSIBLE_NAMES=x", 
#                                    "Y_POSSIBLE_NAMES=y")) %>%
#   rename(PARID = PIN) %>%
#   st_set_crs("WGS84") %>%
#   st_transform(PA_st_plane) %>%
#   inner_join(parcel_uses)

parcel_uses <- read_csv('https://data.wprdc.org/dataset/2b3df818-601e-4f06-b150-643557229491/resource/f2b8d575-e256-4718-94ad-1e12239ddb92/download/assessments.csv',
                                   show_col_types = FALSE) %>%
  select(PARID, USEDESC) 

# create set of buffers for every hundred feet, up to ~ 1 mile

site_locs <- inner_join(parcel_locs, sites) %>%
  st_transform(PA_st_plane) 

site_buffers <- site_locs %>%
  st_buffer(100) %>%
  mutate(radius = 100)

for (i in seq(200, 10600, by=100)) {
  these_buffers <- site_locs %>%
    st_buffer(i) %>%
    mutate(radius = i)
  
  site_buffers <- rbind(site_buffers, these_buffers)
}

vars <- c(popln = "P2_001N",
          hispanic = "P2_002N",
          nh_white = "P2_005N",
          nh_black = "P2_006N")  

# data <- get_decennial(geography = "block",
#                       state = "PA",
#                       county = c("Butler",
#                                  "Beaver",
#                                  "Armstrong",
#                                  "Washington",
#                                  "Allegheny",
#                                  "Westmoreland"),
#                       year = 2020,
#                       variables = vars,
#                       output = "wide",
#                       geometry = TRUE) %>%
#   st_transform(PA_st_plane)

census_indices <- st_intersects(site_buffers, data)

site_buffers$popln <- 0
site_buffers$hispanic <- 0
site_buffers$nh_white <- 0
site_buffers$nh_black <- 0

Sys.time()
## this loop takes 2 hours 40 minutes for 2000 sites
for (i in 1:length(site_buffers$PARID)) {
  site_buffers$popln[i] <-
    sum(data[census_indices[[i]],]$popln)
  
  site_buffers$hispanic[i] <-
    sum(data[census_indices[[i]],]$hispanic)
  
  site_buffers$nh_white[i] <-
    sum(data[census_indices[[i]],]$nh_white)
  
  site_buffers$nh_black[i] <-
    sum(data[census_indices[[i]],]$nh_black)
}
Sys.time()

# How far do you have to go to get 2000 people? How diverse are those 2000 people?
site_buffers <- site_buffers %>%
  mutate(from_2k = abs(2000 - popln)) %>%
  group_by(PARID) %>%
  filter(from_2k == min(from_2k)) %>%
  filter(radius == min(radius)) %>%
  ungroup() %>%
  mutate(pct_white = nh_white / popln,
         pct_black = nh_black / popln,
         pct_hispanic = hispanic / popln) 

Sys.time()
site_indices <- st_intersects(site_buffers, parcel_locs)
Sys.time()


# (after) what is the number of land uses within a 2000-person radius? 


buff_100ft <- site_buffers[site_buffers$radius == 100,]
buff_2mile <- site_buffers[site_buffers$radius == 10600,]

summary(buff_2mile$popln)
summary(buff_100ft$popln)

