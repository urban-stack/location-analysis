options(java.parameters = "-Xmx2G")

library(arrow)
library(here)
library(r5r)
library(sf)
library(tidyverse)
library(tigris)

sites <- here("02_data",
              "sites.csv") %>%
  read_csv(show_col_types = FALSE)

uses <- here("02_data",
             "uses.csv") %>%
  read_csv(show_col_types = FALSE)

# load parcel locations data
parcels <- st_read('https://data.wprdc.org/dataset/6bb2a968-761d-48cf-ac5b-c1fc80b4fe6a/resource/42231cab-8341-48d6-b695-47612dd6514a/download/parcelcoords.csv',
                   options = c("X_POSSIBLE_NAMES=x", 
                               "Y_POSSIBLE_NAMES=y")) %>%
  rename(id = PIN) %>%
  st_set_crs("WGS84")

site_locs <- parcels %>%
  filter(id %in% sites$PARID)

# #################################################
# Sample 100 sites for test (delete once we've confirmed it all works)
# sample_n <- 10
# sample_inc <- round(length(site_locs$id) / sample_n)
# sample_indices <- seq(1, sample_inc * sample_n, by = sample_inc)
# 
# site_locs <- site_locs[sample_indices,]
# 
# ggplot(site_locs) +
#   geom_sf()
# ###################################################

# Load destinations from parcel data
dest_locs <- read_csv('https://data.wprdc.org/dataset/2b3df818-601e-4f06-b150-643557229491/resource/f2b8d575-e256-4718-94ad-1e12239ddb92/download/assessments.csv',
                      show_col_types = FALSE) %>%
  select(PARID, 
         USEDESC) %>%
  inner_join(parcels, by = c("PARID" = "id")) %>%
  left_join(uses) %>%
  filter(Category == "destination") %>%
  mutate(total_opps = 1000) %>%
  rename(id = PARID,
         lat = y,
         lon = x)

# load grocery locations
grocery_pts <- st_read("https://openac-alcogis.opendata.arcgis.com/datasets/ab9ec54e46d8403db31cff6bdc890aff_0.geojson") %>%
  filter(!is.na(st_dimension(.))) %>%
  mutate(id = as.character(OBJECTID)) %>%
  mutate(total_opps = 1000) %>%
  select(id, total_opps)

# load school locations (https://openac-alcogis.opendata.arcgis.com/datasets/AlCoGIS::allegheny-county-public-schools-local-education-agency-leas-locations/about)
school_pts <- st_read(here("data",
                           "schools.geojson")) %>%
  mutate(id = as.character(FID)) %>%
  mutate(total_opps = 1000) %>%
  select(id, total_opps)

# load park locations (https://www.pasda.psu.edu/uci/DataSummary.aspx?dataset=308)
county <- counties(state = "PA") %>%
  filter(COUNTYFP == "003") %>%
  st_transform("WGS84")
  
park_pts <- st_read(here("data",
                         "DCNR_LocalParkAccess201511")) %>%
  st_transform("WGS84") %>%
  st_filter(county) %>%
  mutate(id = as.character(PARK_ID)) %>%
  mutate(total_opps = 1000) %>%
  select(id, total_opps)

# Load LEHD data
jobs <- here("data",
             "pa_wac_S000_JT00_2019.csv.gz") %>%
  read_csv(show_col_types = FALSE) %>%
  mutate(GEOID10 = as.character(w_geocode)) %>%
  mutate(total_opps = C000 * 1000,
         lo_pay_jobs = CE01 * 1000,
         hi_pay_jobs = CE03 * 1000,
         retail_jobs = CNS07 * 1000,
         entrnmt_jobs = CNS17 * 1000,
         hosplty_jobs = CNS18 * 1000) 

# Get job locations (block centroids)
job_locs <- blocks(county = c("Allegheny",
                              "Westmoreland", 
                              "Washington", 
                              "Butler", 
                              "Beaver", 
                              "Armstrong", 
                              "Fayette"),
                   state = "PA",
                   year = 2010) %>%
  inner_join(jobs) %>%
  rename(id = GEOID10) %>%
  select(id,
         total_opps,
         lo_pay_jobs,
         hi_pay_jobs,
         retail_jobs,
         entrnmt_jobs,
         hosplty_jobs) %>%
  st_centroid() %>%
  st_transform("WGS84")

# Subset locations with each job type
hi_pay_job_locs <- job_locs %>%
  filter(hi_pay_jobs > 0) %>%
  select(id, hi_pay_jobs) %>%
  rename(total_opps = hi_pay_jobs)

lo_pay_job_locs <- job_locs %>%
  filter(lo_pay_jobs > 0) %>%
  select(id, lo_pay_jobs) %>%
  rename(total_opps = lo_pay_jobs)

retail_job_locs <- job_locs %>%
  filter(retail_jobs > 0) %>%
  select(id, retail_jobs) %>%
  rename(total_opps = retail_jobs)

entrnmt_job_locs <- job_locs %>%
  filter(entrnmt_jobs > 0) %>%
  select(id, entrnmt_jobs) %>%
  rename(total_opps = entrnmt_jobs)

hosplty_job_locs <- job_locs %>%
  filter(hosplty_jobs > 0) %>%
  select(id, hosplty_jobs) %>%
  rename(total_opps = hosplty_jobs)

dest_data <- list(job_locs,
                  hi_pay_job_locs,
                  lo_pay_job_locs,
                  retail_job_locs,
                  entrnmt_job_locs,
                  hosplty_job_locs,
                  dest_locs,
                  grocery_pts,
                  school_pts,
                  park_pts)

dest_labels <- c("all_jobs",
                 "hi_pay_jobs",
                 "lo_pay_jobs",
                 "retail_jobs",
                 "entrnmt_jobs",
                 "hosplty_jobs",
                 "dest_parcels",
                 "grocery",
                 "school",
                 "park")

# Start R5 core
r5_core <- here("03_network") %>%
  setup_r5(verbose = FALSE)

# LTS 2 Bike access

#### Computing time:
## All jobs: 3.25 hours
## High-paying jobs: 3.25 hours
## Low-paying jobs: 3.25 hours
## Retail jobs: 3 hours
## Entertainment jobs: 3 hours
## Hospitality jobs: 3 hours
## Destination parcels: 4 hours
## Grocery: 3 hours
## School: 3 hours
## Parks: 3 hours

access <- accessibility(r5_core,
                       origins = site_locs,
                       destinations = job_locs,
                       opportunities_colname = "total_opps",
                       mode = "BICYCLE",
                       max_lts = 2,
                       max_trip_duration = 50,
                       decay_function = "logistic",
                       cutoffs = 20,
                       decay_value = 5,
                       verbose = FALSE) %>%
  mutate(dest_type = "all_jobs") %>%
  mutate(mode = "bike")

for(i in 2:10) {
  print(paste("starting bike", dest_labels[i], Sys.time()))
  
  this_access <- accessibility(r5_core,
                               origins = site_locs,
                               destinations = dest_data[[i]],
                               opportunities_colname = "total_opps",
                               mode = "BICYCLE",
                               max_lts = 2,
                               max_trip_duration = 50,
                               decay_function = "logistic",
                               cutoffs = 20,
                               decay_value = 5,
                               verbose = FALSE) %>%
    mutate(dest_type = dest_labels[i]) %>%
    mutate(mode = "bike")
  
  access <- rbind(access, this_access)
  
  print(paste("done with bike", dest_labels[i], Sys.time()))
}

# walk

#### Computing time:
## All jobs: 30 minutes
## High-paying jobs: 25 minutes
## Low-paying jobs: 28 minutes
## Retail jobs: 15 minutes
## Entertainment jobs: 15 minutes
## Hospitality jobs: 18 minutes
## Destination parcels: 40 minutes
## Grocery: 14 minutes
## School: 14 minutes
## Parks: 14 minutes

for(i in 1:10) {
  print(paste("starting walk", dest_labels[i], Sys.time()))
  
  this_access <- accessibility(r5_core,
                               origins = site_locs,
                               destinations = dest_data[[i]],
                               opportunities_colname = "total_opps",
                               mode = "WALK",
                               max_lts = 2,
                               max_trip_duration = 50,
                               decay_function = "logistic",
                               cutoffs = 20,
                               decay_value = 5,
                               verbose = FALSE) %>%
    mutate(dest_type = dest_labels[i]) %>%
    mutate(mode = "walk")
  
  access <- rbind(access, this_access)
  
  print(paste("done with walk", dest_labels[i], Sys.time()))
}

# drive

#### Computing time:
## All jobs: 2.5 hours
## High-paying jobs: 2.5 hours
## Low-paying jobs: 2.5 hours
## Retail jobs: 2.5 hours
## Entertainment jobs: 2 hours
## Hospitality jobs: 2 hours
## Destination parcels: 3 hours
## Grocery: 2 hours
## School: 2 hours
## Parks: 2 hours

for(i in 1:10) {
  print(paste("starting car", dest_labels[i], Sys.time()))
  
  this_access <- accessibility(r5_core,
                               origins = site_locs,
                               destinations = dest_data[[i]],
                               opportunities_colname = "total_opps",
                               mode = "CAR",
                               max_trip_duration = 10,
                               decay_function = "logistic",
                               cutoffs = 40,
                               decay_value = 10,
                               verbose = FALSE) %>%
    mutate(dest_type = dest_labels[i]) %>%
    mutate(mode = "car")
  
  access <- rbind(access, this_access)
  
  print(paste("done with car", dest_labels[i], Sys.time()))
}

# transit

#### Computing time:
## All jobs: 43 minutes
## High-paying jobs: 33 minutes
## Low-paying jobs: 35 minutes
## Retail jobs: 9 minutes
## Entertainment jobs: 5 minutes
## Hospitality jobs: 11 minutes
## Destination parcels: 2 hours
## Grocery: 3 minutes
## School: 4 minutes
## Parks: 6 minutes
for(i in 1:10) {
  print(paste("starting transit", dest_labels[i], Sys.time()))
  
  this_access <- accessibility(r5_core,
                               origins = site_locs,
                               destinations = dest_data[[i]],
                               opportunities_colname = "total_opps",
                               mode = "TRANSIT",
                               max_trip_duration = 10,
                               decay_function = "logistic",
                               cutoffs = 40,
                               decay_value = 10,
                               verbose = FALSE) %>%
    mutate(dest_type = dest_labels[i]) %>%
    mutate(mode = "transit")
  
  access <- rbind(access, this_access)
  
  print(paste("done with transit", dest_labels[i], Sys.time()))
}

access_wide <- access %>%
  mutate(access_type = paste0("access_",
                              mode,
                              "_",
                              dest_type)) %>%
  rename(PARID = from_id) %>%
  select(PARID, accessibility, access_type) %>%
  pivot_wider(id_cols = PARID, 
              names_from = access_type, 
              values_from = accessibility)

write_parquet(access_wide,
          here("data",
               "access.parquet"))