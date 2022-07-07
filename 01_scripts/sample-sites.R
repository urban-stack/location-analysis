## This script generates a random sample of n sites and join
## that sample to census data

library(tidyverse)
library(tidycensus)
library(sf)
library(here)

`%!in%` <- Negate(`%in%`)

sites <- here("02_data",
              "sites.csv") %>%
  read_csv(show_col_types = FALSE)

# load parcel locations data
parcels <- st_read('https://data.wprdc.org/dataset/6bb2a968-761d-48cf-ac5b-c1fc80b4fe6a/resource/42231cab-8341-48d6-b695-47612dd6514a/download/parcelcoords.csv',
                   options = c("X_POSSIBLE_NAMES=x",
                               "Y_POSSIBLE_NAMES=y")) %>%
  rename(id = PIN) %>%
  st_set_crs("WGS84")

initial_sites <- here("02_data",
                      "sample_sites_tract_data.csv") %>%
  read_csv() %>%
  select(id, lat, lon) %>%
  rename(x = lon, y = lat)

n_sites <- 4001

sample_locs <- parcels %>%
  filter(id %in% sites$PARID) %>%
  filter(id %!in% initial_sites$id) %>%
  sample_n(n_sites) %>%
  st_drop_geometry() %>%
  rbind(initial_sites)

write_csv(sample_locs, 
          file = here("02_data",
                      "sample-sites-5k.csv"))

## Yixin: Feel free to select additional census variables (if you want).
## You can see a list of available variables by typing:
# View(load_variables(2020, "acs5")) in to your RStudio console
variables <- data.frame(load_variables(2020, "acs5"))
write.csv(variables2,"E:/GSD/2022 Summer/RA/siting tool/R/variables for census.csv", row.names = FALSE)

# See tidycensus documentation here: 
#             https://walker-data.com/tidycensus/articles/basic-usage.html

vars <- c(
  median_income_ = "B06011_001",
  n_rented_homes_ = "B25003_003",
  total_homes_ = "B25003_001",
  gini_index_ = "B19083_001",
  n_sf_det_ = "B25024_002",
  n_sf_att_ = "B25024_003"
)

tracts <- get_acs(geography = "tract",
                  state = "PA",
                  county = "Allegheny",
                  variables = vars,
                  year = 2020,
                  output = "wide",
                  geometry = TRUE) %>%
  st_transform("WGS84") %>%
  mutate(pct_rental_E = n_rented_homes_E / total_homes_E,
         n_sf_homes_E = n_sf_det_E + n_sf_att_E) %>%
  mutate(pct_sf_homes_E = n_sf_homes_E / total_homes_E,
         n_mf_homes_E = total_homes_E - n_sf_homes_E) %>%
  rename(tract_geoid = GEOID) %>%
  select(tract_geoid, 
         median_income_E, 
         gini_index_E, 
         pct_rental_E,
         pct_sf_homes_E,
         n_sf_homes_E,
         n_mf_homes_E)

site_tracts <- st_intersection(sample_locs, tracts)

loan_data <- here("02_data",
                  "loan-data",
                  "tract_loans.csv") %>%
  read_csv() %>%
  mutate(tract_geoid = paste0("42003", Tract)) %>%
  select(tract_geoid,
         sf_num_loans,
         sf_med_income,
         sf_med_upb,
         sf_purchase,
         sf_refinance,
         sf_rehab,
         sf_cashout,
         mf_num_loans,
         mf_purchase,
         mf_refinance,
         mf_rehab,
         mf_cashout)  %>%
  replace_na(replace = list(mf_num_loans = 0,
                            mf_purchase = 0,
                            mf_refinance = 0,
                            mf_rehab = 0,
                            mf_cashout = 0))

rate_data <- here("02_data",
                  "loan-data",
                  "tract_sf_loans_18_20.csv") %>%
  read_csv()%>%
  mutate(tract_geoid = paste0("42003", Tract)) %>%
  select(tract_geoid,
         mean_rate)

site_tracts_loans <- left_join(site_tracts, loan_data) %>%
  left_join(rate_data) %>%
  mutate(annual_loans_per_sf_home = sf_num_loans / (n_sf_homes_E * 13),
         annual_loans_per_mf_home = mf_num_loans / (n_mf_homes_E * 13),
         income_ratio_sf = sf_med_income / median_income_E,
         pct_sf_purchase = sf_purchase / sf_num_loans,
         pct_sf_refi = sf_refinance / sf_num_loans,
         pct_sf_rehab = sf_rehab / sf_num_loans,
         pct_sf_cashout = sf_cashout / sf_num_loans) %>%
  rename(lon = x,
         lat = y) %>%
  select(id,
         lat,
         lon,
         tract_geoid,
         median_income_E,
         gini_index_E,
         pct_rental_E,
         pct_sf_homes_E,
         annual_loans_per_sf_home,
         annual_loans_per_mf_home,
         income_ratio_sf,
         pct_sf_purchase,
         pct_sf_refi,
         pct_sf_rehab,
         pct_sf_cashout,
         mean_rate) %>%
  st_drop_geometry()

write_csv(site_tracts_loans,
  here("02_data",
       "sample_sites_tract_data.csv"))
  
  
