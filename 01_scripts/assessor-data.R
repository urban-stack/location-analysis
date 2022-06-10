library(here)
library(knitr)
library(tidyverse)
library(lubridate)
library(sf)

`%!in%` <- Negate(`%in%`)

uses <- here("02_data",
             "uses.csv") %>%
  read_csv(show_col_types = FALSE)

inflation <- here("02_data",
                  "inflation.csv") %>%
  read_csv(show_col_types = FALSE)

parcel_locs <- st_read('https://data.wprdc.org/dataset/6bb2a968-761d-48cf-ac5b-c1fc80b4fe6a/resource/42231cab-8341-48d6-b695-47612dd6514a/download/parcelcoords.csv',
                       options = c("X_POSSIBLE_NAMES=x", 
                                   "Y_POSSIBLE_NAMES=y")) %>%
  rename(PARID = PIN) %>%
  st_set_crs("WGS84")

parcel_data <- read_csv('https://data.wprdc.org/dataset/2b3df818-601e-4f06-b150-643557229491/resource/f2b8d575-e256-4718-94ad-1e12239ddb92/download/assessments.csv',
                        show_col_types = FALSE) %>%
  select(PARID, 
         PROPERTYHOUSENUM,
         PROPERTYADDRESS,
         PROPERTYFRACTION,
         PROPERTYCITY,
         PROPERTYSTATE,
         PROPERTYUNIT,
         PROPERTYZIP,
         CLASSDESC,
         USEDESC,
         LOTAREA,
         SALEDATE,
         SALEPRICE,
         PREVSALEDATE,
         PREVSALEPRICE,
         PREVSALEDATE2,
         PREVSALEPRICE2,
         FAIRMARKETLAND,
         FAIRMARKETBUILDING,
         FAIRMARKETTOTAL,
         ASOFDATE,
         YEARBLT) %>%
  mutate(address = paste(PROPERTYHOUSENUM,
                         PROPERTYADDRESS,
                         PROPERTYFRACTION,
                         PROPERTYZIP)) %>%
  mutate(USEDESC = ifelse(PARID == "0880H00194000000",
                          "SINGLE FAMILY",
                          USEDESC)) %>%
  inner_join(parcel_locs) %>%
  left_join(uses) 

parcel_data %>%
  select(PARID, USEDESC, Category, x, y) %>%
  write_csv(here("02_data",
                 "all-parcels.csv"))

sites <- parcel_data %>%
  filter(Category == "site") %>%
  filter(CLASSDESC == "RESIDENTIAL" |
           CLASSDESC == "COMMERCIAL") %>%
  filter(!is.na(SALEDATE) & !is.na(SALEPRICE)) %>%
  filter(PARID != "0014G00199000000" &
           PARID != "0165G00270000000") %>%
  mutate(SALEPRICE = 
           ifelse(PARID == "0098C00044000000", 
                  (SALEPRICE / 1000), 
                  SALEPRICE)) %>%
  mutate(PREVSALEDATE = ifelse(PARID == "0596M00299000000",
                               SALEDATE,
                               PREVSALEDATE)) %>%
  mutate(PREVSALEPRICE = ifelse(PARID == "0596M00299000000",
                                SALEPRICE,
                                PREVSALEPRICE)) %>%
  mutate(SALEDATE = ifelse(PARID == "0596M00299000000",
                           "10-24-2004",
                           SALEDATE)) %>%
  mutate(SALEPRICE = ifelse(PARID == "0596M00299000000",
                            350000,
                            SALEPRICE)) %>%
  mutate(PREVSALEDATE = ifelse(is.na(PREVSALEDATE), 
                               "01-01-1950", 
                               PREVSALEDATE)) %>%
  mutate(SALEDATE = mdy(SALEDATE),
         PREVSALEDATE = mdy(PREVSALEDATE),
         PREVSALEDATE2 = mdy(PREVSALEDATE2),
         ASOFDATE = dmy(ASOFDATE)) %>%
  mutate(years_since_sale = as.numeric(ASOFDATE - SALEDATE)/365.25,
         btw_sales_1 = as.numeric(SALEDATE - PREVSALEDATE)/365.25,
         btw_sales_2 = as.numeric(PREVSALEDATE - PREVSALEDATE2)/365.25) %>%
  mutate(btw_sales_1 = case_when(btw_sales_1 > 0 ~ btw_sales_1,
                                 btw_sales_1 == 0 ~ btw_sales_2,
                                 btw_sales_1 < 0 ~ -1 * btw_sales_1)) %>%
  mutate(btw_sales_avg = ifelse(is.na(btw_sales_2), 
                                btw_sales_1,
                                (btw_sales_1 + btw_sales_2)/2)) %>%
  mutate(year = year(SALEDATE)) %>%
  left_join(inflation) %>%
  mutate(infl_adj_price = SALEPRICE * adj) 

zero_land_val <- sites %>%
  filter(FAIRMARKETLAND == 0 &
           FAIRMARKETBUILDING > 0 &
           substr(USEDESC, 1, 6) != "VACANT") 

condo_addresses <- zero_land_val %>%
  group_by(address) %>%
  summarize(n = n()) %>%
  arrange(desc(n))

condos <- sites %>%
  filter(address %in% condo_addresses$address) %>%
  group_by(address) %>%
  summarise(n_condos = n(),
            PARID = first(PARID),
            FAIRMARKETTOTAL = sum(FAIRMARKETTOTAL),
            FAIRMARKETBUILDING = sum(FAIRMARKETBUILDING),
            LOTAREA = sum(LOTAREA),
            USEDESC = first(USEDESC),
            btw_sales_avg = mean(btw_sales_avg, na.rm = TRUE),
            infl_adj_price = sum(infl_adj_price, na.rm = TRUE)) %>%
  filter(n_condos > 1)

sites <- sites %>%
  mutate(n_condos = 1) %>%
  filter(sites$address %!in% condos$address) %>%
  select(address,
         n_condos,
         PARID,
         FAIRMARKETTOTAL, 
         FAIRMARKETBUILDING, 
         LOTAREA,
         USEDESC,
         btw_sales_avg,
         infl_adj_price) %>%
  rbind(condos) %>%
  select(-address, -n_condos)

write_csv(sites,
          here("02_data",
               "sites.csv"))