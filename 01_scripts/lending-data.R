library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(fs)
library(modeest)
library(ggspatial)
library(ggthemes)
library(scales)

# since data structure are different in 2008-2009, 2010-2017, and 2018-2020, prepare three col names for multi-family data
mf_08_09_names <- c("EntFlag",
                    "RecNo",
                    "State",
                    "MSA",
                    "County",
                    "Tract",
                    "PctMinority",
                    "MedIncome",
                    "areaMedIncome",
                    "TractIncomeRatio",
                    "areaMedFamInc",
                    "UPB",
                    "Purpose",
                    "InstType",
                    "FedGuaran",
                    "UndAreaIndi",
                    "LienStat"
)

mf_10_17_names <- c("EntFlag",
                    "RecNo",
                    "State",
                    "MSA",
                    "County",
                    "Tract",
                    "PctMinority",
                    "MedIncome",
                    "areaMedIncome",
                    "TractIncomeRatio",
                    "areaMedFamInc",
                    "UPB",
                    "Purpose",
                    "InstType",
                    "FedGuaran",
                    "LienStat"
)

mf_18_20_names <- c("EntFlag",
                    "RecNo",
                    "State",
                    "MSA",
                    "County",
                    "Tract",
                    "PctMinority",
                    "MedIncome",
                    "areaMedIncome",
                    "TractIncomeRatio",
                    "areaMedFamInc",
                    "UPB",
                    "Purpose",
                    "InstType",
                    "FedGuaran",
                    "LienStat",
                    "LTV",
                    "NoteDate",
                    "Term",
                    "numUnits",
                    "rate",
                    "amt",
                    "propVal",
                    "prepayPenalty",
                    "balloon",
                    "interestOnly",
                    "negAmort",
                    "other",
                    "pctAfford",
                    "constMethod",
                    "Rural",
                    "MissDelta",
                    "Appalachia",
                    "persPov",
                    "consPov",
                    "HiOpp",
                    "QOZ")

# import multi-family data
### Note - this script is from an earlier project repo - the file paths 
# won't work and need to be updated.

for (i in c(2008:2009)) {
  filename <- paste0("mf_", i, "_fnma")
  assign(
    filename,
    read_fwf(path(
      "raw_datasets",
      paste(i, "_MFCensusTract", i, sep = ""),
      paste("fnma_mf", i, "c_loans.txt", sep = "")
    ),
    col_types = cols("number","character","character","number",
                     "character","character","number","number",
                     "number","number","number","number","number",
                     "number","number","number","number","number"),
    show_col_types = FALSE) %>%
      setNames(mf_08_09_names) %>%
      filter(State == '42',
             County == '003') %>%
      mutate(year = i)
  )
}

for (i in c(2008:2009)) {
  filename <- paste0("mf_", i, "_fhlmc")
  assign(
    filename,
    read_fwf(path(
      "raw_datasets",
      paste(i, "_MFCensusTract", i, sep = ""),
      paste("fhlmc_mf", i, "c_loans.txt", sep = "")
    ),
    col_types = cols("number","character","character","number",
                     "character","character","number","number",
                     "number","number","number","number","number",
                     "number","number","number","number","number"),
    show_col_types = FALSE) %>%
      setNames(mf_08_09_names) %>%
      filter(State == '42',
             County == '003') %>%
      mutate(year = i)
  )
}

for (i in c(2010:2017)) {
  filename <- paste0("mf_", i, "_fnma")
  assign(
    filename,
    read_fwf(path(
      "raw_datasets",
      paste(i, "_MFCensusTract", i, sep = ""),
      paste("fnma_mf", i, "c_loans.txt", sep = "")
    ),
    col_types = cols("number","character","character","number",
                     "character","character","number","number",
                     "number","number","number","number","number",
                     "number","number","number","number"),
    show_col_types = FALSE) %>%
      setNames(mf_10_17_names) %>%
      filter(State == '42',
             County == '003') %>%
      mutate(year = i)
  )
}

for (i in c(2010:2017)) {
  filename <- paste0("mf_", i, "_fhlmc")
  assign(
    filename,
    read_fwf(path(
      "raw_datasets",
      paste(i, "_MFCensusTract", i, sep = ""),
      paste("fhlmc_mf", i, "c_loans.txt", sep = "")
    ),
    col_types = cols("number","character","character","number",
                     "character","character","number","number",
                     "number","number","number","number","number",
                     "number","number","number","number"),
    show_col_types = FALSE) %>%
      setNames(mf_10_17_names) %>%
      filter(State == '42',
             County == '003') %>%
      mutate(year = i)
  )
}

for (i in c(2018:2020)) {
  filename <- paste0("mf_", i, "_fnma")
  assign(
    filename,
    read_fwf(path(
      "raw_datasets",
      paste(i, "_MFCensusTract", i, sep = ""),
      paste("fnma_mf", i, "c_loans.txt", sep = "")
    ),col_types = cols("number","character","character",
                       "number","character","character",
                       "number","number","number","number",
                       "number","number","number","number",
                       "number","number","number","number",
                       "number","number","number","number",
                       "number","number","number","number",
                       "number","number","number","number",
                       "number","number","number","number",
                       "number"),
    show_col_types = FALSE) %>%
      setNames(mf_18_20_names) %>%
      filter(State == '42',
             County == '003') %>%
      mutate(year = i)
  )
}

for (i in c(2018:2020)) {
  filename <- paste0("mf_", i, "_fhlmc")
  assign(
    filename,
    read_fwf(path(
      "raw_datasets",
      paste(i, "_MFCensusTract", i, sep = ""),
      paste("fhlmc_mf", i, "c_loans.txt", sep = "")
    ),col_types = cols("number","character","character",
                       "number","character","character",
                       "number","number","number","number",
                       "number","number","number","number",
                       "number","number","number","number",
                       "number","number","number","number",
                       "number","number","number","number",
                       "number","number","number","number",
                       "number","number","number","number",
                       "number"),
    show_col_types = FALSE) %>%
      setNames(mf_18_20_names) %>%
      filter(State == '42',
             County == '003') %>%
      mutate(year = i)
  )
}

# since data structure are different in 2008-2009, 2010-2017, 
# and 2018-2020, prepare three col names for single-family data. 
#Data structure of single-family is a bit different from multi-family. 
sf_08_09_names <- c("EntFlag",
                    "RecNo",
                    "State",
                    "MSA",
                    "County",
                    "Tract",
                    "PctMinority",
                    "MedIncome",
                    "areaMedIncome",
                    "TractIncomeRatio",
                    "BorrowersIncome",
                    "areaMedFamInc",
                    "BorrowerIncomeRatio",
                    "UPB",
                    "Purpose",
                    "FedGuaran",
                    "NumBorrower",
                    "FirstBuyer",
                    "Race_1",
                    "Race_2",
                    "Race_3",
                    "Race_4",
                    "Race_5",
                    "Ethnicity",
                    "CoBoRace_1",
                    "CoBoRace_2",
                    "CoBoRace_3",
                    "CoBoRace_4",
                    "CoBoRace_5",
                    "CoBoEthnicity",
                    "Gender",
                    "CoBoGender",
                    "Age",
                    "CoBoAge",
                    "Occupancy",
                    "UndAreaIndi",
                    "RateSpread",
                    "Hoepa",
                    "PropertyType",
                    "LienStat"
)

sf_10_17_names <- c("EntFlag",
                    "RecNo",
                    "State",
                    "MSA",
                    "County",
                    "Tract",
                    "PctMinority",
                    "MedIncome",
                    "areaMedIncome",
                    "TractIncomeRatio",
                    "BorrowersIncome",
                    "areaMedFamInc",
                    "BorrowerIncomeRatio",
                    "UPB",
                    "Purpose",
                    "FedGuaran",
                    "NumBorrower",
                    "FirstBuyer",
                    "Race_1",
                    "Race_2",
                    "Race_3",
                    "Race_4",
                    "Race_5",
                    "Ethnicity",
                    "CoBoRace_1",
                    "CoBoRace_2",
                    "CoBoRace_3",
                    "CoBoRace_4",
                    "CoBoRace_5",
                    "CoBoEthnicity",
                    "Gender",
                    "CoBoGender",
                    "Age",
                    "CoBoAge",
                    "Occupancy",
                    "RateSpread",
                    "Hoepa",
                    "PropertyType",
                    "LienStat"
)

sf_18_20_names <- c("EntFlag",
                    "RecNo",
                    "State",
                    "MSA",
                    "County",
                    "Tract",
                    "PctMinority",
                    "MedIncome",
                    "areaMedIncome",
                    "TractIncomeRatio",
                    "BorrowersIncome",
                    "areaMedFamInc",
                    "BorrowerIncomeRatio",
                    "UPB",
                    "Purpose",
                    "FedGuaran",
                    "NumBorrower",
                    "FirstBuyer",
                    "Race_1",
                    "Race_2",
                    "Race_3",
                    "Race_4",
                    "Race_5",
                    "Ethnicity",
                    "CoBoRace_1",
                    "CoBoRace_2",
                    "CoBoRace_3",
                    "CoBoRace_4",
                    "CoBoRace_5",
                    "CoBoEthnicity",
                    "Gender",
                    "CoBoGender",
                    "Age",
                    "CoBoAge",
                    "Occupancy",
                    "RateSpread",
                    "Hoepa",
                    "PropertyType",
                    "LienStat",
                    "Over62",
                    "CoBoOver62",
                    "LTV",
                    "NoteDate",
                    "Term",
                    "numUnits",
                    "rate",
                    "amt",
                    "Preapproval",
                    "AppliChannel",
                    "AUS",
                    "CreditScoreBo",
                    "CreditScoreCoBo",
                    "DTIRatio",
                    "DiscountPoints",
                    "IntroRatePeriod",
                    "LandProperyInterest",
                    "propVal",
                    "Rural",
                    "MissDelta",
                    "Appalachia",
                    "persPov",
                    "consPov",
                    "HiOpp",
                    "QOZ")


for (i in c(2008:2009)) {
  filename <- paste0("sf_", i, "_fhlmc")
  assign(
    filename,
    read_fwf(path(
      "raw_datasets",
      paste(i, "_SFCensusTractFRE", i, sep = ""),
      paste("fhlmc_sf", i, "c_loans.txt", sep = "")
    ),
    show_col_types = FALSE) %>%
      setNames(sf_08_09_names) %>%
      filter(State == '42',
             County == '003') %>%
      mutate(year = i)
  )
}

for (i in c(2008:2009)) {
  filename <- paste0("sf_", i, "_fnma")
  assign(
    filename,
    read_fwf(path(
      "raw_datasets",
      paste(i, "_SFCensusTractFNM", i, sep = ""),
      paste("fnma_sf", i, "c_loans.txt", sep = "")
    ),
    show_col_types = FALSE) %>%
      setNames(sf_08_09_names) %>%
      filter(State == '42',
             County == '003') %>%
      mutate(year = i)
  )
}

for (i in c(2010:2017)) {
  filename <- paste0("sf_", i, "_fhlmc")
  assign(
    filename,
    read_fwf(path(
      "raw_datasets",
      paste(i, "_SFCensusTractFRE", i, sep = ""),
      paste("fhlmc_sf", i, "c_loans.txt", sep = "")
    ),
    show_col_types = FALSE) %>%
      setNames(sf_10_17_names) %>%
      filter(State == '42',
             County == '003') %>%
      mutate(year = i)
  )
}

for (i in c(2010:2017)) {
  filename <- paste0("sf_", i, "_fnma")
  assign(
    filename,
    read_fwf(path(
      "raw_datasets",
      paste(i, "_SFCensusTractFNM", i, sep = ""),
      paste("fnma_sf", i, "c_loans.txt", sep = "")
    ),
    show_col_types = FALSE) %>%
      setNames(sf_08_09_names) %>%
      filter(State == '42',
             County == '003') %>%
      mutate(year = i)
  )
}

for (i in c(2018:2020)) {
  filename <- paste0("sf_", i, "_fhlmc")
  assign(
    filename,
    read_fwf(path(
      "raw_datasets",
      paste(i, "_SFCensusTractFRE", i, sep = ""),
      paste("fhlmc_sf", i, "c_loans.txt", sep = "")
    ),
    show_col_types = FALSE) %>%
      setNames(sf_18_20_names) %>%
      filter(State == '42',
             County == '003') %>%
      mutate(year = i)
  )
}

for (i in c(2018:2020)) {
  filename <- paste0("sf_", i, "_fnma")
  assign(
    filename,
    read_fwf(path(
      "raw_datasets",
      paste(i, "_SFCensusTractFNM", i, sep = ""),
      paste("fnma_sf", i, "c_loans.txt", sep = "")
    ),
    show_col_types = FALSE) %>%
      setNames(sf_18_20_names) %>%
      filter(State == '42',
             County == '003') %>%
      mutate(year = i)
  )
}

# bind multi-family and single family data
mf_data <-
  bind_rows(
    mf_2008_fhlmc,
    mf_2009_fhlmc,
    mf_2010_fhlmc,
    mf_2011_fhlmc,
    mf_2012_fhlmc,
    mf_2013_fhlmc,
    mf_2014_fhlmc,
    mf_2015_fhlmc,
    mf_2016_fhlmc,
    mf_2017_fhlmc,
    mf_2018_fhlmc,
    mf_2019_fhlmc,
    mf_2020_fhlmc,
    mf_2008_fnma,
    mf_2009_fnma,
    mf_2010_fnma,
    mf_2011_fnma,
    mf_2012_fnma,
    mf_2013_fnma,
    mf_2014_fnma,
    mf_2015_fnma,
    mf_2016_fnma,
    mf_2017_fnma,
    mf_2018_fnma,
    mf_2019_fnma,
    mf_2020_fnma
  )%>%
  mutate(FamilyType = "mf")

sf_2008_2017_data <-
  bind_rows(
    sf_2008_fhlmc,
    sf_2009_fhlmc,
    sf_2010_fhlmc,
    sf_2011_fhlmc,
    sf_2012_fhlmc,
    sf_2013_fhlmc,
    sf_2014_fhlmc,
    sf_2015_fhlmc,
    sf_2016_fhlmc,
    sf_2017_fhlmc,
    sf_2018_fhlmc,
    sf_2019_fhlmc,
    sf_2020_fhlmc,
    sf_2008_fnma,
    sf_2009_fnma,
    sf_2010_fnma,
    sf_2011_fnma,
    sf_2012_fnma,
    sf_2013_fnma,
    sf_2014_fnma,
    sf_2015_fnma,
    sf_2016_fnma,
    sf_2017_fnma)

# transform continuous age variable to categorical variable
sf_2008_2017_data$Age <- as.numeric(cut(sf_2008_2017_data$Age,
                                        breaks = c(0, 24, 34, 44, 54, 64, 74, 120, 121, 999),
                                        labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9)))

sf_2008_2017_data$CoBoAge <- as.numeric(cut(sf_2008_2017_data$CoBoAge,
                                            breaks = c(0, 24, 34, 44, 54, 64, 74, 120, 121, 999),
                                            labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9)))

sf_data <-
  bind_rows(
    sf_2008_2017_data,
    sf_2018_fnma,
    sf_2019_fnma,
    sf_2020_fnma
  ) %>%
  mutate(FamilyType = "sf")

# select columns that are collected in the whole years
mf_data <- mf_data %>% 
  select(1:15, 17:18, 40)

# focus on the 2018 to 2020 data since they contain additional information such as interest rate
sf_data$Age <- case_when(sf_data$Age == 1 ~ 20,
                         sf_data$Age == 2 ~ 30,
                         sf_data$Age == 3 ~ 40,
                         sf_data$Age == 4 ~ 50,
                         sf_data$Age == 5 ~ 60,
                         sf_data$Age == 6 ~ 70,
                         sf_data$Age == 7 ~ 80)

sf_loans_18_20 <- sf_data %>% filter(year >= 2018)

sf_data <- sf_data %>% 
  select(1:35, 37:41, 67)

data <- bind_rows(mf_data, sf_data)

# remove data from 2008 to 2011 as their tract boundary is different from that of after 2012
mf_data <- mf_data %>% 
  filter(year != "2008", year != "2009", year != "2010", year != "2011")
sf_data <- sf_data %>% 
  filter(year != "2008", year != "2009", year != "2010", year != "2011")
data <- data %>% 
  filter(year != "2008", year != "2009", year != "2010", year != "2011")

write_csv(sf_data, "sf_loan_data.csv")
write_csv(mf_data, "mf_loan_data.csv")
write_csv(data, "loan_data.csv")

# count the number of sf and mf loans for each tract and calculate medians of income and upb.
tract_sf_loans <- data %>% filter(FamilyType == "sf", 
                                  BorrowersIncome < 999999999, 
                                  UPB < 999999999, 
                                  Age != "NA") %>% 
  group_by(Tract) %>% 
  summarise(sf_num_loans = n(), 
            sf_med_income = median(BorrowersIncome), 
            sf_med_upb = median(UPB), 
            indian = sum(Race_1 == 1), 
            asian = sum(Race_1 == 2), 
            black = sum(Race_1 == 3), 
            hawai = sum(Race_1 == 4), 
            white = sum(Race_1 == 5), 
            mean_age = mean(Age),  
            sf_purchase = sum(Purpose == 1), 
            sf_refinance = sum(Purpose == 2), 
            sf_rehab = sum(Purpose == 4), 
            sf_cashout = sum(Purpose == 7), 
            sf_na = sum(Purpose == 9))

tract_mf_loans <- data %>% filter(FamilyType == "mf") %>% 
  group_by(Tract) %>% 
  summarise(mf_num_loans = n(), 
            mf_med_upb = median(UPB), 
            mf_purchase = sum(Purpose == 1), 
            mf_refinance = sum(Purpose == 2), 
            mf_rehab = sum(Purpose == 4), 
            mf_cashout = sum(Purpose == 7), 
            mf_na = sum(Purpose == 9))

tract_loans <- full_join(tract_sf_loans, 
                         tract_mf_loans, 
                         by = c("Tract" = "Tract"))

# focus on the 2018 to 2020 data since they contain additional information such as interest rate
tract_sf_loans_18_20 <- sf_loans_18_20 %>% 
  filter(FamilyType == "sf", 
         BorrowersIncome < 999999999, 
         UPB < 999999999, 
         Age != "NA", 
         rate < 99, 
         amt < 999999999) %>% 
  group_by(Tract) %>% 
  summarise(sf_num_loans = n(), 
            sf_med_income = median(BorrowersIncome), 
            sf_med_upb = median(UPB), 
            indian = sum(Race_1 == 1), 
            asian = sum(Race_1 == 2), 
            black = sum(Race_1 == 3), 
            hawai = sum(Race_1 == 4), 
            white = sum(Race_1 == 5),   
            sf_purchase = sum(Purpose == 1), 
            sf_refinance = sum(Purpose == 2), 
            sf_rehab = sum(Purpose == 4), 
            sf_cashout = sum(Purpose == 7), 
            sf_na = sum(Purpose == 9), 
            mean_age = mean(Age), 
            mean_rate = mean(rate), 
            mean_amt = mean(amt), 
            mean_term = mean(Term))

# Set coordinate system
PA_state_plane <- 
  "+proj=lcc +lat_1=41.95 +lat_2=40.88333333333333 +lat_0=40.16666666666666 +lon_0=-77.75 +x_0=600000 +y_0=0 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

# Get the tract boundary data and the number of units data
acsvars <- load_variables(2019, "acs5")
tracts <- get_acs(geography = "tract", 
                  county = "003", 
                  state = "PA", 
                  variable = c(unit = "B11011_001"), 
                  output = "wide", geometry = TRUE) %>% 
  st_transform(PA_state_plane)

tracts$GEOID <- gsub("^.{0,5}", "", tracts$GEOID)

# The number of tracts collected from FHFA is different from that of tigris. 
# FHFA contains 446 tracts though the tigris contains only 402.

tract_loans <- full_join(tract_loans, tracts, by = c("Tract" = "GEOID"))
# tract_loans <- mutate_at(tract_loans, 
#                          c("sf_num_loans", 
#                            "mf_num_loans", 
#                            "sf_med_income", 
#                            "sf_med_upb", 
#                            "mf_med_upb"), ~replace(., is.na(.), 0))

tract_loans <- tract_loans %>% 
  mutate(family = case_when(sf_num_loans > 0 & mf_num_loans > 0 ~ 
                              "Single Family and Multi Family",
                            sf_num_loans > 0 & is.na(mf_num_loans) ~ 
                              "Single Family only",
                            is.na(sf_num_loans) & mf_num_loans > 0 ~ 
                              "Single Family only",
                            
                            is.na(sf_num_loans) & is.na(mf_num_loans) ~ "None"
  )) %>% 
  slice(-1)

tract_sf_loans_18_20 <- full_join(tract_sf_loans_18_20, tracts, by = c("Tract" = "GEOID"))

write_csv(tract_loans, "tract_loans.csv")
write_csv(tract_sf_loans_18_20, "tract_sf_loans_18_20.csv")