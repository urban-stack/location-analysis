####################

library(tidyverse)
library(arrow)
library(here)
library(EFAtools)

### Read in data
access <- here("02_data",
               "access.parquet") %>%
  read_parquet()

assessor <- here("02_data",
                 "sites.csv") %>%
  read_csv(show_col_types = FALSE) %>%
  select(-USEDESC)# "-" means not included 

diversity <- here("02_data",
                  "diversity.parquet") %>%
  read_parquet() %>%
  select(PARID,
         radius,
         pct_white,
         pct_black,
         pct_hispanic,
         n_uses)

proximity <- here("02_data",
                  "bad_proximity.csv") %>%
  read_csv(show_col_types = FALSE)

img_data <- here("google SVI 5k",
                 "image_features.csv") %>% 
  read_csv(show_col_types = F)

img_id <- here("02_data",
               "sample-sites-5k.csv") %>% 
  read_csv(show_col_types = F)

img <- cbind(img_id, img_data) %>% 
  rename(PARID = id) %>% 
  select(PARID,
         x,
         y,
         image_id,
         built_score_ad,
         paved_score_ad,
         sky_score_ad,
         nature_score_ad,
         road_score_ad,
         sidewalk_score_ad,
         terrain_score_ad,
         vegetation_score_ad,
         pole_score_ad)

site_data <- inner_join(access, assessor) %>%
  inner_join(diversity) %>%
  inner_join(proximity) %>% 
  inner_join(img)

factor_data <- site_data %>%
  select(-PARID,-PROPERTYCITY,-x,-y,-image_id,
         #-built_score_ad,
         -paved_score_ad,
         -sky_score_ad,
         #-nature_score_ad,
         -road_score_ad,
         -sidewalk_score_ad,
         -terrain_score_ad,
         -vegetation_score_ad,
         -pole_score_ad
         )

#### Check data to see if factor analysis is okay

KMO(factor_data)

Sys.time()
n_factors <- N_FACTORS(factor_data,
                       #criteria = c("CD", "EKC", "HULL", "KGC", "PARALLEL", "SCREE", "SMT")
                       )
Sys.time()
n_factors
### Get the number of factors - 5 based on CD & Kaiser Guttman with EFA

Sys.time()
n_factors_hull <- HULL(factor_data, 
                       eigen_type = "EFA")
Sys.time()
n_factors_hull

n_factors_KGC <- KGC(factor_data, 
                     eigen_type = "EFA")
n_factors_KGC

n_factors_CD <- CD(factor_data)
n_factors_CD

n_factors_EKC <- EKC(factor_data)
n_factors_EKC

n_factors_PARALLEL <- PARALLEL(factor_data,
                               eigen_type = "EFA")
n_factors_PARALLEL

############ Do the factor analysis
factors <- EFA(factor_data, n_factors = 7, rotation = 'oblimin')

factors

factors <- EFA(factor_data, n_factors = 8, rotation =
               ## "none",
               # "varimax",
               # "equamax",
               ## "quartimax",
               ## "geominT",
               ## "bentlerT",
               ## "bifactorT",
               "promax",
               # #"oblimin",
               # #"quartimin",
               ## "simplimax",
               # ##"bentlerQ",
               # #"geominQ",
               # #"bifactorQ"
               )

factors

loadings <- data.frame(matrix(factors$rot_loadings, ncol = 8)) %>%
  mutate(variable = colnames(factor_data)) %>%
  mutate(f_drivable = X1,
         f_walkable = X2,
         f_diverse = X3,
         f_biketodest = X4,
         f_biketojob = X5,
         f_amendities = X6,
         f_dense = X7,
         f_built_nature = X8) %>%
  select(variable, 
         f_drivable,
         f_walkable,
         f_diverse,
         f_biketodest,
         f_biketojob,
         f_amendities,
         f_dense,
         f_built_nature)

######### Generate indices

scores <- data.frame(FACTOR_SCORES(factor_data, factors)$scores)

factor_scores <- cbind(site_data, scores) %>%
  mutate(f_drivable = F3,
         f_walkable = F1,
         f_diverse = F2,
         f_biketodest = F4,
         f_biketojob = F7,
         f_amendities = F5,
         f_dense = F8,
         f_built_nature = F6) %>%
  select(PARID, 
         f_drivable,
         f_walkable,
         f_diverse,
         f_biketodest,
         f_biketojob,
         f_amendities,
         f_dense,
         f_built_nature)

write_csv(factor_scores,
          here("02_data",
               "indices_with_img.csv"))

write_csv(loadings,
          here("02_data",
               "loadings_with_img.csv"))
