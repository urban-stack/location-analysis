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
  select(-USEDESC)

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

site_data <- inner_join(access, assessor) %>%
  inner_join(diversity) %>%
  inner_join(proximity)

factor_data <- site_data %>%
  select(-PARID, -btw_sales_avg)

#### Check data to see if factor analysis is okay

KMO(factor_data)

### Get the number of factors - 5 based on CD & Kaiser Guttman with EFA

Sys.time()
n_factors <- N_FACTORS(factor_data)
Sys.time()

n_factors

############ Do the factor analysis
factors <- EFA(factor_data, n_factors = 5, rotation = 'oblimin')

factors

loadings <- data.frame(matrix(factors$rot_loadings, ncol = 5)) %>%
  mutate(variable = colnames(factor_data)) %>%
  mutate(f_drivable = X1,
         f_walkable = X2,
         f_dense = -1 * X3,
         f_diverse = X4,
         f_affordable = -1 * X5) %>%
  select(variable, 
         f_drivable,
         f_walkable,
         f_dense,
         f_diverse,
         f_affordable)

######### Generate indices

scores <- data.frame(FACTOR_SCORES(factor_data, factors)$scores)

factor_scores <- cbind(site_data, scores) %>%
  mutate(f_drivable = X1,
         f_walkable = X2,
         f_dense = -1 * X3,
         f_diverse = X4,
         f_affordable = -1 * X5) %>%
  select(PARID, 
         f_drivable,
         f_walkable,
         f_dense,
         f_diverse,
         f_affordable)

write_csv(factor_scores,
          here("02_data",
               "indices.csv"))

write_csv(loadings,
          here("02_data",
               "loadings.csv"))
