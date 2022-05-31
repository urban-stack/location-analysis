## Generate table of inflation adjustment factors

library(here)
library(priceR)
library(tidyverse)

years = seq(1700, 2021, by = 1)
infl_adj = adjust_for_inflation(price = rep(1, times = length(years)), 
                                from_date = years,
                                to_date = 2021,
                                country = "US")

infl_adj_tble <- tibble(year = years,
                        adj = infl_adj)

here("data",
     "inflation.csv") %>%
  write_csv(x = infl_adj_tble, file = .)