####

library(tidyverse)
library(readxl)
library(here)


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
  select(`PARCEL NUMBER`,
         `TYPE OF WORK`,
         `TYPE OF STRUCTURE`)

for (i in 1:11) {
  next_permits <- here("02_data",
                       "pli-permit-summary",
                       paste0("pli-permit-summary-",
                              months[i],
                              ".xlsx")) %>%
    read_xlsx(sheet = 1) %>%
    select(`PARCEL NUMBER`,
           `TYPE OF WORK`,
           `TYPE OF STRUCTURE`)
    
    permits <- rbind(permits, next_permits)
  
}

res_permits <- permits %>%
  mutate(PARID = paste0(substr(`PARCEL NUMBER`, 1, 4),
                        substr(`PARCEL NUMBER`, 6, 6),
                        substr(`PARCEL NUMBER`, 8, 12),
                        substr(`PARCEL NUMBER`, 14, 17),
                        substr(`PARCEL NUMBER`, 19, 20))) %>%
  filter(`TYPE OF WORK` == "NEW CONSTRUCTION" |
           `TYPE OF WORK` == "COMPLETE DEMOLITION" |
           `TYPE OF WORK` == "NEW" |
           `TYPE OF WORK` == "NEW CONSTRUCTION" |
           `TYPE OF WORK` == "REPLACEMENT" |
           `TYPE OF WORK` == "PARTIAL DEMOLITION") %>%
  filter(`TYPE OF STRUCTURE` == "Residential" |
           `TYPE OF STRUCTURE` == "Residential - Two-Family") 

permit_sites <- unique(res_permits$PARID)
