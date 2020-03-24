library(tidyverse)
library(readxl)
library(stringr)

raw_lead_data <- read_excel('data-raw/monthlypostingJan2020.xlsx', skip = 1)

tested_schools <- raw_lead_data %>% 
  group_by(SchoolName, XMOD) %>%
  mutate(maxResult = max(RESULT), unit = 'ppb') %>%
  select(XMOD, district = DISTRICT, schoolName = SchoolName, 
         schoolAddress = SchoolAddress, maxResult, unit) %>%
  unique() %>% 
  ungroup()




  