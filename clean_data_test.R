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
  ungroup() %>% 
  group_by(schoolName) %>% 
  mutate(maxResult = max(maxResult),
         lead = case_when(XMOD != "<" & maxResult > 5 ~ TRUE, 
                          XMOD == "<" & maxResult == 5 ~ FALSE, 
                          TRUE ~ as.logical(NA)),
         maxResult = ifelse(lead == FALSE, NA, maxResult),
         status = "tested") %>% 
  filter(!is.na(lead)) %>% 
  select(-XMOD) %>% 
  unique()

write.csv(tested_schools, file="tested_schools.csv")

exempt <- read_excel('data-raw/exemption_forms.xlsx')

exempt_schools <- exempt %>% 
  mutate(maxResult = NA, unit = NA, lead = NA, status = "exempt") %>% 
  select(district = `School District`, schoolName = Name, schoolAddress = Address, 
         maxResult, unit, lead, status)

write.csv(exempt_schools, file="exempt_schools.csv")


