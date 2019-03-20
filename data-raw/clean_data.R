library(tidyverse)
library(readxl)
library(ggmap)
library(stringr)

tested <- read_excel('data-raw/MonthlyPostingMarch.xlsx', skip = 1)
glimpse(tested)

# how many exceed 5 ppb? what is the follow up status?
tested %>% 
  group_by(`Action_Level_Exceedance?`, ALE_Follow_Up_Status) %>% 
  summarise(count = n())

lead_present <- tested %>% 
  filter(XMOD != "<", ALE_Follow_Up_Status == "Pending" | is.na(ALE_Follow_Up_Status)) %>% 
  group_by(SchoolName) %>% 
  mutate(medianResult = median(RESULT), unit = 'ppb', lead = TRUE) %>% 
  select(district = DISTRICT, schoolName = SchoolName, 
         schoolAddress = SchoolAddress, medianResult, unit, lead) %>% 
  unique() %>% 
  ungroup()

tested_schools <- tested %>% 
  select(district = DISTRICT, schoolName = SchoolName, schoolAddress = SchoolAddress) %>% 
  unique() %>% 
  left_join(lead_present) %>% 
  mutate(status = 'tested', lead = ifelse(is.na(lead), FALSE, lead))

View(tested_schools)
glimpse(tested_schools)
tested_schools %>% 
  separate(schoolAddress, c('Street', 'State', 'Zip'), sep = 'CA', remove = FALSE) %>% View
un_tested <- read_excel('data-raw/SchoolsUnsampled.xlsx')
glimpse(un_tested)

not_tested_schools <- un_tested %>% 
  mutate(schoolAddress = paste(Street, City, 'CA', Zip, sep = ' '),
         medianResult = NA, unit = NA, lead = NA, status = "not tested") %>% 
  select(district = District, schoolName = School, schoolAddress, medianResult, 
         unit, lead, status)
View(not_tested_schools)

exempt <- read_excel('data-raw/exemption_forms.xlsx')
glimpse(exempt)

exempt_schools <- exempt %>% 
  mutate(medianResult = NA, unit = NA, lead = NA, status = "exempt") %>% 
  select(district = `School District`, schoolName = Name, schoolAddress = Address, 
         medianResult, unit, lead, status)

View(exempt_schools)

all_schools <- tested_schools %>% 
  bind_rows(not_tested_schools) %>% 
  bind_rows(exempt_schools)

# geo coding from daniel and victoria
geo_coded <- read_csv('data-raw/ca_schools_lead_testing_data_geocoded.csv') %>%
  select(schoolAddress, city, county, latitude, longitude) %>% 
  unique()

# check for dirty district naming
dirty_district <- all_schools %>% 
  select(district) %>% 
  unique %>% 
  arrange(district) %>% 
  separate(district, c('begin', 'rest'), ' ', remove = FALSE) %>% 
  mutate(prev_beg = lag(begin), next_beg = lead(begin), prev_district = lag(district),
         next_district = lead(district),
         match = case_when(
           begin == prev_beg ~ prev_district,
           begin == next_beg ~ next_district,
           TRUE ~ 'no match'
         )) %>%
  filter(match != 'no match') %>% 
  select(district, match) 

# district names from CDE https://www.cde.ca.gov/ds/si/ds/pubschls.asp
cde_districts <- read_excel('data-raw/pubdistricts.xlsx', skip = 5) %>% 
  select(district = District) %>% 
  mutate(in_cde = TRUE)

district_lookup <- dirty_district %>%   
  left_join(cde_districts) %>% 
  filter(is.na(in_cde)) %>% 
  select(-in_cde)

all_schools %>% 
  left_join(district_lookup) %>% 
  mutate(district = ifelse(is.na(match), district, match),
         district = ifelse(is.na(district) | district == 'private', 'Private', district)) %>% 
  select(-match) %>%
  left_join(geo_coded) %>% 
  write_csv('ca_schools_lead_testing_data.csv')

# check repeat school names
repeat_names <- all_schools %>% 
  select(schoolName, schoolAddress) %>% 
  unique() %>% 
  group_by(schoolAddress) %>% 
  summarise(count = n()) %>% 
  filter(count > 1) %>% 
  mutate(duplicate = TRUE) %>% 
  select(-count) %>% 
  ungroup()

# didn't see any cases of same school address and similar but different names
all_schools %>% 
  left_join(repeat_names) %>% 
  filter(duplicate) %>% View


