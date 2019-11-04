library(tidyverse)
library(readxl)
library(stringr)

# tested <- read_excel('data-raw/MonthlyPostingMarch.xlsx', skip = 1)
# tested <- read_excel('data-raw/monthlypostingjune.xlsx', skip = 1)
# tested <- read_excel('data-raw/monthlyposting_september2019.xlsx', skip = 1)
tested <- read_excel('data-raw/monthlyposting_october2019.xlsx', skip = 1)
glimpse(tested)

# how many exceed 5 ppb? what is the follow up status?
tested %>% 
  group_by(`Action_Level_Exceedance?`, ALE_Follow_Up_Status) %>% 
  summarise(count = n())

lead_present <- tested %>% 
  filter(XMOD != "<") %>% 
  group_by(SchoolName) %>% 
  mutate(maxResult = max(RESULT), unit = 'ppb', lead = TRUE) %>% 
  select(district = DISTRICT, schoolName = SchoolName, 
         schoolAddress = SchoolAddress, maxResult, unit, lead) %>% 
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
         maxResult = NA, unit = NA, lead = NA, status = "not tested") %>% 
  select(district = District, schoolName = School, schoolAddress, maxResult, 
         unit, lead, status)
View(not_tested_schools)

exempt <- read_excel('data-raw/exemption_forms.xlsx')
glimpse(exempt)

exempt_schools <- exempt %>% 
  mutate(maxResult = NA, unit = NA, lead = NA, status = "exempt") %>% 
  select(district = `School District`, schoolName = Name, schoolAddress = Address, 
         maxResult, unit, lead, status)

View(exempt_schools)

all_schools <- tested_schools %>% 
  bind_rows(not_tested_schools) %>% 
  bind_rows(exempt_schools)


exempt_schools %>% 
  filter(schoolName == 'Jensen Ranch Elementary')
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

cleaned_data <- all_schools %>% 
  left_join(district_lookup) %>% 
  mutate(district = ifelse(is.na(match), district, match),
         district = ifelse(is.na(district) | district == 'private', 'Private', district)) %>% 
  select(-match) %>%
  left_join(geo_coded)

# updates from september data
now_tested <- cleaned_data %>% 
  group_by(schoolName, schoolAddress, status) %>% 
  summarise(count = n()) %>% 
  spread(status, count) %>% 
  mutate(tot = sum(`not tested`, tested, na.rm = TRUE)) %>% 
  filter(tot == 2) %>% 
  select(schoolName, schoolAddress) %>% 
  mutate(new_status = 'tested')

still_not_tested <- cleaned_data %>% 
  filter(status == 'not tested') %>% 
  left_join(now_tested) %>% 
  filter(is.na(new_status)) %>% 
  select(-new_status)

cleaned_data <- cleaned_data %>% 
  filter(status != 'not tested') %>% 
  bind_rows(still_not_tested) 

cleaned_data %>% 
  arrange(district, schoolName) %>% 
  mutate(county = ifelse(county == 'SF', 'San Francisco County', county)) %>% names
  write_csv('ca_schools_lead_testing_data.csv') 


    # % of schools who have tested out of those required to test (so exempting exempt schools). 
tested <- cleaned_data %>% 
  filter(status != 'exempt') %>% 
  select(schoolName, status) %>% 
  unique() %>% 
  group_by(status) %>% 
  summarise(count = n()) %>% 
  pull(count)

tested[2] / (tested[1] + tested[2]) # prop tested

# Also great to know the % and number of schools and school districts that found lead over 5 PPB.
lead_found <- cleaned_data %>% 
  select(schoolName, lead) %>% 
  unique() %>%
  filter(lead) %>% 
  summarise(count = n()) %>% 
  pull(count)

lead_found / (tested[1] + tested[2]) 

district_lead_found <- cleaned_data %>% 
  select(district, lead) %>% 
  unique() %>% 
  filter(lead) %>% 
  summarise(count = n()) %>% 
  pull(count)

num_districts <- cleaned_data %>% 
  select(district) %>% 
  unique() %>% 
  summarise(count = n()) %>% 
  pull(count)

district_lead_found / num_districts

# confirm that all districts have at least one non exempt school
cleaned_data %>% 
  select(district, status) %>% 
  group_by(district, status) %>% 
  mutate(count = n()) %>% 
  unique() %>% 
  spread(status, count) %>% 
  filter(is.na(exempt), is.na(tested), is.na(`not tested`)) 

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

#la data
la <- read_excel('data-raw/LAUSD_Drinking_Water_Lead_Sample_Data_Details_-_3-18-19.xlsx')
glimpse(la)
table(la$`Current Status`)
la %>% 
  filter(`Current Status` == 'ACTIVE') %>% 
  select(schoolName = `School Name`, `Most Recent 1st Draw (ppb)`, `Most Recent 2nd Draw (ppb)`) %>% 
  gather(draw, ppb, -schoolName) %>% 
  group_by(schoolName) %>% 
  summarise(ppb = median(ppb), count = n()) %>% 
  View

glimpse(cleaned_data)
