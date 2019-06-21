library(tidyverse)
library(readxl)

# tested <- read_excel('data-raw/MonthlyPostingMarch.xlsx', skip = 1)
tested <- read_excel('data-raw/monthlypostingjune.xlsx', skip = 1)
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

tested_schools %>% 
  bind_rows(not_tested_schools) %>% 
  bind_rows(exempt_schools) %>% 
  write_csv('ca_schools_lead_testing_data.csv')
