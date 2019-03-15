library(tidyverse)
library(readxl)


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

un_tested <- read_excel('data-raw/SchoolsUnsampled.xlsx')
glimpse(un_tested)


exempt <- read_excel('data-raw/exemption_forms.xlsx')
glimpse(exempt)
