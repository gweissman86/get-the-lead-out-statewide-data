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
  mutate(medianResult = median(RESULT), unit = 'ppb') %>% 
  select(district = DISTRICT, schoolName = SchoolName, 
         schoolAddress = SchoolAddress, medianResult, unit) %>% 
  unique() %>% 
  ungroup()

schools_with_lead <- lead_present %>% 
  select(schoolName, schoolAddress) %>% 
  unique() %>% 
  mutate(lead = TRUE)


schools <- tested %>% 
  select(district = DISTRICT, schoolName = SchoolName, schoolAddress = SchoolAddress) %>% 
  unique() %>% 
  left_join(schools_with_lead) %>% 
  mutate(status = 'tested')



un_tested <- read_excel('data-raw/SchoolsUnsampled.xlsx')
glimpse(un_tested)


exempt <- read_excel('data-raw/exemption_forms.xlsx')
glimpse(exempt)
