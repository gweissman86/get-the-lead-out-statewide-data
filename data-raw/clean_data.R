library(tidyverse)
library(readxl)


tested <- read_excel('data-raw/MonthlyPostingFebruary.xlsx', skip = 1)
glimpse(tested)
un_tested <- read_excel('data-raw/SchoolsUnsampled.xlsx')
glimpse(un_tested)
