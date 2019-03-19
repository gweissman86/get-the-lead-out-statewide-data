# Data Cleaning

In directory `data-raw` there is the `clean_data.R` script that parses and combines three datasets provided by the [State Water Boards](https://www.waterboards.ca.gov/drinking_water/certlic/drinkingwater/leadsamplinginschools.html)

1. `MonthlyPostingMarch.xlsx` are the results of lead testing
2. `SchoolsUnsampled.xlsx` are the schools that have not been sampled yet
3. `exemption_forms.xlsx` are the schools that are exempt due to independent testing

For schools with sites that tested greater than 5 ppb lead, the median value (of values >5 ppb) at the school is used to represent the lead level.

The cleaned and combined dataset is named`ca_schools_lead_testing_data.csv`

## Data Dictionary

* **district:** name of district
* **schoolName:** name of school     
* **schoolAddress:** school address  
* **medianResult:** median lead found for test above 5 ppb
* **unit:** unit of median result (ppb = parts per billion)
* **lead:** was lead detected above 5 ppb (TRUE, FALSE, or NA if not tested yet or exempt from testing)
* **status:** testing status (tested, not tested, or NA if exempt)