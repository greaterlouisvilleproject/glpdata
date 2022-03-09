suppressMessages(library(tidyverse))
library(glptools)

path <- "data-raw/health/smoking/"

#Gather data
smoking_county <- read.csv(paste0(path, 'smoking_total/PLACES_2021.csv')) %>%
  filter(Measure=='Current smoking among adults aged >=18 years' & Data_Value_Type=='Age-adjusted prevalence') %>%
  select(c('LocationID','Year','Data_Value')) %>%
  mutate(sex='total',race='total') %>%
  rename('FIPS'='LocationID', 'year'='Year', 'smoking_percentage'='Data_Value') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(smoking_percentage, simple=T)

usethis::use_data(smoking_county, overwrite = TRUE)

