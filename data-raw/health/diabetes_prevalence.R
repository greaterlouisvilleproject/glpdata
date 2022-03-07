suppressMessages(library(tidyverse))
library(glptools)

path <- "data-raw/health/diabetes_prevalence/"

#Gather data
diabetes_df_total <- glptools::any_time(paste0(path, 'total'), starting_year=2004, skip=0, col_types=NULL,read.csv)

#Process and subset data
diabetes_df_total <- diabetes_df_total %>%
  select(c('CountyFIPS','year','Percentage')) %>%
  mutate(sex='total',race='total') %>%
  rename('FIPS'='CountyFIPS','diabetes_prevalence'='Percentage') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#Male
diabetes_df_male <- glptools::any_time(paste0(path, 'male'), starting_year=2004, skip=0, col_types=NULL,read.csv) %>%
  select(c('CountyFIPS','year','Percentage')) %>%
  mutate(sex='male',race='total') %>%
  rename('FIPS'='CountyFIPS','diabetes_prevalence'='Percentage') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#Female
diabetes_df_female <- glptools::any_time(paste0(path, 'female'), starting_year=2004, skip=0, col_types=NULL,read.csv) %>%
  select(c('CountyFIPS','year','Percentage')) %>%
  mutate(sex='female',race='total') %>%
  rename('FIPS'='CountyFIPS','diabetes_prevalence'='Percentage') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#Combine all of the data
diabetes_prevalence_county <- rbind(diabetes_df_total, diabetes_df_male, diabetes_df_female)
diabetes_prevalence_county$diabetes_prevalence <- as.numeric(diabetes_prevalence_county$diabetes_prevalence)

usethis::use_data(diabetes_prevalence_county, overwrite = TRUE)

