suppressMessages(library(tidyverse))
library(glptools)

path <- "data-raw/health/physical_inactivity/"

#Gather and process data
physical_inactivity_df_total <- glptools::any_time(paste0(path, 'physical_inactivity_total'), starting_year=2004, skip=0, col_types=NULL,read.csv) %>%
  select(c('CountyFIPS','year','Percentage')) %>%
  mutate(sex='total',race='total') %>%
  rename('FIPS'='CountyFIPS','physical_inactivity'='Percentage') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#Male
physical_inactivity_df_male <- glptools::any_time(paste0(path, 'physical_inactivity_male'), starting_year=2004, skip=0, col_types=NULL,read.csv) %>%
  select(c('CountyFIPS','year','Percentage')) %>%
  mutate(sex='male',race='total') %>%
  rename('FIPS'='CountyFIPS','physical_inactivity'='Percentage') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#Female
physical_inactivity_df_female <- glptools::any_time(paste0(path, 'physical_inactivity_female'), starting_year=2004, skip=0, col_types=NULL,read.csv) %>%
  select(c('CountyFIPS','year','Percentage')) %>%
  mutate(sex='female',race='total') %>%
  rename('FIPS'='CountyFIPS','physical_inactivity'='Percentage') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#Combine all of the data
physical_inactivity_county <- rbind(physical_inactivity_df_total, physical_inactivity_df_male, physical_inactivity_df_female)
physical_inactivity_county$physical_inactivity <- as.numeric(physical_inactivity_county$physical_inactivity)

usethis::use_data(physical_inactivity_county, overwrite = TRUE)
