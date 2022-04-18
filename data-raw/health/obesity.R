suppressMessages(library(tidyverse))
library(glptools)

path <- "data-raw/health/obesity/"

#Gather data
obesity_df_total <- glptools::any_time(paste0(path, 'total'), starting_year=2004, skip=0, col_types=NULL,read.csv)

#Process and subset data
obesity_df_total <- obesity_df_total %>%
  select(c('CountyFIPS','year','Percentage')) %>%
  mutate(sex='total',race='total') %>%
  rename('FIPS'='CountyFIPS','obesity_percentage'='Percentage') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  mutate(obesity_percentage=as.numeric(obesity_percentage)) %>%
  stl_merge(obesity_percentage, simple=T)

#Male
obesity_df_male <- glptools::any_time(paste0(path, 'male'), starting_year=2004, skip=0, col_types=NULL,read.csv) %>%
  select(c('CountyFIPS','year','Percentage')) %>%
  mutate(sex='male',race='total') %>%
  rename('FIPS'='CountyFIPS','obesity_percentage'='Percentage') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  mutate(obesity_percentage=as.numeric(obesity_percentage)) %>%
  stl_merge(obesity_percentage, simple=T)

#Female
obesity_df_female <- glptools::any_time(paste0(path, 'female'), starting_year=2004, skip=0, col_types=NULL,read.csv) %>%
  select(c('CountyFIPS','year','Percentage')) %>%
  mutate(sex='female',race='total') %>%
  rename('FIPS'='CountyFIPS','obesity_percentage'='Percentage') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  mutate(obesity_percentage=as.numeric(obesity_percentage)) %>%
  stl_merge(obesity_percentage, simple=T)

#Combine all of the data
obesity_county <- rbind(obesity_df_total, obesity_df_male, obesity_df_female)

usethis::use_data(obesity_county, overwrite = TRUE)
