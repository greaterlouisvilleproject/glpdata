suppressMessages(library(tidyverse))
library(glptools)

path <- "data-raw/health/flu_vaccines/"

#Gather data
flu_df_total <- glptools::any_time(paste0(path, 'total'), starting_year=2012, skip=0, col_types=NULL,read.csv)

#Process and subset data
flu_df_total <- flu_df_total %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','flu_shot'='analysis_value') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(flu_shot, simple=T)

#Male
flu_df_male <- glptools::any_time(paste0(path, 'male'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','flu_shot'='analysis_value') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(flu_shot, simple=T)

#Female
flu_df_female <- glptools::any_time(paste0(path, 'female'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','flu_shot'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(flu_shot, simple=T)

#White
flu_df_white <- glptools::any_time(paste0(path, 'white'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','flu_shot'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(flu_shot, simple=T)

#Black
flu_df_black <- glptools::any_time(paste0(path, 'black'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','flu_shot'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(flu_shot, simple=T)

#Hispanic
flu_df_hispanic <- glptools::any_time(paste0(path, 'hispanic'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','flu_shot'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(flu_shot, simple=T)

#American Indian/Alaska Native
flu_df_aian <- glptools::any_time(paste0(path, 'american_indian_alaska_native'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','flu_shot'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(flu_shot, simple=T)

#Asian/Pacific Islander
flu_df_api <- glptools::any_time(paste0(path, 'api'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','flu_shot'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(flu_shot, simple=T)

#Other
flu_df_other <- glptools::any_time(paste0(path, 'other'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','flu_shot'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(flu_shot, simple=T)

#Combine all of the data
flu_county <- rbind(flu_df_total, flu_df_male, flu_df_female, flu_df_black, flu_df_white, flu_df_hispanic, flu_df_api, flu_df_aian, flu_df_other)

#Replace "All" in the race and sex columns with "total" to be able to use GLP graphic functions later
flu_county$race <- tolower(flu_county$race)
flu_county$sex <- tolower(flu_county$sex)
flu_county <- flu_county %>% mutate(race=replace(race, race=='all', 'total'),
                                    race=replace(race, race=='asian/pacific islander', 'API'),
                                    race=replace(race, race=='american indian/alaska native', 'AIAN'),
                                    sex=replace(sex, sex=='all', 'total'))

usethis::use_data(flu_county, overwrite = TRUE)
