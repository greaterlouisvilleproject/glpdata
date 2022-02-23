suppressMessages(library(tidyverse))
library(glptools)

path <- "data-raw/health/mammography_screening/"

#Gather data
mammography_df_total <- glptools::any_time(paste0(path, 'total'), starting_year=2012, skip=0, col_types=NULL,read.csv)

#Process and subset data
mammography_df_total <- mammography_df_total %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','mammography_screening'='analysis_value') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#White
mammography_df_white <- glptools::any_time(paste0(path, 'white'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','mammography_screening'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#Black
mammography_df_black <- glptools::any_time(paste0(path, 'black'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','mammography_screening'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#Hispanic
mammography_df_hispanic <- glptools::any_time(paste0(path, 'hispanic'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','mammography_screening'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#American Indian/Alaska Native
mammography_df_aian <- glptools::any_time(paste0(path, 'american_indian_alaska_native'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','mammography_screening'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#Asian/Pacific Islander
mammography_df_api <- glptools::any_time(paste0(path, 'api'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','mammography_screening'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#Other
mammography_df_other <- glptools::any_time(paste0(path, 'other'), starting_year=2012, skip=0, col_types=NULL, read.csv) %>%
  select(c('fips','year','primary_sex','primary_race','analysis_value')) %>%
  rename('FIPS'='fips','sex'='primary_sex','race'='primary_race','mammography_screening'='analysis_value') %>%
  glptools::pull_peers( add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

#Combine all of the data
mammography_county <- rbind(mammography_df_total, mammography_df_black, mammography_df_white, mammography_df_hispanic, mammography_df_api, mammography_df_aian, mammography_df_other)

#Replace "All" in the race and sex columns with "total" to be able to use GLP graphic functions later
mammography_county <- mammography_county %>% mutate(race=replace(race, race=='All', 'total'), sex=replace(sex, sex=='All', 'total'))
mammography_county$race <- tolower(mammography_county$race)
mammography_county$sex <- tolower(mammography_county$sex)

usethis::use_data(mammography_county, overwrite = TRUE)
