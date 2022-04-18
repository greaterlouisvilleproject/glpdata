suppressMessages(library(tidyverse))
library(glptools)

path <- "data-raw/health/teen_births/"

#Gather and clean data
teen_births_total <- read.delim(paste0(path, 'teen_births_total.txt')) %>%
  select(-c(Notes,County,Year.Code)) %>%
  na.omit() %>%
  rename('FIPS'='County.Code','year'='Year','teen_births'='Births') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  mutate(race='total') %>%
  stl_merge(teen_births, simple=T)

teen_births_hispanic <- read.delim(paste0(path, 'teen_births_hispanic.txt')) %>%
  select(-c(Notes,County,Year.Code)) %>%
  na.omit() %>%
  rename('FIPS'='County.Code','year'='Year','teen_births'='Births') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  mutate(race='hispanic') %>%
  stl_merge(teen_births, simple=T)

teen_births_white_2007_2015 <- read.delim(paste0(path, 'teen_births_white_2007_2015.txt')) %>%
  select(-c(Notes,County,Year.Code)) %>%
  na.omit() %>%
  rename('FIPS'='County.Code','year'='Year','teen_births'='Births') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  mutate(race='white') %>%
  stl_merge(teen_births, simple=T)

teen_births_white_2016_2020 <- read.delim(paste0(path, 'teen_births_white_2016_2020.txt')) %>%
  select(-c(Notes,County,Year.Code)) %>%
  na.omit() %>%
  rename('FIPS'='County.Code','year'='Year','teen_births'='Births') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  mutate(race='white') %>%
  filter(teen_births!='Suppressed') %>%
  mutate(teen_births=as.numeric(teen_births)) %>%
  stl_merge(teen_births, simple=T)

teen_births_black_2007_2015 <- read.delim(paste0(path, 'teen_births_black_2007_2015.txt')) %>%
  select(-c(Notes,County,Year.Code)) %>%
  na.omit() %>%
  rename('FIPS'='County.Code','year'='Year','teen_births'='Births') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  mutate(race='black') %>%
  stl_merge(teen_births, simple=T)

teen_births_black_2016_2020 <- read.delim(paste0(path, 'teen_births_black_2016_2020.txt')) %>%
  select(-c(Notes,County,Year.Code)) %>%
  na.omit() %>%
  rename('FIPS'='County.Code','year'='Year','teen_births'='Births') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  mutate(race='black') %>%
  filter(teen_births!='Suppressed') %>%
  mutate(teen_births=as.numeric(teen_births)) %>%
  stl_merge(teen_births, simple=T)

teen_births_county <- rbind(teen_births_total, teen_births_hispanic, teen_births_white_2007_2015, teen_births_white_2016_2020, teen_births_black_2007_2015, teen_births_black_2016_2020)

#Pull the overall population data of women, 15-19, in each area
#2005 - 2019 ACS Data
population_vars_1yr <- build_census_var_df('acs1', 'B01001')

population_all_counties_1yr <- get_census(population_vars_1yr, 'FIPS')

population_women_15_19_1yr <- population_all_counties_1yr %>%
  mutate(human=T) %>%
  process_census(cat_var='human', age_groups='15_19', output_name='population') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  filter(sex=='female', var_type=='population', year>2006)

#Merge the CDC teen_birth data and USCB population data and calculate the teen births per 1,000
teen_births_county <- merge(teen_births_county, population_women_15_19_1yr, by.x=c("FIPS", "year","race"), by.y=c("FIPS", "year","race"))
teen_births_county$teen_births <- as.numeric(teen_births_county$teen_births)

teen_births_county <- teen_births_county %>%
  mutate(teen_births_per_1000 = (teen_births/population)*1000)

usethis::use_data(teen_births_county, overwrite = TRUE)

