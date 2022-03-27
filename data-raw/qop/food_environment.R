suppressMessages(library(tidyverse))
library(glptools)

#Data about the number of grocery stores, convenience stores, fast food restaurants and farmers'
#markets per 100,000 people.

#Documentation Notes:
#The years of data come from different Excel sheets in the food_environment folder. (That is the
#reason for the inconsistent ordering of the years)
#GROCPTH16 = grocery stores per 1,000 people in 2016
#GROCPTH11 = grocery stores per 1,000 people in 2011
#GROCPTH14 = grocery stores per 1,000 people in 2014
#GROCPTH09 = grocery stores per 1,000 people in 2009
#GROCPTH12 = grocery stores per 1,000 people in 2012
#GROCPTH07 = grocery stores per 1,000 people in 2007

df_grocery_stores_2016 <- readxl::read_xls("data-raw/qop/food_environment/FoodEnvironmentAtlas.xls", sheet = 'STORES') %>%
  select(c('FIPS', 'GROCPTH16')) %>%
  rename('grocery_stores_per_1000'='GROCPTH16') %>%
  mutate(grocery_stores_per_100000=100*grocery_stores_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(grocery_stores_per_100000, simple=T) %>%
  mutate(year=2016)

df_grocery_stores_2011 <- readxl::read_xls("data-raw/qop/food_environment/FoodEnvironmentAtlas.xls", sheet = 'STORES') %>%
  select(c('FIPS', 'GROCPTH11')) %>%
  rename('grocery_stores_per_1000'='GROCPTH11') %>%
  mutate(grocery_stores_per_100000=100*grocery_stores_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(grocery_stores_per_100000, simple=T) %>%
  mutate(year=2011)

df_grocery_stores_2014 <- readxl::read_xls("data-raw/qop/food_environment/DataDownload.xls", sheet = 'STORES') %>%
  select(c('FIPS', 'GROCPTH14')) %>%
  rename('grocery_stores_per_1000'='GROCPTH14') %>%
  mutate(grocery_stores_per_100000=100*grocery_stores_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(grocery_stores_per_100000, simple=T) %>%
  mutate(year=2014)

df_grocery_stores_2009 <- readxl::read_xls("data-raw/qop/food_environment/DataDownload.xls", sheet = 'STORES') %>%
  select(c('FIPS', 'GROCPTH09')) %>%
  rename('grocery_stores_per_1000'='GROCPTH09') %>%
  mutate(grocery_stores_per_100000=100*grocery_stores_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(grocery_stores_per_100000, simple=T) %>%
  mutate(year=2009)

df_grocery_stores_2012 <- readxl::read_xls("data-raw/qop/food_environment/2015 Food Environment Atlas Data Download.xls", sheet = 'STORES') %>%
  select(c('FIPS', 'GROCPTH12')) %>%
  rename('grocery_stores_per_1000'='GROCPTH12') %>%
  mutate(grocery_stores_per_100000=100*grocery_stores_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(grocery_stores_per_100000, simple=T) %>%
  mutate(year=2012)

df_grocery_stores_2007 <- readxl::read_xls("data-raw/qop/food_environment/2015 Food Environment Atlas Data Download.xls", sheet = 'STORES') %>%
  select(c('FIPS', 'GROCPTH07')) %>%
  rename('grocery_stores_per_1000'='GROCPTH07') %>%
  mutate(grocery_stores_per_100000=100*grocery_stores_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(grocery_stores_per_100000, simple=T) %>%
  mutate(year=2007)

grocery_store_county <- rbind(df_grocery_stores_2007, df_grocery_stores_2009, df_grocery_stores_2011,
                              df_grocery_stores_2012, df_grocery_stores_2014, df_grocery_stores_2016)


#Documentation Notes:
#CONVSPTH16 = convenience stores per 1,000 people in 2016
#CONVSPTH11 = convenience stores per 1,000 people in 2011
#CONVSPTH14 = convenience stores per 1,000 people in 2014
#CONVSPTH09 = convenience stores per 1,000 people in 2009
#CONVSPTH12 = convenience stores per 1,000 people in 2012
#CONVSPTH07 = convenience stores per 1,000 people in 2007

df_convenience_stores_2016 <- readxl::read_xls("data-raw/qop/food_environment/FoodEnvironmentAtlas.xls", sheet = 'STORES') %>%
  select(c('FIPS', 'CONVSPTH16')) %>%
  rename('convenience_stores_per_1000'='CONVSPTH16') %>%
  mutate(convenience_stores_per_100000=100*convenience_stores_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(convenience_stores_per_100000, simple=T) %>%
  mutate(year=2016)

df_convenience_stores_2011 <- readxl::read_xls("data-raw/qop/food_environment/FoodEnvironmentAtlas.xls", sheet = 'STORES') %>%
  select(c('FIPS', 'CONVSPTH11')) %>%
  rename('convenience_stores_per_1000'='CONVSPTH11') %>%
  mutate(convenience_stores_per_100000=100*convenience_stores_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(convenience_stores_per_100000, simple=T) %>%
  mutate(year=2011)

df_convenience_stores_2014 <- readxl::read_xls("data-raw/qop/food_environment/DataDownload.xls", sheet = 'STORES') %>%
  select(c('FIPS', 'CONVSPTH14')) %>%
  rename('convenience_stores_per_1000'='CONVSPTH14') %>%
  mutate(convenience_stores_per_100000=100*convenience_stores_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(convenience_stores_per_100000, simple=T) %>%
  mutate(year=2014)

df_convenience_stores_2009 <- readxl::read_xls("data-raw/qop/food_environment/DataDownload.xls", sheet = 'STORES') %>%
  select(c('FIPS', 'CONVSPTH09')) %>%
  rename('convenience_stores_per_1000'='CONVSPTH09') %>%
  mutate(convenience_stores_per_100000=100*convenience_stores_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(convenience_stores_per_100000, simple=T) %>%
  mutate(year=2009)

df_convenience_stores_2012 <- readxl::read_xls("data-raw/qop/food_environment/2015 Food Environment Atlas Data Download.xls", sheet = 'STORES') %>%
  select(c('FIPS', 'CONVSPTH12')) %>%
  rename('convenience_stores_per_1000'='CONVSPTH12') %>%
  mutate(convenience_stores_per_100000=100*convenience_stores_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(convenience_stores_per_100000, simple=T) %>%
  mutate(year=2012)

df_convenience_stores_2007 <- readxl::read_xls("data-raw/qop/food_environment/2015 Food Environment Atlas Data Download.xls", sheet = 'STORES') %>%
  select(c('FIPS', 'CONVSPTH07')) %>%
  rename('convenience_stores_per_1000'='CONVSPTH07') %>%
  mutate(convenience_stores_per_100000=100*convenience_stores_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(convenience_stores_per_100000, simple=T) %>%
  mutate(year=2007)

convenience_store_county <- rbind(df_convenience_stores_2007, df_convenience_stores_2009, df_convenience_stores_2011,
                              df_convenience_stores_2012, df_convenience_stores_2014, df_convenience_stores_2016)


#Documentation Notes:
#FFRPTH16 = fast food restaurants per 1,000 people in 2016
#FFRPTH11 = fast food restaurants per 1,000 people in 2011
#FFRPTH14 = fast food restaurants per 1,000 people in 2014
#FFRPTH09 = fast food restaurants per 1,000 people in 2009
#FFRPTH12 = fast food restaurants per 1,000 people in 2012
#FFRPTH07 = fast food restaurants per 1,000 people in 2007

df_fast_food_2016 <- readxl::read_xls("data-raw/qop/food_environment/FoodEnvironmentAtlas.xls", sheet = 'RESTAURANTS') %>%
  select(c('FIPS', 'FFRPTH16')) %>%
  rename('fast_food_per_1000'='FFRPTH16') %>%
  mutate(fast_food_per_100000=100*fast_food_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(fast_food_per_100000, simple=T) %>%
  mutate(year=2016)

df_fast_food_2011 <- readxl::read_xls("data-raw/qop/food_environment/FoodEnvironmentAtlas.xls", sheet = 'RESTAURANTS') %>%
  select(c('FIPS', 'FFRPTH11')) %>%
  rename('fast_food_per_1000'='FFRPTH11') %>%
  mutate(fast_food_per_100000=100*fast_food_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(fast_food_per_100000, simple=T) %>%
  mutate(year=2011)

df_fast_food_2014 <- readxl::read_xls("data-raw/qop/food_environment/DataDownload.xls", sheet = 'RESTAURANTS') %>%
  select(c('FIPS', 'FFRPTH14')) %>%
  rename('fast_food_per_1000'='FFRPTH14') %>%
  mutate(fast_food_per_100000=100*fast_food_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(fast_food_per_100000, simple=T) %>%
  mutate(year=2014)

df_fast_food_2009 <- readxl::read_xls("data-raw/qop/food_environment/DataDownload.xls", sheet = 'RESTAURANTS') %>%
  select(c('FIPS', 'FFRPTH09')) %>%
  rename('fast_food_per_1000'='FFRPTH09') %>%
  mutate(fast_food_per_100000=100*fast_food_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(fast_food_per_100000, simple=T) %>%
  mutate(year=2009)

df_fast_food_2012 <- readxl::read_xls("data-raw/qop/food_environment/2015 Food Environment Atlas Data Download.xls", sheet = 'RESTAURANTS') %>%
  select(c('FIPS', 'FFRPTH12')) %>%
  rename('fast_food_per_1000'='FFRPTH12') %>%
  mutate(fast_food_per_100000=100*fast_food_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(fast_food_per_100000, simple=T) %>%
  mutate(year=2012)

df_fast_food_2007 <- readxl::read_xls("data-raw/qop/food_environment/2015 Food Environment Atlas Data Download.xls", sheet = 'RESTAURANTS') %>%
  select(c('FIPS', 'FFRPTH07')) %>%
  rename('fast_food_per_1000'='FFRPTH07') %>%
  mutate(fast_food_per_100000=100*fast_food_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(fast_food_per_100000, simple=T) %>%
  mutate(year=2007)

fast_food_county <- rbind(df_fast_food_2007, df_fast_food_2009, df_fast_food_2011,
                                  df_fast_food_2012, df_fast_food_2014, df_fast_food_2016)


#Documentation Notes:
#FMRKTPTH18 = farmers' markets per 1,000 people in 2018
#FMRKTPTH13 = farmers' markets per 1,000 people in 2013
#FMRKTPTH16 = farmers' markets per 1,000 people in 2016
#FMRKTPTH09 = farmers' markets per 1,000 people in 2009

df_farmers_market_2018 <- readxl::read_xls("data-raw/qop/food_environment/FoodEnvironmentAtlas.xls", sheet = 'LOCAL') %>%
  select(c('FIPS', 'FMRKTPTH18')) %>%
  rename('farmers_markets_per_1000'='FMRKTPTH18') %>%
  mutate(farmers_markets_per_100000=100*farmers_markets_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(farmers_markets_per_100000, simple=T) %>%
  mutate(year=2018)

df_farmers_market_2013 <- readxl::read_xls("data-raw/qop/food_environment/FoodEnvironmentAtlas.xls", sheet = 'LOCAL') %>%
  select(c('FIPS', 'FMRKTPTH13')) %>%
  rename('farmers_markets_per_1000'='FMRKTPTH13') %>%
  mutate(farmers_markets_per_100000=100*farmers_markets_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(farmers_markets_per_100000, simple=T) %>%
  mutate(year=2013)

df_farmers_market_2016 <- readxl::read_xls("data-raw/qop/food_environment/DataDownload.xls", sheet = 'LOCAL') %>%
  select(c('FIPS', 'FMRKTPTH16')) %>%
  rename('farmers_markets_per_1000'='FMRKTPTH16') %>%
  mutate(farmers_markets_per_100000=100*farmers_markets_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(farmers_markets_per_100000, simple=T) %>%
  mutate(year=2016)

df_farmers_market_2009 <- readxl::read_xls("data-raw/qop/food_environment/DataDownload.xls", sheet = 'LOCAL') %>%
  select(c('FIPS', 'FMRKTPTH09')) %>%
  rename('farmers_markets_per_1000'='FMRKTPTH09') %>%
  mutate(farmers_markets_per_100000=100*farmers_markets_per_1000) %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(farmers_markets_per_100000, simple=T) %>%
  mutate(year=2009)

farmers_markets_county <- rbind(df_farmers_market_2009, df_farmers_market_2013,
                                df_farmers_market_2016, df_farmers_market_2018)

food_environment_county <- grocery_store_county  %>%
  inner_join(convenience_store_county, by=c('FIPS','year')) %>%
  inner_join(fast_food_county, by=c('FIPS','year')) %>%
  full_join(farmers_markets_county, by=c('FIPS','year'))

usethis::use_data(food_environment_county, overwrite = TRUE)
