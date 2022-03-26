suppressMessages(library(tidyverse))
library(glptools)

#Dataframe with the FIPS and the total population that is considered low income and beyond
#1 mile from a supermarket for urban areas or 10 miles for rural areas

#Documentation note:
#LALOWI1_10 = Low income population count beyond 1 mile for urban areas or 10 miles for rural areas from supermarket

df_food_access_2019 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2019.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'LALOWI1_10')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5), LALOWI1_10=as.numeric(LALOWI1_10)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(LALOWI1_10)) %>%
  rename('FIPS'='CensusTract','low_income_low_access'='sum(LALOWI1_10)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(low_income_low_access, simple=T)

#Now merge this data with 2019 population data from the ACS

population_vars_1yr <- build_census_var_df('acs1', 'B01001')

population_all_counties_1yr <- get_census(population_vars_1yr, 'FIPS') %>%
  filter(var_type=='estimate', age_group=='all', year==2019)

simpler_population_df <- population_all_counties_1yr %>%
  filter(sex=='total', race=='total') %>%
  select(c('FIPS','value')) %>%
  rename('population2019'='value') %>%
  stl_merge(population2019, simple=T)

df_food_access_2019 <- merge(df_food_access_2019, simpler_population_df) %>%
  mutate(low_access_percent = 100*(low_income_low_access/population2019), year=2019)

#Now for the 2015 data

df_food_access_2015 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2015.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'LALOWI1_10')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5), LALOWI1_10=as.numeric(LALOWI1_10)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(LALOWI1_10)) %>%
  rename('FIPS'='CensusTract','low_income_low_access'='sum(LALOWI1_10)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(low_income_low_access, simple=T)

#Now merge this data with 2015 population data from the ACS

population_2015 <- get_census(population_vars_1yr, 'FIPS') %>%
  filter(var_type=='estimate', age_group=='all', year==2015)

simpler_population_df_2015 <- population_2015 %>%
  filter(sex=='total', race=='total') %>%
  select(c('FIPS','value')) %>%
  rename('population2015'='value') %>%
  stl_merge(population2015, simple=T)

df_food_access_2015 <- merge(df_food_access_2015, simpler_population_df_2015) %>%
  mutate(low_access_percent = 100*(low_income_low_access/population2015), year=2015)

#Combine all of the data
food_access_county <- dplyr::bind_rows(df_food_access_2015, df_food_access_2019)

usethis::use_data(food_access_county, overwrite = TRUE)





#Now for race data
#We have data by race and county on the count of people beyond 1 mile from a supermarket

#Documentation notes:
#lawhite1 = White population count beyond 1 mile from supermarket
#lablack1 = Black or African American population count beyond 1 mile from supermarket
#laasian1 = Asian population count beyond 1 mile from supermarket
#laaian1 = American Indian or Alaska Native population count beyond 1 mile from supermarket
#laomultir1 = Other/Multiple race population count beyond 1 mile from supermarket
#lahisp1 = Hispanic or Latino ethnicity population count beyond 1 mile from supermarket

food_access_white_2019 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2019.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'lawhite1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         lawhite1=as.numeric(lawhite1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(lawhite1)) %>%
  rename('FIPS'='CensusTract',
         'white'='sum(lawhite1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(white, simple=T)

food_access_black_2019 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2019.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'lablack1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         lablack1=as.numeric(lablack1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(lablack1)) %>%
  rename('FIPS'='CensusTract',
         'black'='sum(lablack1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(black, simple=T)

food_access_asian_2019 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2019.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'laasian1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         laasian1=as.numeric(laasian1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(laasian1)) %>%
  rename('FIPS'='CensusTract',
         'asian'='sum(laasian1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(asian, simple=T)

food_access_AIAN_2019 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2019.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'laaian1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         laaian1=as.numeric(laaian1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(laaian1)) %>%
  rename('FIPS'='CensusTract',
         'AIAN'='sum(laaian1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(AIAN, simple=T)

food_access_other_2019 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2019.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'laomultir1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         laomultir1=as.numeric(laomultir1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(laomultir1)) %>%
  rename('FIPS'='CensusTract',
         'other'='sum(laomultir1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(other, simple=T)

food_access_hispanic_2019 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2019.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'lahisp1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         lahisp1=as.numeric(lahisp1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(lahisp1)) %>%
  rename('FIPS'='CensusTract',
         'hispanic'='sum(lahisp1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(hispanic, simple=T)

df_list_2019 <- list(food_access_white_2019, food_access_black_2019, food_access_asian_2019, food_access_AIAN_2019,
                food_access_other_2019, food_access_hispanic_2019)

food_access_race_merged_2019 <- df_list_2019 %>%
  reduce(full_join, by='FIPS')

food_access_race_merged_2019 <- gather(food_access_race_merged_2019,
                   key = "race",
                   value = "count",
                   -FIPS)

food_access_race_merged_2019 <- food_access_race_merged_2019 %>%
  mutate(year=2019)

#Merge the 2019 population data
simpler_population_race_2019 <- population_all_counties_1yr %>%
  filter(sex=='total') %>%
  select(c('FIPS', 'race', 'value')) %>%
  rename('population2019'='value') %>%
  stl_merge(population2019, simple=T)

food_access_race_merged_2019 <- merge(food_access_race_merged_2019, simpler_population_race_2019, by=c("FIPS", "race"))

food_access_race_merged_2019 <- food_access_race_merged_2019 %>%
  mutate(percent_more_than_10_miles_grocery = 100*(count/population2019))

#Now for the 2015 race data

food_access_white_2015 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2015.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'lawhite1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         lawhite1=as.numeric(lawhite1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(lawhite1)) %>%
  rename('FIPS'='CensusTract',
         'white'='sum(lawhite1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(white, simple=T)

food_access_black_2015 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2015.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'lablack1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         lablack1=as.numeric(lablack1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(lablack1)) %>%
  rename('FIPS'='CensusTract',
         'black'='sum(lablack1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(black, simple=T)

food_access_asian_2015 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2015.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'laasian1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         laasian1=as.numeric(laasian1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(laasian1)) %>%
  rename('FIPS'='CensusTract',
         'asian'='sum(laasian1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(asian, simple=T)

food_access_AIAN_2015 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2015.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'laaian1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         laaian1=as.numeric(laaian1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(laaian1)) %>%
  rename('FIPS'='CensusTract',
         'AIAN'='sum(laaian1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(AIAN, simple=T)

food_access_other_2015 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2015.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'laomultir1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         laomultir1=as.numeric(laomultir1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(laomultir1)) %>%
  rename('FIPS'='CensusTract',
         'other'='sum(laomultir1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(other, simple=T)

food_access_hispanic_2015 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2015.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'lahisp1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         lahisp1=as.numeric(lahisp1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(lahisp1)) %>%
  rename('FIPS'='CensusTract',
         'hispanic'='sum(lahisp1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(hispanic, simple=T)

df_list_2015 <- list(food_access_white_2015, food_access_black_2015, food_access_asian_2015, food_access_AIAN_2015,
                     food_access_other_2015, food_access_hispanic_2015)

food_access_race_merged_2015 <- df_list_2015 %>%
  reduce(full_join, by='FIPS')

food_access_race_merged_2015 <- gather(food_access_race_merged_2015,
                                       key = "race",
                                       value = "count",
                                       -FIPS)

food_access_race_merged_2015 <- food_access_race_merged_2015 %>%
  mutate(year=2015)

#Merge the 2015 population data
simpler_population_race_2015 <- population_2015 %>%
  filter(sex=='total') %>%
  select(c('FIPS', 'race', 'value')) %>%
  rename('population2015'='value') %>%
  stl_merge(population2015, simple=T)

food_access_race_merged_2015 <- merge(food_access_race_merged_2015, simpler_population_race_2015, by=c("FIPS", "race"))

food_access_race_merged_2015 <- food_access_race_merged_2015 %>%
  mutate(percent_more_than_10_miles_grocery = 100*(count/population2015))

#Combine all of the data
food_access_race_county <- dplyr::bind_rows(food_access_race_merged_2015, food_access_race_merged_2019)

usethis::use_data(food_access_race_county, overwrite = TRUE)





#Now for data related to households receiving SNAP benefits and beyond a certain distance from the supermarket

#Documentation notes
#lasnap1 = Housing units receiving SNAP benefits beyond 1 mile from a supermarket
#Note: Data for distances above 1 mile are largely null in the dataset. We'll stick with 1 mile.

df_snap_low_access <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2019.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'lasnap1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         lasnap1=as.numeric(lasnap1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(lasnap1)) %>%
  rename('FIPS'='CensusTract','SNAP_households_beyond_1_mile'='sum(lasnap1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(SNAP_households_beyond_1_mile, simple=T) %>%
  mutate(year=2019)

#Merge with the total households in 2019
household_vars_1yr <- build_census_var_df('acs1', 'B11001')

household_counties_1yr <- get_census(household_vars_1yr, 'FIPS')

households_2019 <- household_counties_1yr %>%
  filter(year==2019, var_type=='estimate', race=='total', label=='Estimate!!Total:') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  select(c('FIPS','value')) %>%
  rename('households2019'='value') %>%
  stl_merge(households2019, simple=T)

df_snap_2019_merged <- merge(df_snap_low_access, households_2019) %>%
  mutate(percent_households_snap_low_access = 100*(SNAP_households_beyond_1_mile/households2019))

#Now for 2015 SNAP data

df_snap_low_access_2015 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2015.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'lasnap1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         lasnap1=as.numeric(lasnap1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(lasnap1)) %>%
  rename('FIPS'='CensusTract','SNAP_households_beyond_1_mile'='sum(lasnap1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(SNAP_households_beyond_1_mile, simple=T) %>%
  mutate(year=2015)

households_2015 <- household_counties_1yr %>%
  filter(year==2015, var_type=='estimate', race=='total', label=='Estimate!!Total') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  select(c('FIPS','value')) %>%
  rename('households2015'='value') %>%
  stl_merge(households2015, simple=T)

df_snap_2015_merged <- merge(df_snap_low_access_2015, households_2015) %>%
  mutate(percent_households_snap_low_access = 100*(SNAP_households_beyond_1_mile/households2015))

food_access_snap_county <- dplyr::bind_rows(df_snap_2015_merged, df_snap_2019_merged)

usethis::use_data(food_access_snap_county, overwrite = TRUE)





#Now for data related to households that don't have a vehicle and are more than 1 mile from a supermarket

#Documentation notes
#lahunv1 = Housing units without a vehicle and beyond 1 mile from a supermarket
#Note: Data for distances above 1 mile are largely null in the dataset. We'll stick with 1 mile.

df_no_vehicle_low_access <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2019.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'lahunv1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         lahunv1=as.numeric(lahunv1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(lahunv1)) %>%
  rename('FIPS'='CensusTract','no_vehicle_households_beyond_1_mile'='sum(lahunv1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(no_vehicle_households_beyond_1_mile, simple=T) %>%
  mutate(year=2019)

#Merge with the total households in 2019 (dataframe carved out above)

df_no_vehicle_merged_2019 <- merge(df_no_vehicle_low_access, households_2019) %>%
  mutate(percent_households_no_vehicle_low_access = 100*(no_vehicle_households_beyond_1_mile/households2019))

#Now for 2015 data

df_no_vehicle_low_access_2015 <- readxl::read_xlsx("data-raw/qop/food_access/FoodAccessResearchAtlasData2015.xlsx", sheet = 'Food Access Research Atlas') %>%
  select(c('CensusTract', 'lahunv1')) %>%
  mutate(CensusTract=substr(CensusTract, start = 1, stop = 5),
         lahunv1=as.numeric(lahunv1)) %>%
  group_by(CensusTract) %>%
  na.omit() %>%
  summarize(sum(lahunv1)) %>%
  rename('FIPS'='CensusTract','no_vehicle_households_beyond_1_mile'='sum(lahunv1)') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(no_vehicle_households_beyond_1_mile, simple=T) %>%
  mutate(year=2015)

#Merge with the total households in 2015 (dataframe carved out above)

df_no_vehicle_merged_2015 <- merge(df_no_vehicle_low_access_2015, households_2015) %>%
  mutate(percent_households_no_vehicle_low_access = 100*(no_vehicle_households_beyond_1_mile/households2015))

food_access_no_vehicle_county <- dplyr::bind_rows(df_no_vehicle_merged_2015, df_no_vehicle_merged_2019)

usethis::use_data(food_access_no_vehicle_county, overwrite = TRUE)


