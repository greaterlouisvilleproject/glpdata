library(tidyverse)
library(magrittr)
library(glptools)

path <- "data-raw/health/natality/"

#All

#Read and clean data
all_03 <- read_tsv(path %+% "all_03.txt")
all_07 <- read_tsv(path %+% "all_07.txt")
underweight_03 <- read_tsv(path %+% "underweight_03.txt")
underweight_07 <- read_tsv(path %+% "underweight_07.txt")

all_03 %<>% clean_wonder()
all_07 %<>% clean_wonder()
underweight_03 %<>% clean_wonder()
underweight_07 %<>% clean_wonder()

#Bind data frames
all <- bind_rows(all_03, all_07)
underweight <- bind_rows(underweight_03, underweight_07)

underweight %<>% rename(underweight = births)

natality <- left_join(all, underweight, by = c("FIPS", "year"))

#Create underweight bith variable
natality %<>%
  mutate(
    underweight = underweight / births * 100,
    race = "total",
    sex = "total")


#Race
#Read and clean data

all_bw_03 <- read_tsv(path %+% "all_bw_03.txt")
all_bw_07 <- read_tsv(path %+% "all_bw_07.txt")
underweight_bw_03 <- read_tsv(path %+% "underweight_bw_03.txt")
underweight_bw_07 <- read_tsv(path %+% "underweight_bw_07.txt")
all_h_03 <- read_tsv(path %+% "all_h_03.txt")
all_h_07 <- read_tsv(path %+% "all_h_07.txt")
underweight_h_03 <- read_tsv(path %+% "underweight_h_03.txt")
underweight_h_07 <- read_tsv(path %+% "underweight_h_07.txt")

all_bw_03 %<>% clean_wonder()
all_bw_07 %<>% clean_wonder()
underweight_bw_03 %<>% clean_wonder()
underweight_bw_07 %<>% clean_wonder()
all_h_03 %<>% clean_wonder() %>% mutate(race = "hispanic")
all_h_07 %<>% clean_wonder() %>% mutate(race = "hispanic")
underweight_h_03 %<>% clean_wonder() %>% mutate(race = "hispanic")
underweight_h_07 %<>% clean_wonder() %>% mutate(race = "hispanic")

#Bind data frames
all_race <- bind_rows(all_bw_03, all_bw_07, all_h_03, all_h_07)
underweight_race <- bind_rows(underweight_bw_03, underweight_bw_07, underweight_h_03, underweight_h_07)

underweight_race %<>% rename(underweight = births)

natality_race <- left_join(all_race, underweight_race, by = c("FIPS", "year", "race"))

#Create underweight birth variable
natality_race %<>%
  mutate(
    underweight = underweight / births * 100,
    sex = "total")

natality <- bind_rows(natality, natality_race) %>%
  select(-births) %>%
  organize()

rm(all, all_03, all_07, all_bw_03, all_bw_07, all_h_03, all_h_07, all_race,
   underweight, underweight_03, underweight_07, underweight_bw_03, underweight_bw_07,
   underweight_h_03, underweight_h_07, underweight_race, natality_race, path)

#Mortality: Years of Potential Life Lost

path <- "data-raw/health/mortality/"

process_mortality <- function(df, race_name){
  #Extract data for 10 most populous counties
  totals <- df %>%
    filter(FIPS ==  "total") %>%
    select(-FIPS) %>%
    rename(
      total_deaths = deaths,
      total_population = population)

  #append data to df and calculate ypll per 100,000 residents
  df %<>%
    filter(FIPS != "total") %>%
    left_join(totals, by = c("year", "age")) %>%
    mutate(
      deaths = deaths - total_deaths,
      population = population - total_population) %>%
    select(-total_deaths, -total_population) %>%
    mutate(ypll = deaths * (75 - age)) %>%
    age_adj_rate(var = "ypll") %>%
    mutate(race = race_name) %>%
    organize()

  df
}

all <- wonder_time(path %+% "total")
blackwhite <- wonder_time(path %+% "bw")
hispanic <- wonder_time(path %+% "h")

all %<>% process_mortality("total")
white <- blackwhite %>%
  filter(race == "white") %>%
  select(-race) %>%
  process_mortality("white")
black <- blackwhite %>%
  filter(race == "black") %>%
  select(-race) %>%
  process_mortality("black")
hispanic %<>% process_mortality("hispanic")

mortality <- bind_rows(all, white, black, hispanic) %>%
  mutate(sex = "total") %>%
  organize()

rm(all, black, blackwhite, hispanic, white, path, process_mortality)


#BRFSS

library(Hmisc)
library(labelled)
library(survey)

path <- "data-raw/health/brfss/"

#brfss_time(path)

brfss <- read_feather(path %+% "brfss.feather")

brfss %<>%
  rename(MSA = msa) %>%
  pull_peers_MSA() %>%
  organize()

#recode data
brfss$age[brfss$year >= 2003] <- plyr::mapvalues(brfss$age[brfss$year >= 2011], 1:6, c(1, 1, 1, 2, 2, 3))

brfss$poor_or_fair <- if_else(brfss$hlth == 4 | brfss$hlth == 5, 1, 0)
brfss$poor_or_fair[brfss$hlth == 7 | brfss$hlth == 9] <- NA

brfss$physdays[brfss$physdays == 77 | brfss$physdays == 99] <- NA
brfss$physdays[brfss$physdays == 88] <- 0

brfss$mentdays[brfss$mentdays == 77 | brfss$mentdays == 99] <- NA
brfss$mentdays[brfss$mentdays == 88] <- 0

brfss <- brfss %>% filter(year > 2003)

#create survey objects
brfss_svy <- svydesign(ids = ~0, weights = ~wgt, data = brfss)
pop_age <- c(0.530534557, 0.299194019, 0.170271424)

brfss_svy_std <- svystandardize(brfss_svy, by = ~age, over = ~MSA+year, population = pop_age, excluding.missing = ~poor_or_fair+physdays+mentdays)

brfss <- svyby(~poor_or_fair+physdays+mentdays, by = ~MSA+year, design = brfss_svy_std, svymean, na.rm = TRUE)

brfss %<>%
  select(MSA, year, poor_or_fair, physdays, mentdays) %>%
  mutate(poor_or_fair = poor_or_fair * 100)

greensboro <- brfss[brfss$city == "Greensboro" & brfss$year == 2014,]

greensboro$year <- 2015
brfss <- rbind(brfss, greensboro)

greensboro$year <- 2016
brfss <- rbind(brfss, greensboro)

greensboro$year <- 2017
brfss <- rbind(brfss, greensboro)

brfss %<>%
  mutate(
    race = "total",
    sex = "total") %>%
  organize()


#Health index
brfss_FIPS <- brfss %>%
  add_FIPS_to_MSA() %>%
  select(-MSA)

health_index <- bind_df(mortality, natality, brfss_FIPS) %>%
  pull_peers_FIPS()

#Create index
norm_z <- function(x){
  z <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

health_z <- health_index %>%
  filter(current == 1) %>%
  select(-city, -baseline, -current)
#health_z <- map_df(health_z, remove_var_label)

health_z <- health_z %>%
  group_by(year) %>%
  mutate_at(vars(ypll:mentdays), norm_z) %>%
  mutate_at(vars(ypll:mentdays), funs(. * -1)) %>%
  ungroup() %>%
  rename_at(vars(ypll:mentdays), paste0, "_index") %>%
  mutate(
    health_index =
      ypll_index * .5 +
      underweight_index * .2 +
      poor_or_fair_index * .1 +
      physdays_index * .1 +
      mentdays_index * .1)

health_index %<>%
  bind_df(health_z) %>%
  organize()

trend_single(health_index, health_index)

rm(msa_fips, natality, mortality, brfss_results)

usethis::use_data(mortality, overwrite = TRUE)
usethis::use_data(natality, overwrite = TRUE)
usethis::use_data(brfss, overwrite = TRUE)
usethis::use_data(health_index, overwrite = TRUE)

rm(greensboro, brfss_FIPS, health_z)
