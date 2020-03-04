library(tidyverse)
library(magrittr)
library(glptools)

path <- "data-raw/health/natality/"

# All

# Read and clean data
all_03 <- read_tsv(path %p% "all_03.txt")
all_07 <- read_tsv(path %p% "all_07.txt")
underweight_03 <- read_tsv(path %p% "underweight_03.txt")
underweight_07 <- read_tsv(path %p% "underweight_07.txt")

all_03 %<>% clean_wonder()
all_07 %<>% clean_wonder()
underweight_03 %<>% clean_wonder()
underweight_07 %<>% clean_wonder()

# Bind data frames
all <- bind_rows(all_03, all_07)
underweight <- bind_rows(underweight_03, underweight_07)

underweight %<>% rename(underweight = births)

natality_total <- full_join(all, underweight, by = c("FIPS", "year"))

# Create underweight bith variable
natality_total %<>%
  mutate(
    underweight = underweight / births * 100,
    race = "total",
    sex = "total")


# Sex

# Read and clean data
all_sex_03 <- read_tsv(path %p% "all_sex_03.txt")
all_sex_07 <- read_tsv(path %p% "all_sex_07.txt")
underweight_sex_03 <- read_tsv(path %p% "underweight_sex_03.txt")
underweight_sex_07 <- read_tsv(path %p% "underweight_sex_07.txt")

all_sex_03 %<>% clean_wonder()
all_sex_07 %<>% clean_wonder()
underweight_sex_03 %<>% clean_wonder()
underweight_sex_07 %<>% clean_wonder()

# Bind data frames
all_sex <- bind_rows(all_sex_03, all_sex_07)
underweight_sex <- bind_rows(underweight_sex_03, underweight_sex_07)

underweight_sex %<>% rename(underweight = births)

natality_sex <- full_join(all_sex, underweight_sex, by = c("FIPS", "year", "sex"))

# Create underweight bith variable
natality_sex %<>%
  mutate(
    underweight = underweight / births * 100,
    race = "total")


# Race

# Read and clean data
all_bw_03 <- read_tsv(path %p% "all_bw_03.txt")
all_bw_07 <- read_tsv(path %p% "all_bw_07.txt")
underweight_bw_03 <- read_tsv(path %p% "underweight_bw_03.txt")
underweight_bw_07 <- read_tsv(path %p% "underweight_bw_07.txt")
all_h_03 <- read_tsv(path %p% "all_h_03.txt")
all_h_07 <- read_tsv(path %p% "all_h_07.txt")
underweight_h_03 <- read_tsv(path %p% "underweight_h_03.txt")
underweight_h_07 <- read_tsv(path %p% "underweight_h_07.txt")

all_bw_03 %<>% clean_wonder()
all_bw_07 %<>% clean_wonder()
underweight_bw_03 %<>% clean_wonder()
underweight_bw_07 %<>% clean_wonder()
all_h_03 %<>% clean_wonder() %>% mutate(race = "hispanic")
all_h_07 %<>% clean_wonder() %>% mutate(race = "hispanic")
underweight_h_03 %<>% clean_wonder() %>% mutate(race = "hispanic")
underweight_h_07 %<>% clean_wonder() %>% mutate(race = "hispanic")

# Bind data frames
all_race <- bind_rows(all_bw_03, all_bw_07, all_h_03, all_h_07)
underweight_race <- bind_rows(underweight_bw_03, underweight_bw_07, underweight_h_03, underweight_h_07)

underweight_race %<>% rename(underweight = births)

natality_race <- full_join(all_race, underweight_race, by = c("FIPS", "year", "race"))

# Create underweight birth variable
natality_race %<>%
  mutate(
    underweight = underweight / births * 100,
    sex = "total")

# Bind all data frames
natality_county <- bind_rows(natality_total, natality_sex, natality_race) %>%
  select(-births) %>%
  organize()

update_sysdata(natality_county)

rm(all, all_03, all_07, all_bw_03, all_bw_07, all_h_03, all_h_07, all_race, all_sex_03, all_sex_07, all_sex,
   underweight, underweight_03, underweight_07, underweight_bw_03, underweight_bw_07, underweight_h_03,
   underweight_h_07, underweight_race, underweight_sex_03, underweight_sex_07, underweight_sex,
   natality_total, natality_race, natality_sex, path)
