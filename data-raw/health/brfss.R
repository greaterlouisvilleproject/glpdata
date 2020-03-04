library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(feather)
library(survey)

path <- "data-raw/health/brfss/"

brfss_micro <- read_feather("data-raw/microdata/brfss_micro.feather")

function(df, var, age_var = "age", pop_var = "population"){

  df$var <- df[[var]]
  df$age_var <- df[[age_var]]
  df$pop_var <- df[[pop_var]]

  grouping_vars <- c("FIPS", "year", "race", "sex")

  # Label age groups
  if(age_var == "age"){
    age_groups <-
      data.frame(
        age_var = 0:100,
        age_group = c(
          0,
          rep(1, 4),
          rep(2, 10),
          rep(3, 10),
          rep(4, 10),
          rep(5, 10),
          rep(6, 10),
          rep(7, 10),
          rep(8, 10),
          rep(9, 10),
          rep(10, 16)))

    df <- df %>%
      left_join(age_groups, by = c("age_var"))
  } else if(age_var == "age_10"){
    df$age_group <- df$age_var
  }

  # Summarise variable and population variable by age groups
  df <- df %>%
    group_by_at(df %cols_in% c("age_group", grouping_vars)) %>%
    summarise(
      var = sum(var, na.rm = TRUE),
      pop_var = sum(pop_var, na.rm = TRUE)) %>%
    ungroup()

  # Standard population from the CDC: https://wonder.cdc.gov/wonder/help/ucd.html#2000%20Standard%20Population
  std_pop <-
    data.frame(
      age_group = 0:10,
      weight = c(
        0.013818,
        0.055317,
        0.145565,
        0.138646,
        0.135573,
        0.162613,
        0.134834,
        0.087247,
        0.066037,
        0.044842,
        0.015508))

  # Subset standardized population to only ages present in the sample,
  #   standardize to total 1, and join to data frame
  std_pop <- std_pop %>%
    filter(age_group %in% df$age_group) %>%
    mutate(weight = weight / sum(weight))

  df <- df %>%
    left_join(std_pop, by = "age_group") %>%
    group_by_at(df %cols_in% grouping_vars) %>%
    summarise(rate = sum(var / pop_var * weight) * 100000) %>%
    ungroup()

  df[[var]] <- df$rate

  df <- df %>% select(-rate)

  df
}

# Recode data
brfss_micro %<>%
  filter(year > 2002) %>%
  mutate(
    poor_or_fair = recode(genhlth, 0, 0, 0, 1, 1, .default = NA_real_),

    physdays = replace(physdays, physdays == 88, 0),
    physdays = replace(physdays, physdays %in% c(77, 99), NA),

    mentdays = replace(mentdays, mentdays == 88, 0),
    mentdays = replace(mentdays, mentdays %in% c(77, 99), NA),

    diabetes = replace(diabetes, diabetes %in% 2:4, 0) %>%
      replace(. %in% 7:9, NA),

    asthma = replace(asthma, asthma %in% 1:2, 1) %>%
      replace(. == 9, NA),

    pcp = recode(pcp, `1` = 1, `2` = 1, `3` = 0, .default = NA_real_))


age_groups <-
  data.frame(
    age_var = 0:100,
    age_group = c(
      0,
      rep(1, 4),
      rep(2, 10),
      rep(3, 10),
      rep(4, 10),
      rep(5, 10),
      rep(6, 10),
      rep(7, 10),
      rep(8, 10),
      rep(9, 10),
      rep(10, 16)))

age_groups <-
  data.frame(
    age_var = 18:99,
    age_group = c(
      rep(1, length(18:44)),
      rep(2, length(45:64)),
      rep(3, length(65:99))))

std_pop <-
  data.frame(
    age_group = 1:3,
    weight = c(
      0.530534557,
      0.299194019,
      0.170271424))

brfss_micro %<>% left_join(age_groups, by = c("age" = "age_var"))

std_pop <-
  data.frame(
    age_group = 0:10,
    weight = c(
      0.013818,
      0.055317,
      0.145565,
      0.138646,
      0.135573,
      0.162613,
      0.134834,
      0.087247,
      0.066037,
      0.044842,
      0.015508))

std_pop <- std_pop %>%
  filter(age_group %in% brfss_micro$age_group) %>%
  mutate(weight = weight / sum(weight))

brfss_prevelance <- brfss_micro %>%
  group_by(MSA, year, age_group) %>%
  summarise(age_pop = sum(wgt)) %>%
  ungroup() %>%
  group_by(MSA, year) %>%
  transmute(
    age_group,
    group_pct = age_pop / sum(age_pop, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(std_pop, by = "age_group") %>%
  transmute(
    MSA, year, age_group,
    wgt_adjustment = weight / group_pct)


test <- brfss_micro %>%
  group_by(MSA, year) %>%
  left_join(brfss_prevelance, by = c("MSA", "year", "age_group")) %>%
  mutate(wgt2 = wgt * wgt_adjustment) %>%
  summarise(
    pcp_raw = weighted.mean(pcp, wgt, na.rm = TRUE),
    pcp_age = weighted.mean(pcp, wgt2, na.rm = TRUE)) %>%
  ungroup()

test %<>%
  left_join(std_pop, by = "age_group") %>%
  group_by(MSA, year) %>%
  summarise(pcp = sum(pcp * weight)) %>%
  ungroup()




brfss_msa_1yr <- bind_df(
  svy_race_sex(brfss_micro, "poor_or_fair", "wgt", "MSA"),
  svy_race_sex(brfss_micro, "physdays",     "wgt", "MSA"),
  svy_race_sex(brfss_micro, "mentdays",     "wgt", "MSA"),
  svy_race_sex(brfss_micro, "diabetes",     "wgt", "MSA"),
  svy_race_sex(brfss_micro, "asthma",       "wgt", "MSA"),
  svy_race_sex(brfss_micro, "pcp",          "wgt", "MSA"))

brfss_msa_1yr %<>%
  mutate_at(vars(poor_or_fair, diabetes, asthma, pcp), ~ . * 100) %>%
  organize() %>%
  mutate(no_pcp = 100 - pcp)

# The Greensboro MSA does not have enough results to be included after 2014.
# Model data based off CHR Greensboro data and the Greensboro AHEC
# (contains Greensboro MSA and 5 other counties)
# Also need to model physical and mentally healthy days

greensboro_chr_14 <- read_csv(path %p% "2014.csv")
greensboro_chr_15 <- read_csv(path %p% "2015.csv")
greensboro_chr_16 <- read_csv(path %p% "2016.csv")

greensboro_chr_14 %<>%
  filter(
    STATECODE == "37",
    COUNTYCODE %in% c("081", "151", "157")) %>%
  transmute(
    FIPS             = paste0(STATECODE, COUNTYCODE),
    year             = 2014,
    poor_or_fair     = `Poor or fair health Value` * 100,
    physdays         = `Poor physical health days Value`,
    mentdays         = `Poor mental health days Value`,
    diabetes         = `Diabetes Value` * 100)

greensboro_chr_15 %<>%
  filter(`5-Digit FIPS Code` %in% c("37081", "37151", "37157")) %>%
  transmute(
    FIPS             = `5-Digit FIPS Code`,
    year             = 2015,
    poor_or_fair     = as.numeric(`Poor or fair health Value`) * 100,
    physdays         = as.numeric(`Poor physical health days Value`),
    mentdays         = as.numeric(`Poor mental health days Value`),
    diabetes         = as.numeric(`Diabetes Value`) * 100)

greensboro_chr_16 %<>%
  filter(`5-digit FIPS Code` %in% c("37081", "37151", "37157")) %>%
  transmute(
    FIPS             = `5-digit FIPS Code`,
    year             = 2016,
    poor_or_fair     = as.numeric(`Poor or fair health raw value`) * 100,
    physdays         = as.numeric(`Poor physical health days raw value`),
    mentdays         = as.numeric(`Poor mental health days raw value`),
    diabetes         = as.numeric(`Diabetes prevalence raw value`) * 100)

greensboro_chr_17 <- greensboro_chr_16 %>%
  mutate(year = 2017)

greensboro_chr <- bind_rows(greensboro_chr_14, greensboro_chr_15, greensboro_chr_16, greensboro_chr_17) %>%
  mutate(race = "total", sex = "total") %>%
  left_join(glpdata:::population_msa_counties, by = c("FIPS", "year", "sex", "race")) %>%
  select(-race, -sex)

greensboro_chr %<>%
  select(-FIPS) %>%
  group_by(year) %>%
  summarise_all(~weighted.mean(., population)) %>%
  select(-population) %>%
  filter(year > 2014) %>%
  mutate(MSA = 24660, sex = "total", race = "total")

brfss_msa_1yr %<>%
  filter(!(MSA == 24660 & sex == "total" & race == "total" & year >= 2015)) %>%
  bind_rows(greensboro_chr) %>%
  organize()

update_sysdata(brfss_msa_1yr)

rm(brfss_micro, greensboro_chr_14, greensboro_chr_15,
   greensboro_chr_16, greensboro_chr_17, greensboro_chr, path)
