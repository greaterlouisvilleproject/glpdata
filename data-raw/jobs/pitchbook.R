library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/jobs/pitchbook/"

pop_df <- glpdata:::population_msa_1yr %>%
  filter(sex == "total", race == "total", year == 2018) %>%
  select(MSA, population)

fortune <- readxl::read_excel(path %p% "PitchBook_1000.xlsx", skip = 7)

fortune %<>%
  transmute(
    name = `Company Name`,
    location = `HQ Location`,
    zip = `HQ Post Code`) %>%
  left_join(MSA_zip, by = "zip") %>%
  group_by(MSA) %>%
  summarise(fortune_1000 = n()) %>%
  per_capita_adj(fortune_1000, geog = "MSA", keep_pop = T) %>%
  pull_peers() %>%
  filter(current == 1) %>%
  filter(MSA != "31140") %>%
  summarise(m = mean(fortune_1000_pp)) %>%
  pull(m) * 1304162

VC <- readxl::read_excel(path %p% "PitchBook_VC.xlsx", skip = 7,
                         col_types = c("text", "text", "text", "text", "date",
                                       "numeric", "text", "text"))

VC %<>%
  transmute(
    name = `Company Name`,
    zip = `Company Post Code`,
    year = `Deal Date` %>% as.character() %>% str_sub(1, 4) %>% as.numeric(),
    amount = `Deal Size`)

process_pitchbook <- function (df) {

  geog <- df_type(df)

  df %>%
    filter(!is.na(!!geog)) %>%
    group_by(!!geog, year) %>%
    summarise(
      deals = n(),
      deals_amount = n() - sum(is.na(amount)),              # includes deals with only listed amount
      total_dollars = sum(amount, na.rm = TRUE) * 1000000,
      avg_deal = total_dollars / deals_amount,
      median_deal = median(amount, na.rm = TRUE) * 1000000) %>%
    ungroup() %>%
    COLA(total_dollars:median_deal, rpp = F) %>%
    per_capita_adj(deals, total_dollars, keep_vars = T) %>%
    mutate(
      deals_per_100000 = deals_pp * 100000) %>%
    transmute(
      MSA, year,
      sex = "total", race = "total",
      deals, deals_per_100000,
      total_dollars, total_dollars_pp,
      avg_deal, median_deal)


}

pitchbook_msa_1yr <- VC  %>%
  left_join(MSA_zip, by = "zip") %>%
  group_by(MSA, year) %>%
  summarise(
    deals = n(),
    deals_amount = n() - sum(is.na(amount)),              # includes deals with only listed amount
    total_dollars = sum(amount, na.rm = TRUE) * 1000000,
    avg_deal = total_dollars / deals_amount,
    median_deal = median(amount, na.rm = TRUE) * 1000000) %>%
  ungroup() %>%
  filter(!is.na(MSA)) %>%
  COLA(total_dollars:median_deal, rpp = F) %>%
  per_capita_adj(deals, total_dollars, keep_vars = T) %>%
  mutate(
    deals_per_100000 = deals_pp * 100000) %>%
  transmute(
    MSA, year,
    sex = "total", race = "total",
    deals, deals_per_100000,
    total_dollars, total_dollars_pp,
    avg_deal, median_deal)

pitchbook_zip <- VC %>%
  filter(zip %in% FIPS_zip$zip[FIPS_zip$FIPS == "21111"]) %>%
  group_by(zip, year) %>%


pitchbook_msa_1yr <- VC

update_sysdata(pitchbook_msa_1yr)
