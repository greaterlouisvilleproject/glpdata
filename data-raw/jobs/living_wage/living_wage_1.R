library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(rvest)

path <- "data-raw/jobs/living_wage/"

# Read or webscrape living wage table

if ("lw_table.csv" %in% list.files(path)) {
  lw_table <- read_csv(path %p% "lw_table.csv")
} else {

  # Get living wage data from MIT
  for(f in FIPS_df$FIPS[FIPS_df$FIPS != "MERGED"]){
    webpage <- read_html("http://livingwage.mit.edu/counties/" %p% f)

    df <- webpage %>%
      html_nodes(xpath = "/html/body/div[2]/div[1]/table") %>%
      html_table()

    df <- df[[1]]

    df$FIPS <- f

    lw_table <- assign_row_join(lw_table, df)
  }

  # Reshape data frame
  lw_table %<>%
    filter(`Hourly Wages` == "Living Wage") %>%
    select(-`Hourly Wages`) %>%
    gather(-FIPS, key = "family_type", value = "living_wage") %>%
    filter(str_detect(family_type, "\\*", negate = TRUE))

  # Determine family structure and living wage
  lw_table %<>%
    mutate(
      hh_adults = str_sub(family_type, 1, 1) %>%
        as.numeric(),
      hh_children = str_extract(family_type, "..Child") %>%
        str_sub(1, 1) %>%
        replace_na(0) %>%
        as.numeric(),
      stay_at_home = if_else(str_detect(family_type, "\\("), T, F),
      living_wage = str_sub(living_wage, start = 2) %>%
        as.numeric() * 2080) %>%
    select(-family_type)

  #Adjust for inflation
  cpi_multiplier <- COLA_df$cpi_index[COLA_df$FIPS == "21111" & COLA_df$year == 2018]

  lw_table %<>% mutate(living_wage = living_wage * cpi_multiplier)

  write_csv(lw_table, path %p% "lw_table.csv")
}

# Read ACS microdata

if("acs_micro.feather" %in% list.files("data-raw/microdata")){
  acs_micro <- read_feather("data-raw/microdata/acs_micro.feather")
} else{
  acs_micro <- read_csv("data-raw/microdata/acs_micro.csv")
  acs_micro %<>% process_acs()
  write_feather(acs_micro, "data-raw/microdata/acs_micro.feather")
}

acs_micro <- read_feather(path %p% "living_wage.feather")

acs_micro %<>% mutate(FIPS = replace(FIPS, FIPS == "01073",
                              "1073"))

acs_micro %<>% mutate(
  hh_income_tot = replace(HHINCOME, HHINCOME == 9999999, NA),
  INCEARN  = replace(INCEARN, INCEARN == 0, NA),
  LABFORCE = replace(LABFORCE, LABFORCE == 0, NA)) %>%
  group_by(SERIAL, year) %>%
  mutate(
    hh_income_earned = sum(INCEARN, na.rm = TRUE),
    working = if_else(any(LABFORCE == 2 & age > 17), 1, 0)) %>%
  filter(working == 1) %>%
  ungroup()

#
#lw %<>%
#  group_by(SERIAL, FAMUNIT) %>%
#  mutate(PERNUM = row_number()) %>%
#  ungroup() %>%
#  mutate(SERIAL = paste0(SERIAL, "-", FAMUNIT))

#Calculate number of children, adults, and size of household
acs_micro %<>%
  mutate(
    adult = if_else(age > 17, 1, 0),
    child = if_else(age <= 17, 1, 0)) %>%
  group_by(SERIAL, year) %>%
  mutate(
    hh_adults = sum(adult),
    hh_children = sum(child),
    hh_size = hh_adults + hh_children) %>%
  ungroup() %>%
  arrange(FIPS, SERIAL, PERNUM)

#Determine if there is a stay at home parent
acs_micro %<>%
  group_by(SERIAL, year) %>%
  mutate(
    stay_at_home = if_else(any(age > 17 & SCHOOL == 1 & LABFORCE == 1 & NCHILD > 0), T, F)) %>%
  ungroup()

acs_micro %<>% left_join(lw_table, by = c("FIPS", "hh_adults", "hh_children", "stay_at_home"))

acs_micro %<>%
  mutate(lw = if_else(hh_income_earned > living_wage, 1, 0)) %>%
  filter(year > 2014) %>%
  filter(PERNUM == 1)

acs_svy <- svydesign(ids = ~0, weights = ~HHWT, data = acs_micro)

results <- svyby(~lw, by = ~FIPS, design = acs_svy, svymean, na.rm = TRUE)

results %<>% mutate(lw = lw*100)

png("test2.png", 3000, 2400, res = 200)
ranking(results %>% pull_peers_FIPS() %>% mutate(year = 2016), lw)
dev.off()

rankin





















