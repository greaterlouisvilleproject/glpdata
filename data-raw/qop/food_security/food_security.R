library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/food_security/"

cps_micro <- feather::read_feather("data-raw/microdata/cps_micro.feather")

cps_micro

process_fa <- function(df, geog) {
  df %>%
    group_by_at(vars(!!geog)) %>%
    summarise(
      low_food_access.total    = sum(LAPOP1_10) / sum(POP2010),
      low_food_access.white    = sum(lawhite1[Urban == 1], lawhite10[Urban == 0]) / sum(TractWhite),
      low_food_access.black    = sum(lablack1[Urban == 1], lablack10[Urban == 0]) / sum(TractBlack),
      low_food_access.hispanic = sum(lahisp1[Urban == 1],  lahisp10[Urban == 0])  / sum(TractHispanic)) %>%
    gather(-!!geog, key = "variable", value = "value") %>%
    separate(variable, c("variable", "race"), "\\.") %>%
    mutate(sex = "total") %>%
    spread(key = variable, value = value) %>%
    mutate(low_food_access = low_food_access * 100)
}

food_access <- readxl::read_excel(path %p% "Food Access Atlas.xlsx",
                                    sheet = "Food Access Research Atlas")

food_access <- readxl::read_excel(path %p% "Food Environment Atlas.xls",
                                  sheet = "ACCESS")

food_access %<>%
  mutate(FIPS = str_sub(CensusTract, 1, 5)) %>%
  pull_peers(geography = "MSA", add_info = FALSE) %>%
  left_join(MSA_FIPS, by = "FIPS")

food_access_county <- food_access %>%
  pull_peers(geog = "FIPS") %>%
  mutate(FIPS = replace(FIPS, FIPS %in% c("29189", "29510"), "MERGED")) %>%
  process_fa("FIPS")

food_access_msa    <- food_access %>% process_fa("MSA")

food_access_map <- food_access %>%
  filter(FIPS == 21111) %>%
  transmute(
    tract = "1400000US" %p% CensusTract,
    LAPOP1_10,
    POP2010)

food_access_tract <- food_access_map %>%
  transmute(
    tract,
    low_food_access = LAPOP1_10 / POP2010 * 100)

food_access_nh <- food_access_map %>%
  left_join(nh_tract, by = c("tract" = "GEO_ID")) %>%
  group_by(neighborhood) %>%
  summarise(low_food_access = sum(LAPOP1_10) / sum(POP2010) * 100)

