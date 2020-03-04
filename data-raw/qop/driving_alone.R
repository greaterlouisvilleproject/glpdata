library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

acs_micro <- feather::read_feather("data-raw/microdata/acs_micro.feather")

acs_micro %<>%
  filter(TRANWORK != 0) %>%
  mutate(
    MSA = as.character(MSA),
    driving_alone = if_else(CARPOOL == 1, 1, 0))

driving_alone_county  <- svy_race_sex(acs_micro, "driving_alone")
driving_alone_msa_1yr <- svy_race_sex(acs_micro, "driving_alone", geog = "MSA")

driving_alone_county  %<>% mutate(driving_alone = driving_alone * 100)
driving_alone_msa_1yr %<>% mutate(driving_alone = driving_alone * 100)

update_sysdata(driving_alone_county, driving_alone_msa_1yr)

rm(acs_micro)
