library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(feather)
library(survey)

cps_micro <- read_feather("data-raw/microdata/cps_micro.feather")

cps_micro %<>%
  mutate(
    cert = if_else(PROFCERT == 2, 1, 0),
    cert = replace(cert, PROFCERT == 99, NA),
    cert_degree = if_else(cert == 1 | educ %in% c("assoc", "bach", "grad"), 1, 0),
    cert_no_degree = if_else(cert == 1 & educ %in% c("no_hs", "hs", "some_col"), 1, 0)) %>%
  select(-PROFCERT) %>%
  filter(age %in% 25:64) %>%
  filter(!duplicated(CPSIDP))

cps_svy <- svydesign(ids = ~1, weights = ~WTFINL, data = cps_micro)

certificates_msa_1yr <- svyby(~cert+cert_degree+cert_no_degree,
                          ~MSA+year, design = cps_svy, svymean, na.rm = TRUE,
                          na.rm.all = TRUE, drop.empty.groups = FALSE)

certificates_msa_1yr %<>%
  select(-se.cert, -se.cert_degree, -se.cert_no_degree) %>%
  mutate_at(vars(MSA, year), unfactor) %>%
  mutate_at(vars(cert:cert_no_degree), ~ . * 100) %>%
  mutate(sex = "total", race = "total", MSA = as.character(MSA)) %>%
  filter(year < 2019) %>%
  organize()

update_sysdata(certificates_msa_1yr)

rm(cps_micro, cps_svy)
