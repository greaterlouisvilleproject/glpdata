library(glptools)
glp_load_packages()

cps_micro <- feather::read_feather("data-raw/microdata/cps_micro.feather")

cps_micro %<>%
  filter(
    age %in% 25:64,
    !duplicated(CPSIDP)) %>%
  mutate(
    certificate = if_else(PROFCERT == 2, 1, 0),
    certificate = if_else(PROFCERT == 99, NA_real_, certificate),
    certificate_degree = if_else(certificate == 1 | educ %in% c("assoc", "bach", "grad"), 1, 0),
    certificate_no_degree = if_else(certificate == 1 & educ %in% c("no_hs", "hs", "some_col"), 1, 0))

cps_svy <- survey::svydesign(ids = ~1, weights = ~WTFINL, data = cps_micro)

certificate_msa_1yr <-
  survey::svyby(~certificate+certificate_degree+certificate_no_degree,
                ~MSA+year, design = cps_svy, survey::svymean,
                na.rm = TRUE, na.rm.all = TRUE, drop.empty.groups = FALSE)

certificate_msa_1yr %<>%
  select(-se.certificate, -se.certificate_degree, -se.certificate_no_degree) %>%
  mutate(
    MSA = as.character(MSA),
    year = as.numeric(as.character(year))) %>%
  mutate_at(vars(certificate:certificate_no_degree), ~ . * 100) %>%
  mutate(sex = "total", race = "total", MSA = as.character(MSA)) %>%
  filter(year > 2014, year < 2020) %>%
  organize()

usethis::use_data(certificate_msa_1yr, overwrite = TRUE)

rm(cps_micro, cps_svy)
