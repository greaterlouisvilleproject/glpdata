library(glptools)
glp_load_packages()

acs_micro <- feather::read_feather("data-raw/microdata/acs_micro_repwts.feather")

acs_micro %<>%
  filter(age >= 16 & age <= 24) %>%
  mutate(
    MSA = as.character(MSA),
    not_employed = if_else(EMPSTAT == 2 | EMPSTAT == 3, 1, 0),
    not_employed = replace(not_employed, EMPSTAT == 0, NA),

    not_in_school = if_else(SCHOOL == 1, 1, 0),
    not_in_school = replace(not_in_school, SCHOOL == 0, NA),

    disconnected = if_else(not_employed & not_in_school, T, F),
    disconnected = replace(disconnected,
                           is.na(not_employed) | is.na(not_in_school),
                           NA))

disconnected_county  <- survey_by_demog(acs_micro, "disconnected")
disconnected_msa_1yr <- survey_by_demog(acs_micro, "disconnected", geog = "MSA")

usethis::use_data(disconnected_county, disconnected_msa_1yr, overwrite = TRUE)

rm(acs_micro)
