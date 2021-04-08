library(glptools)
glp_load_packages()

hh_income_00 <- build_census_var_df("sf3", c("P53", "P152"))
hh_income_05_1yr <- build_census_var_df("acs1", "B19013")
hh_income_05_5yr <- build_census_var_df("acs5", "B19013")

hh_income_1yr <- bind_rows(hh_income_00, hh_income_05_1yr)
hh_income_5yr <- bind_rows(hh_income_00, hh_income_05_5yr)

hh_income_county  <- get_census(hh_income_1yr, "FIPS",  var_name = "hh_income", parallel = T)
hh_income_map     <- get_census(hh_income_5yr, "tract", var_name = "hh_income", parallel = T)

hh_income_county %<>%
  stl_merge(hh_income, method = "mean") %>%
  COLA(hh_income)

hh_income_map %<>%
  select(tract, year, sex, race, hh_income) %>%
  COLA(hh_income)

process_map(hh_income_map, hh_income, return_name = "hh_income", method = "mean") %>%
  list2env(.GlobalEnv)

usethis::use_data(hh_income_county, hh_income_tract, hh_income_nh, hh_income_muw, overwrite = TRUE)

rm(hh_income_00, hh_income_05_1yr, hh_income_05_5yr, hh_income_1yr, hh_income_5yr, hh_income_map)
