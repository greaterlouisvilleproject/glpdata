library(glptools)
glp_load_packages()

unemployment_vars_00 <- build_census_var_df("sf3", c("PCT35", "P150"), additional_filters = "In labor force:  Civilian: .*")
unemployment_vars_05_1yr <- build_census_var_df("acs1", c("B23001", "B23002"), additional_filters = "In labor force.*mployed")
unemployment_vars_05_5yr <- build_census_var_df("acs5", "B23001", additional_filters = "In labor force.*mployed")

unemployment_05_1yr <- bind_rows(unemployment_vars_00, unemployment_vars_05_1yr)
unemployment_05_5yr <- bind_rows(unemployment_vars_00, unemployment_vars_05_5yr)

unemployment_county <- get_census(unemployment_05_1yr, "FIPS", parallel = T)
unemployment_map    <- get_census(unemployment_05_5yr, "tract", parallel = T)

unemployment_county %<>%
  mutate(
    unemployed = if_else(str_detect(label, "Unemployed"), T, F),
    age_group = "all")

unemployment_map %<>%
  mutate(
    unemployed = if_else(str_detect(label, "Unemployed"), T, F),
    age_group = "all")

unemployment_county %<>% process_census(var_names = "value", cat_var = "unemployed", output_name = "unemployment")
unemployment_map    %<>% process_census(var_names = "value", cat_var = "unemployed", output_name = "unemployment")

process_map(unemployment_map, unemployment, pop = "unemployment_pop", return_name = "unemployment", method = "mean") %>%
  list2env(.GlobalEnv)

usethis::use_data(unemployment_county, unemployment_tract, unemployment_nh, unemployment_muw, overwrite = TRUE)

rm(unemployment_vars_00, unemployment_vars_05_1yr, unemployment_vars_05_5yr,
   unemployment_05_1yr, unemployment_05_5yr, unemployment_map)
