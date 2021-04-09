library(glptools)
glp_load_packages()

health_insurance_vars_1yr   <- build_census_var_df("acs1", "C27001")
health_insurance_vars_5yr   <- build_census_var_df("acs5", "B27001")

health_insurance_county <- get_census(health_insurance_vars_1yr, "FIPS")
health_insurance_map    <- get_census(health_insurance_vars_5yr, "tract")

health_insurance_county %<>%
  mutate(
    insured = case_when(
      str_detect(label, "With health") ~ TRUE,
      str_detect(label, "No health")     ~ FALSE,
      TRUE ~ NA)) %>%
  filter(age_group != "all")

health_insurance_map %<>%
  mutate(
    insured = case_when(
      str_detect(label, "With health") ~ TRUE,
      str_detect(label, "No health")     ~ FALSE,
      TRUE ~ NA)) %>%
  filter(age_group != "all")

health_insurance_county %<>% process_census(cat_var = "insured", output_name = "health_insurance")
health_insurance_map    %<>% process_census(cat_var = "insured", output_name = "health_insurance")

process_map(health_insurance_map, health_insurance, return_name = "health_insurance") %>%
  list2env(.GlobalEnv)

usethis::use_data(health_insurance_county, health_insurance_tract,
                  health_insurance_nh, health_insurance_muw, overwrite = TRUE)

rm(health_insurance_vars_1yr, health_insurance_vars_5yr, health_insurance_map)
