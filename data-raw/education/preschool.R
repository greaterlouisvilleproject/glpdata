library(glptools)
glp_load_packages()

preschool_vars_00_total <- build_census_var_df("sf3", "PCT023", age_groups = "3_4")
preschool_vars_05_1yr   <- build_census_var_df("acs1", "B14003", age_groups = "3_4")
preschool_vars_05_5yr   <- build_census_var_df("acs5", "B14003", age_groups = "3_4")

preschool_vars_1yr <- bind_rows(preschool_vars_00_total, preschool_vars_05_1yr, preschool_vars_05_5yr)
preschool_vars_5yr <- bind_rows(preschool_vars_00_total, preschool_vars_05_5yr, preschool_vars_05_5yr)

preschool_county <- get_census(preschool_vars_1yr, "FIPS")
preschool_map    <- get_census(preschool_vars_5yr, "tract")

preschool_county %<>% mutate(in_school = if_else(str_detect(label, "Not enrolled"), F, T))
preschool_map    %<>% mutate(in_school = if_else(str_detect(label, "Not enrolled"), F, T))

preschool_county %<>%
  process_census(cat_var = "in_school",
                 output_name = "preschool",
                 age_groups = "3_4")

preschool_map %<>%
  process_census(cat_var = "in_school",
                 output_name = "preschool",
                 age_groups = "3_4")

preschool_map %>%
  process_map(preschool, return_name = "preschool") %>%
  list2env(.GlobalEnv)

acs_micro <- feather::read_feather("data-raw/microdata/acs_micro_FIPS_repwts.feather")

acs_micro %<>%
  filter(age %in% 3:4) %>%
  mutate(preschool = if_else(SCHOOL == 2, T, F))

preschool_county  <- svy_race_sex(acs_micro, "preschool", "proportion")

usethis::use_data(preschool_county, preschool_tract, preschool_nh, preschool_muw, overwrite = TRUE)

rm(preschool_vars_00_total, preschool_vars_05_1yr, preschool_vars_05_5yr,
   preschool_vars_1yr, preschool_vars_5yr, preschool_map)
