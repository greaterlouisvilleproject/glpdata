library(glptools)
glp_load_packages()

# County and MSA
acs_micro <- feather::read_feather("data-raw/microdata/acs_micro_FIPS_repwts.feather")

acs_micro %<>%
  filter(TRANWORK != 0) %>%
  mutate(driving_alone = if_else(CARPOOL == 1, T, F))

driving_alone_county  <- svy_race_sex(acs_micro, "driving_alone", "proportion")
driving_alone_msa_1yr <- svy_race_sex(acs_micro, "driving_alone", "proportion", geog = "MSA")

# Maps
driving_alone_vars_00   <- build_census_var_df("sf3", c("P030", "PCT065"), additional_filters = "Drove alone|16")
driving_alone_vars_05   <- build_census_var_df("acs5", c("B08301", "B08105"), additional_filters = "alone|Total.?$")

driving_alone_vars <- bind_rows(driving_alone_vars_00, driving_alone_vars_05)

driving_alone_map <- get_census(filter(driving_alone_vars, year > 2000), "tract")

driving_alone_map %<>% mutate(drove_alone = if_else(str_detect(label, "rove alone"), T, F))

driving_alone_map %<>% process_census(cat_var = "drove_alone", output_name = "driving_alone")

process_map(driving_alone_map, "driving_alone", return_name = "driving_alone") %>%
  list2env(.GlobalEnv)

usethis::use_data(driving_alone_county, driving_alone_msa_1yr, driving_alone_tract,
                  driving_alone_nh, driving_alone_muw, overwrite = TRUE)

rm(acs_micro, driving_alone_vars_00, driving_alone_vars_05, driving_alone_vars, driving_alone_map)
