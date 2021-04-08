library(glptools)
glp_load_packages()

# County Data

acs_micro <- feather::read_feather("data-raw/microdata/acs_micro_FIPS_repwts.feather")

acs_micro %<>%
  filter(EMPSTAT == 1) %>%
  mutate(
    MSA = as.character(MSA),
    commute = replace(TRANTIME, TRANTIME == 0, NA))

commute_county  <- svy_race_sex(acs_micro, "commute", "mean")
commute_msa_1yr <- svy_race_sex(acs_micro, "commute", "mean", geog = "MSA")

# Map data
commute_cat_vars_05   <- build_census_var_df("acs5", "B08012", additional_filters = "\\d")
commute_agg_vars_05   <- build_census_var_df("acs5", "B08013")

commute_cat <- get_census(commute_cat_vars_05, "tract")
commute_agg <- get_census(commute_agg_vars_05, "tract")

commute_cat %<>%
  mutate(
    commute_length =
      str_extract(label, "(\\d* to \\d*)|(Less than 5)|(90 or more)") %>%
      str_replace(" to ", "_") %>%
      str_replace("Less than ", "under_") %>%
      str_replace(" or more", "_plus"),
    long_commute_var = if_else(commute_length %in% c("45_59", "60_89", "90_plus"), T, F),
    extreme_commute_var = if_else(commute_length %in% c("60_89", "90_plus"), T, F))

long_commute <- process_census(commute_cat, cat_var = "long_commute_var",
                               output_name = "long_commute")

extreme_commute <- process_census(commute_cat, cat_var = "extreme_commute_var",
                                  output_name = "extreme_commute")

commute_pop <- long_commute %>%
  filter(var_type == "population") %>%
  rename(value = long_commute)

commute_agg %<>%
  transmute(tract, year, sex, race,
            var_type = est_moe, value) %>%
  bind_rows(commute_pop) %>%
  pivot_wider(names_from = "var_type", values_from = "value") %>%
  mutate(
    estimate = estimate / population,
    MOE = MOE / population) %>%
  pivot_longer(
    estimate:population,
    names_to = "var_type",
    values_to = "average_commute")

commute_map <- bind_df(commute_agg, long_commute, extreme_commute)

process_map(commute_map, average_commute, long_commute, extreme_commute, return_name = "commute") %>%
  list2env(.GlobalEnv)

usethis::use_data(commute_county, commute_msa_1yr,
                  commute_tract, commute_nh, commute_muw, overwrite = TRUE)

rm(acs_micro, commute_cat_vars_05, commute_agg_vars_05, commute_cat, commute_agg,
   long_commute, extreme_commute, commute_map, commute_pop)
