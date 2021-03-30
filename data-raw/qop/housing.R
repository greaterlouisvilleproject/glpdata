library(glptools)
glp_load_packages()

path <- "data-raw/qop/housing/"

acs_micro <- feather::read_feather("data-raw/microdata/acs_micro_FIPS_repwts.feather")

acs_micro %<>%
  group_by(year, SERIAL) %>%
  mutate(hh_members = n()) %>%
  ungroup() %>%
  filter(PERNUM == 1) %>%
  mutate(
    OWNCOST  = replace(OWNCOST, OWNCOST == 99999, NA),
    HHINCOME = replace(HHINCOME, HHINCOME == 99999, NA),
    OWNERSHP = replace(OWNERSHP, OWNERSHP == 0, NA),
    RENTGRS  = replace(RENTGRS, RENTGRS == 0 & OWNERSHP == 1, NA),

    homeownership = if_else(OWNERSHP == 1, T, F),

    hcost = if_else(homeownership == 1, OWNCOST, RENTGRS),
    cost_burden = if_else(hcost * 12 / HHINCOME > 0.3, T, F),
    severe_cost_burden = if_else(hcost * 12 / HHINCOME > 0.5, 1, 0),

    hh_type = case_when(
      homeownership  & !cost_burden ~ "noncb_homeowner",
      homeownership  & cost_burden  ~ "cb_homeowner",
      !homeownership & !cost_burden ~ "noncb_renter",
      !homeownership & cost_burden  ~ "cb_renter",
      TRUE ~ NA_character_),

    KITCHEN  = replace(KITCHEN, KITCHEN == 0, NA),
    ROOMS    = replace(ROOMS, ROOMS == 0, NA),
    PLUMBING = replace(PLUMBING, PLUMBING == 0, NA),

    severe_housing_problems = if_else(
      KITCHEN == 1 | PLUMBING == 10 | hh_members / ROOMS > 1 | severe_cost_burden, T, F))

# Homeownership and Cost-burden
housing_county1  <- survey_by_demog(acs_micro, "hh_type", "HHWT")
housing_county2  <- survey_by_demog(acs_micro, "homeownership", "HHWT")
housing_county3  <- survey_by_demog(acs_micro, "cost_burden", "HHWT")
housing_county <- bind_df(housing_county1, housing_county2, housing_county3)

housing_msa1  <- survey_by_demog(acs_micro, "hh_type", "HHWT", geog = "MSA")
housing_msa2  <- survey_by_demog(acs_micro, "homeownership", "HHWT", geog = "MSA")
housing_msa3  <- survey_by_demog(acs_micro, "cost_burden", "HHWT", geog = "MSA")
housing_msa_1yr <- bind_df(housing_msa1, housing_msa2, housing_msa3)

housing_vars_05_5yr <- build_census_var_df("acs5", "B25106")

housing_map <- get_census(housing_vars_05_5yr, "tract")

housing_map %<>%
  mutate(
    homeownership_ = case_when(
      str_detect(label, "Renter") ~ F,
      str_detect(label, "Owner") ~ T,
      TRUE ~ NA),
    cost_burden_ = case_when(
      str_detect(label, "30 percent or more") ~ T,
      str_detect(label, "Zero or negative income") ~ T,
      str_detect(label, "No cash rent") ~ F,
      str_detect(label, "20 to 29 percent") ~ F,
      str_detect(label, "Less than 20 percent") ~ F,
      TRUE ~ NA),
    hh_type = case_when(
      homeownership_ & !cost_burden_  ~ "noncb_homeowner",
      homeownership_ & cost_burden_   ~ "cb_homeowner",
      !homeownership_ & !cost_burden_ ~ "noncb_renter",
      !homeownership_ & cost_burden_  ~ "cb_renter",
      TRUE ~ NA_character_))

housing_map1 <- process_census(housing_map, cat_var = "hh_type", output_name = "housing")
housing_map2 <- process_census(housing_map, cat_var = "homeownership_", output_name = "homeownership")
housing_map3 <- process_census(housing_map, cat_var = "cost_burden_", output_name = "cost_burden")

housing_map <- bind_df(housing_map1, housing_map2, housing_map3)

housing_map %>%
  process_map(cb_homeowner:cost_burden, return_name = "housing") %>%
  list2env(.GlobalEnv)

# Severe Housing Problems
housing_problems_county <- survey_by_demog(acs_micro, "severe_housing_problems", "HHWT")
housing_problems_msa    <- survey_by_demog(acs_micro, "severe_housing_problems", "HHWT", geog = "MSA")

housing_county  %<>% bind_df(housing_problems_county)
housing_msa_1yr %<>% bind_df(housing_problems_msa)

usethis::use_data(housing_county, housing_msa_1yr, housing_tract, housing_nh, housing_muw, overwrite = TRUE)

rm(acs_micro, housing_problems_county, housing_problems_msa,
   housing_county1, housing_county2, housing_county3,
   housing_msa1, housing_msa2, housing_msa3,
   housing_map1, housing_map2, housing_map3,
   housing_vars_05_5yr, housing_map, path)
