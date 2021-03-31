library(glptools)
glp_load_packages()

process_degree <- function(df, geog) {

  grouping_vars <- c(geog, "year", "sex", "race", "age_group")

  df %>% select(any_of(c(geog, "year", "sex", "race", "var_type", "age_group",
                         "no_hs", "hs", "some_col", "assoc", "bach", "grad")))

  df %<>% pivot_longer(cols = c("no_hs", "hs", "some_col", "assoc", "bach", "grad"),
                       names_to = "educ", values_to = "degree")

  df_assoc_plus <- df %>%
    filter(educ %in% c("assoc", "bach", "grad")) %>%
    group_by(across(grouping_vars)) %>%
    summarise(
      educ       = "assoc_plus",
      estimate   = sum(degree[var_type == "estimate"]),
      population = first(degree[var_type == "population"]),
      percent    = estimate / population * 100,
      MOE        = sqrt(sum(degree[var_type == "MOE"]^2)),
      CI         = MOE / population * 100,
      .groups = "drop") %>%
    pivot_longer(cols = estimate:CI, names_to = "var_type", values_to = "degree")

  df_bach_plus <- df %>%
    filter(educ %in% c("bach", "grad", "degree.population")) %>%
    group_by(across(grouping_vars)) %>%
    summarise(
      educ       = "bach_plus",
      estimate   = sum(degree[var_type == "estimate"]),
      population = first(degree[var_type == "population"]),
      percent    = estimate / population * 100,
      MOE        = sqrt(sum(degree[var_type == "MOE"]^2)),
      CI         = MOE / population * 100,
      .groups = "drop") %>%
    pivot_longer(cols = estimate:CI, names_to = "var_type", values_to = "degree")

  df %<>% bind_rows(df_assoc_plus) %>% bind_rows(df_bach_plus)

  df %<>%
    pivot_wider(names_from = "educ", values_from = degree)

  id_vars     <- c(geog, "year", "race", "sex", "var_type")
  degree_vars <- c("no_hs", "hs", "some_col", "assoc", "bach", "grad",
                   "assoc_plus", "bach_plus")

  df %<>%
    select_at(df %cols_in% c(id_vars, "age_group", degree_vars)) %>%
    pivot_wider(names_from = age_group, values_from = degree_vars) %>%
    rename_with(
      ~ str_remove(., "_25_64") %>%
        str_replace(., "25_34", "young")) %>%
    select_at(c(id_vars, degree_vars, paste0(degree_vars, "_young")))

  df
}

# Create variable lists and download data
degree_vars_00     <- build_census_var_df("sf3", "PCT025")
degree_vars_05_1yr <- build_census_var_df("acs1", "B15001")
degree_vars_05_5yr <- build_census_var_df("acs5", "B15001")

degree_vars_1yr <- bind_rows(degree_vars_00, degree_vars_05_1yr) %>%
  filter(age_group %in% c("25_34", "35_44", "45_64"))

degree_vars_5yr <- bind_rows(degree_vars_00, degree_vars_05_5yr) %>%
  filter(age_group %in% c("25_34", "35_44", "45_64"))

#degree_county <- get_census(degree_vars_1yr, "FIPS")
degree_map <- get_census(degree_vars_5yr, "tract")
#degree_zip <- get_census(degree_vars_5yr, "zip")

#degree_zip %<>% filter(zip %in% FIPS_zip$zip[FIPS_zip$FIPS == "21111"])

add_educ <- function(df) {
  df %>%
    mutate(
      educ = case_when(
        str_detect(label, "no diploma|Less than") ~ "no_hs",
        str_detect(label, "High school") ~ "hs",
        str_detect(label, "Some college") ~ "some_col",
        str_detect(label, "Associate") ~ "assoc",
        str_detect(label, "Bachelor") ~ "bach",
        str_detect(label, "Graduate") ~ "grad"))
}

#degree_county %<>% add_educ()
degree_map    %<>% add_educ()
#degree_zip    %<>% add_educ()

degree_map_young <- degree_map %>%
  process_census(cat_var = "educ", output_name = "degree", age_groups = "25_34") %>%
  mutate(age_group = "25_34")

degree_map_adult <- degree_map %>%
  process_census(cat_var = "educ", output_name = "degree", age_groups = "25_64") %>%
  mutate(age_group = "25_64")

degree_map <- bind_rows(degree_map_young, degree_map_adult) %>%
  process_degree("tract")

process_map(degree_map, no_hs:bach_plus_young, return_name = "degree") %>%
  list2env(.GlobalEnv)

# degree_zip_young <- degree_zip %>%
#   process_census(cat_var = "educ", output_name = "degree", age_groups = "25_34") %>%
#   mutate(age_group = "25_34")
#
# degree_zip_adult <- degree_zip %>%
#   process_census(cat_var = "educ", output_name = "degree", age_groups = "25_64") %>%
#   mutate(age_group = "25_64")
#
# degree_zip <- bind_rows(degree_zip_young, degree_zip_adult) %>% process_degree("zip")

acs_micro <- feather::read_feather("data-raw/microdata/acs_micro_FIPS_repwts.feather")

# Recode age groups
acs_micro %<>%
  filter(age %in% 25:64) %>%
  mutate(
    age_group = case_when(
      age %in% 25:34 ~ "25_34",
      age %in% 35:44 ~ "35_44",
      age %in% 45:64 ~ "45_64"))

degree_county_young <- acs_micro %>%
  filter(age_group == "25_34") %>%
  survey_by_demog("educ", other_grouping_vars = "age_group")

degree_county_adult <- acs_micro %>%
  survey_by_demog("educ") %>%
  mutate(age_group = "25_64")

degree_county <- bind_rows(degree_county_adult, degree_county_young) %>%
  process_degree("FIPS")

usethis::use_data(degree_county, degree_tract, degree_nh, degree_muw, overwrite = TRUE)

rm(process_degree, degree_county_young, degree_county_adult,
   degree_map_young, degree_map_adult, add_educ,
   degree_vars_00, degree_vars_05_5yr, degree_vars_5yr,
   degree_vars_1yr, degree_vars_05_1yr,
   degree_map, acs_micro)

