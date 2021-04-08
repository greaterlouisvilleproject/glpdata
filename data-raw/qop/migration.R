library(glptools)
glp_load_packages()

process_inmigration <- function(df) {
  df %>%
    mutate(
      inmigrated = case_when(
        str_detect(label, "Moved from") ~ T,
        str_detect(label, "Moved within|Same house") ~ F,
        TRUE ~ NA))
}
process_outmigration <- function(df) {
  df %>%
    mutate(
      outmigrated = case_when(
        str_detect(label, "Moved to") ~ T,
        str_detect(label, "Moved within|Same house") ~ F,
        TRUE ~ NA))
}
add_educ_label <- function(df) {
  df %>%
    mutate(
      educ = case_when(
        str_detect(label, "no diploma|Less than") ~ "no_hs",
        str_detect(label, "High school") ~ "hs",
        str_detect(label, "Some college") ~ "some_col_assoc",
        str_detect(label, "Bachelor") ~ "bach",
        str_detect(label, "Graduate") ~ "grad")) %>%
    filter(!is.na(educ))
}

#test <- censusapi::listCensusApis()
#test2 <- censusapi::listCensusMetadata("pep/components", 2019)

test_api <- data.frame(
  survey = "pep/components",
  year = 2019,
  variable = c("NETMIG", "DATE"),
  race = "total",
  sex = "total",
  age_group = "all",
  age_low = 0,
  age_high = Inf,
  label = "",
  table = "")

for (f in FIPS_df_two_stl$FIPS) {
  temp <- censusapi::getCensus(
    name = "pep/components",
    vintage = 2019,
    vars = c("PERIOD_CODE", "PERIOD_DESC",
             "BIRTHS", "DEATHS", "NETMIG",
             "DOMESTICMIG", "INTERNATIONALMIG",
             "NATURALINC",
             "RBIRTH", "RDEATH", "RNETMIG",
             "RDOMESTICMIG", "RINTERNATIONALMIG",
             "RNATURALINC"),
    regionin = paste0("state:", str_sub(f, 1, 2)),
    region = paste0("county:", str_sub(f, 3, 5)),
    PERIOD_CODE = paste0(1:10, collapse=","),
    key = Sys.getenv("CENSUS_API_KEY"))

  output <- assign_row_join(output, temp)
}

test <- output %>%
  mutate(across())
  mutate(
    FIPS = paste0(state, county),
    year = str_sub(PERIOD_DESC, -4))



# Total, race
inmigration_vars2  <- build_census_var_df("acs1", c("B07003", "B07004"))
outmigration_vars <- build_census_var_df("acs1", c("B07403", "B07404"))

# Age
inmigration_vars_age  <- build_census_var_df("acs1", "B07001",
                                                    age_groups = c("25_29", "30_34"))
outmigration_vars_age <- build_census_var_df("acs1", "B07401",
                                                    age_groups = c("25_29", "30_34"))

# Education
inmigration_vars_educ  <- build_census_var_df("acs1", "B07009")
outmigration_vars_educ <- build_census_var_df("acs1", "B07409")


# Get data
inmigration  <- get_census(inmigration_vars2, "FIPS", parallel = T)
outmigration <- get_census(outmigration_vars, "FIPS", parallel = T)

inmigration_age  <- get_census(inmigration_vars_age, "FIPS", parallel = T)
outmigration_age <- get_census(outmigration_vars_age, "FIPS", parallel = T)

inmigration_educ  <- get_census(inmigration_vars_educ, "FIPS", parallel = T)
outmigration_educ <- get_census(outmigration_vars_educ, "FIPS", parallel = T)


# Add info
inmigration %<>% process_inmigration()
outmigration %<>% process_outmigration()

inmigration_age %<>% process_inmigration()
outmigration_age %<>% process_outmigration()

inmigration_educ  %<>% process_inmigration() %>% add_educ_label()
outmigration_educ  %<>% process_outmigration() %>% add_educ_label()


# Process data
inmigration  %<>% process_census(cat_var = "inmigrated",  output_name = "inmigration",
                                 output_percent = FALSE, output_population = TRUE)
outmigration %<>% process_census(cat_var = "outmigrated", output_name = "outmigration",
                                 output_percent = FALSE, output_population = TRUE)

inmigration_age  %<>% process_census(cat_var = "inmigrated",  output_name = "inmigration",
                                     age_groups = c("25_29", "30_34"),
                                     output_percent = FALSE, output_population = TRUE)
outmigration_age  %<>% process_census(cat_var = "outmigrated",  output_name = "outmigration",
                                      age_groups = c("25_29", "30_34"),
                                      output_percent = FALSE, output_population = TRUE)

inmigration_educ %<>%
  group_by(educ) %>%
  nest() %>%
  mutate(data = map(data, ~ process_census(.,
                               cat_var = "inmigrated", output_name = "inmigration",
                               output_percent = FALSE, output_population = TRUE))) %>%
  unnest(cols = c(data)) %>%
  ungroup() %>%
  pivot_wider(names_from = educ,
              values_from = inmigration:inmigration_pop)

outmigration_educ %<>%
  group_by(educ) %>%
  nest() %>%
  mutate(data = map(data, ~ process_census(.,
                                           cat_var = "outmigrated", output_name = "outmigration",
                                           output_percent = FALSE, output_population = TRUE))) %>%
  unnest(cols = c(data)) %>%
  ungroup() %>%
  pivot_wider(names_from = educ,
              values_from = outmigration:outmigration_pop)

migration_county <- bind_df(inmigration,      outmigration,
                            inmigration_age,  outmigration_age,
                            inmigration_educ, outmigration_educ)

migration_county %<>%
  stl_merge(inmigration:outmigration_pop_grad, method = "sum") %>%
  transmute(
    FIPS, year, sex, race,

    # Totals
    inmigration_num = inmigration,
    inmigration = inmigration / inmigration_pop * 100,

    outmigration_num = outmigration,
    outmigration = outmigration / outmigration_pop * 100,

    # Age
    inmigration_num_young = inmigration_25_29 + inmigration_30_34,
    inmigration_young =
      inmigration_num_young /
      (inmigration_25_29_pop + inmigration_30_34_pop) * 100,

    outmigration_num_young = outmigration_25_29 + outmigration_30_34,
    outmigration_young =
      outmigration_num_young /
      (outmigration_25_29_pop + outmigration_30_34_pop) * 100,

    # Education
    inmigration_num_no_hs = inmigration_no_hs,
    inmigration_no_hs = inmigration_num_no_hs / inmigration_pop_no_hs * 100,

    inmigration_num_hs = inmigration_hs,
    inmigration_hs = inmigration_num_hs / inmigration_pop_hs * 100,

    inmigration_num_some_col_assoc = inmigration_some_col_assoc,
    inmigration_some_col_assoc = inmigration_num_some_col_assoc / inmigration_pop_some_col_assoc * 100,

    inmigration_num_bach = inmigration_bach,
    inmigration_bach = inmigration_num_bach / inmigration_pop_bach * 100,

    inmigration_num_grad = inmigration_grad,
    inmigration_grad = inmigration_num_grad / inmigration_pop_grad * 100,

    inmigration_num_some_col_plus = inmigration_num_some_col_assoc + inmigration_num_bach + inmigration_num_grad,
    inmigration_some_col_plus = inmigration_num_some_col_plus / (inmigration_pop_some_col_assoc +
                                                                 inmigration_pop_bach +
                                                                 inmigration_pop_grad) * 100,

    inmigration_num_bach_plus = inmigration_num_bach + inmigration_num_grad,
    inmigration_bach_plus = inmigration_num_bach_plus / (inmigration_pop_bach + inmigration_pop_grad) * 100,

    outmigration_num_no_hs = outmigration_no_hs,
    outmigration_no_hs = outmigration_num_no_hs / outmigration_pop_no_hs * 100,

    outmigration_num_hs = outmigration_hs,
    outmigration_hs = outmigration_num_hs / outmigration_pop_hs * 100,

    outmigration_num_some_col_assoc = outmigration_some_col_assoc,
    outmigration_some_col_assoc = outmigration_num_some_col_assoc / outmigration_pop_some_col_assoc * 100,

    outmigration_num_bach = outmigration_bach,
    outmigration_bach = outmigration_num_bach / outmigration_pop_bach * 100,

    outmigration_num_grad = outmigration_grad,
    outmigration_grad = outmigration_num_grad / outmigration_pop_grad * 100,

    outmigration_num_some_col_plus = outmigration_num_some_col_assoc + outmigration_num_bach + outmigration_num_grad,
    outmigration_some_col_plus = outmigration_num_some_col_plus / (outmigration_pop_some_col_assoc +
                                                                   outmigration_pop_bach +
                                                                   outmigration_pop_grad) * 100,

    outmigration_num_bach_plus = outmigration_num_bach + outmigration_num_grad,
    outmigration_bach_plus = outmigration_num_bach_plus / (outmigration_pop_bach + outmigration_pop_grad) * 100,

    net_migration_num =         inmigration_num -        outmigration_num,
    net_migration_num_young =   inmigration_num_young -  outmigration_num_young,
    net_migration_num_no_hs = inmigration_num_no_hs -     outmigration_num_no_hs,
    net_migration_num_hs =          inmigration_num_hs -        outmigration_num_hs,
    net_migration_num_some_col_assoc =    inmigration_num_some_col_assoc - outmigration_num_some_col_assoc,
    net_migration_num_bach =        inmigration_num_bach -      outmigration_num_bach,
    net_migration_num_grad =        inmigration_num_grad -      outmigration_num_grad,
    net_migration_num_some_col_plus =   inmigration_num_some_col_plus - outmigration_num_some_col_plus,
    net_migration_num_bach_plus =   inmigration_num_bach_plus - outmigration_num_bach_plus,

    net_migration                = inmigration -        outmigration,
    net_migration_young          = inmigration_young -  outmigration_young,
    net_migration_no_hs          = inmigration_no_hs -     outmigration_no_hs,
    net_migration_hs             = inmigration_hs -        outmigration_hs,
    net_migration_some_col_assoc = inmigration_some_col_assoc - outmigration_some_col_assoc,
    net_migration_bach           = inmigration_bach -      outmigration_bach,
    net_migration_grad           = inmigration_grad -      outmigration_grad,
    net_migration_some_col_plus      = inmigration_some_col_plus - outmigration_some_col_plus,
    net_migration_bach_plus      = inmigration_bach_plus - outmigration_bach_plus)

usethis::use_data(migration_county, overwrite = TRUE)

rm(process_inmigration, process_outmigration, add_educ_label,
   inmigration_vars, outmigration_vars,
   inmigration_vars_age, outmigration_vars_age,
   inmigration_vars_educ, outmigration_vars_educ,
   inmigration, inmigration_age, inmigration_educ,
   outmigration, outmigration_age, outmigration_educ)

