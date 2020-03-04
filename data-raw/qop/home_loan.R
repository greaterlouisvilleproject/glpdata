library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/home_loan/"

home_loan <- feather::read_feather(path %p% "home_loan.feather")

home_loan %<>%
  filter(
    single_family  == 1,
    owner_occupied == 1,
    first_lien     == 1,
    !is.na(loan_status)) %>%
  mutate(denied = if_else(loan_status == "denied", 1, 0))

home_loan_coapplicant1 <- home_loan %>%
  mutate(
    race = race_applicant,
    sex  = sex_applicant)

home_loan_coapplicant2 <- home_loan %>%
  filter(coapplicant) %>%
  mutate(
    race = race_coapplicant,
    sex  = sex_coapplicant)

home_loan_long <- bind_rows(home_loan_coapplicant1, home_loan_coapplicant2)

per_capita_adj <- function(df, ..., geog, keep_vars = T, keep_pop = F) {

  # Create list of variables from ... argument
  variables <- dplyr:::tbl_at_vars(df, vars(...))

  # Determine geography and other variables to join by
  if(missing(geog)) {
    geog <- df_type(df)
  }

  if(length(geog) > 1) {
    stop("Too many geography columns. Provide geog argument.")
  }

  join_vars <- c(geog, df %cols_in% c("year", "sex", "race"))

  # Create a clean, minimal population data frame
  tryCatch({
    pop_df <- switch(geog,
                     "MSA"   = glpdata:::population_msa_1yr,
                     "FIPS"  = glpdata:::population_county,
                     "tract" = glpdata:::population_tract,
                     "neighborhood"    = glpdata:::population_nh,
                     "muw"   = glpdata:::population_muw)
    },
    error = function(e){
      stop("Geography not MSA, FIPS, or tract")
    })

  if("year" %not_in% join_vars) pop_df %<>% filter(year == 2018)
  if("sex"  %not_in% join_vars & geog %in% c("FIPS", "MSA")) pop_df %<>% filter(sex == "total")
  if("race" %not_in% join_vars & geog %in% c("FIPS", "MSA")) pop_df %<>% filter(race == "total")

  pop_df %<>% select_at(c(join_vars, "population"))

  # Join df to population df and divide by population.
  # If keep_vars == TRUE, retain original variables.
  if (keep_vars) {
    new_df <- df %>%
      bind_df(pop_df, by = join_vars) %>%
      mutate_at(variables, ~ . / population) %>%
      rename_at(variables, ~ paste0(., "_pp")) %>%
      select_at(c(join_vars, paste0(variables, "_pp"), "population"))

    df %<>% bind_df(new_df)
  } else {
    df %<>%
      left_join(pop_df, by = join_vars) %>%
      mutate_at(variables, ~ . / population)
  }

  # If keep_pop == FALSE, remove population variable
  if (!keep_pop) df %<>% select(-population)

  df
}

process_loan <- function(df, geog, demographic = TRUE){
  output <- df %>%
    group_by_at(df %cols_in% c(geog, "year")) %>%
    {
    bind_df(
      summarise(., denial = mean(denied) * 100),
      filter(., denied == 0) %>%
      summarise(
        loan_amount = sum(loan, na.rm = TRUE),
        loans_originated = sum(loan[loan_status == ""], na.rm = TRUE),
        loan_number = n(),
        median_loan = median(loan, na.rm = TRUE)))
    } %>%
    ungroup() %>%
    mutate(race = "total", sex = "total")

  if (demographic) {
    df_sex_race <- df %>%
      group_by_at(df %cols_in% c(geog, "year", "race", "sex")) %>%
      {
        bind_df(
          summarise(., denial = mean(denied) * 100),
          filter(., denied == 0) %>%
            summarise(
              loan_amount = sum(loan, na.rm = TRUE),
              loan_number = n(),
              median_loan = median(loan, na.rm = TRUE)))
      } %>%
      ungroup()

    df_race <- df %>%
      group_by_at(df %cols_in% c(geog, "year", "race")) %>%
      {
        bind_df(
          summarise(., denial = mean(denied) * 100),
          filter(., denied == 0) %>%
            summarise(
              loan_amount = sum(loan, na.rm = TRUE),
              loan_number = n(),
              median_loan = median(loan, na.rm = TRUE)))
      } %>%
      ungroup() %>%
      mutate(sex = "total")

    df_sex <- df %>%
      group_by_at(df %cols_in% c(geog, "year", "sex")) %>%
      {
        bind_df(
          summarise(., denial = mean(denied) * 100),
          filter(., denied == 0) %>%
            summarise(
              loan_amount = sum(loan, na.rm = TRUE),
              loan_number = n(),
              median_loan = median(loan, na.rm = TRUE)))
      } %>%
      ungroup() %>%
      mutate(race = "total")

    output %<>%
      bind_rows(df_sex, df_race, df_sex_race)
  }

  output %<>%
    filter(! (is.na(race) | is.na(sex))) %>%
    organize() %>%
    COLA(loan_amount, median_loan, rpp = F)
}

home_loan_msa_1yr <- process_loan(home_loan, "MSA") %>%
  per_capita_adj(loan_amount, loan_number)

home_loan_county <- home_loan_long %>%
  pull_peers(geog = "FIPS", add_info = FALSE) %>%
  process_loan("FIPS") %>%
  {
  bind_df(
    stl_merge(., loan_amount, loan_number, method = "sum"),
    stl_merge(., median_loan, method = "mean", weight_var = "loan_number"),  #ADD MEDIAN
    stl_merge(., denial, weight_var = "loan_number"))
  } %>%
  per_capita_adj(loan_amount, loan_number)

home_loan_tract <- home_loan %>%
  filter(FIPS == 21111) %>%
  process_loan("tract", demographic = F) %>%
  select(-sex, -race) %>%
  per_capita_adj(loan_amount, loan_number)

home_loan_nh <- home_loan %>%
  filter(FIPS == 21111) %>%
  left_join(nh_tract, by = c("tract" = "GEO_ID")) %>%
  process_loan("neighborhood", FALSE) %>%
  select(-sex, -race) %>%
  per_capita_adj(loan_amount, loan_number, geog = "neighborhood")

update_sysdata(home_loan_county, home_loan_msa_1yr, home_loan_tract, home_loan_nh)

make_map(list(tract = home_loan_tract), )



if(FALSE){

 files <- list.files(path %p% "07_17")

 for(n in 1:length(files)) {
   df <- read_csv(path %p% "07_17/" %p% files[11])

   df %<>%
     rename(MSA = msamd) %>%
     pull_peers(add_info = FALSE)

   feather::write_feather(df, path %p% "feather_data/16.feather")

   rm(df)
   gc()
 }

 files <- list.files(path %p% "18")

 for(n in 1:length(files)) {
   df <- read_delim(path %p% "17_18/" %p% files[2], delim = "|")

   df %<>%
     rename(MSA = derived_msa_md) %>%
     pull_peers(add_info = FALSE)

   feather::write_feather(df, path %p% "feather_data/18.feather")

   rm(df)
   gc()
 }

 process_07_17 <- function(df) {
   df %>%
     transmute(
       MSA,
       FIPS = state_code %p% str_pad(county_code, 3, "left", 0),
       tract = paste0("1400000US", FIPS, str_remove(census_tract_number, "\\.")),
       year = as_of_year,

       coapplicant = if_else(co_applicant_sex_name == "No co-applicant", F, T),

       race_applicant =
         if_else(applicant_ethnicity_name == "Not Hispanic or Latino" &
                   applicant_race_name_1  == "White" &
                   is.na(applicant_race_name_2),
                 "white",
         if_else(applicant_ethnicity_name == "Not Hispanic or Latino" &
                   applicant_race_name_1  == "Black or African American" &
                   is.na(applicant_race_name_2),
                 "black",
         if_else(applicant_ethnicity_name == "Hispanic or Latino",
                 "hispanic",
         if_else(applicant_ethnicity_name == "Not Hispanic or Latino" &
                   applicant_race_name_1  == "Asian" &
                   is.na(applicant_race_name_2),
                 "asian",
                 NA_character_), NA_character_), NA_character_), NA_character_),

       race_coapplicant =
         if_else(co_applicant_ethnicity_name == "Not Hispanic or Latino" &
                   co_applicant_race_name_1 == "White" &
                   is.na(co_applicant_race_name_2),
                 "white",
         if_else(co_applicant_ethnicity_name == "Not Hispanic or Latino" &
                   co_applicant_race_name_1 == "Black or African American" &
                   is.na(co_applicant_race_name_2),
                 "black",
         if_else(co_applicant_ethnicity_name == "Hispanic or Latino",
                 "hispanic",
         if_else(co_applicant_ethnicity_name == "Not Hispanic or Latino" &
                   co_applicant_race_name_1 == "Asian" &
                   is.na(co_applicant_race_name_2),
                 "asian",
                 NA_character_), NA_character_), NA_character_), NA_character_),

       race =
         if_else(!coapplicant, race_applicant,
                 if_else(race_applicant == race_coapplicant, race_applicant,
                         NA_character_), NA_character_),

       sex_applicant =
         if_else(applicant_sex_name == "Male", "male",
         if_else(applicant_sex_name == "Female", "female",
                 NA_character_), NA_character_),

       sex_coapplicant =
         if_else(co_applicant_sex_name == "Male", "male",
         if_else(co_applicant_sex_name == "Female", "female",
                 NA_character_), NA_character_),

       sex =
         if_else(!coapplicant, sex_applicant,
         if_else(sex_applicant == sex_coapplicant, sex_applicant,
                 NA_character_), NA_character_),

       single_family  = if_else(property_type   == 1, 1, 0),
       owner_occupied = if_else(owner_occupancy == 1, 1, 0),
       first_lien     = if_else(lien_status == 1, 1, 0),
       loan_status =
       if_else(action_taken == 1, "approved",
       if_else(action_taken %in% c(2, 8), "accepted",
       if_else(action_taken %in% c(3, 7), "denied",
               NA_character_), NA_character_), NA_character_),

       purpose =
         if_else(loan_purpose_name == "Home purchase", "purchase",
         if_else(loan_purpose_name == "Home improvement", "improvement",
         if_else(loan_purpose_name == "Refinancing", "refinance",
                 NA_character_), NA_character_), NA_character_),

       income = applicant_income_000s * 1000,

       denial_1 = as.numeric(denial_reason_1),
       denial_2 = as.numeric(denial_reason_2),
       denial_3 = as.numeric(denial_reason_3),
       denial_4 = NA_real_,

       loan = loan_amount_000s * 1000)

 }

 process_18 <- function(df) {
   df %>%
     transmute(
       MSA,
       FIPS = county_code,
       tract = "1400000US" %p% census_tract,
       year = activity_year,

       coapplicant = if_else(co_applicant_sex == 5 & co_applicant_race_1 == 8, F, T),

       race_applicant =
         if_else(applicant_ethnicity_1 == 2 &
                 is.na(applicant_ethnicity_2) &
                  applicant_race_1 == 5 &
                  is.na(applicant_race_2),
                 "white",
         if_else(applicant_ethnicity_1 == 2 &
                 is.na(applicant_ethnicity_2) &
                 applicant_race_1 == 3 &
                 is.na(applicant_race_2),
                 "black",
         if_else(str_sub(applicant_ethnicity_1, 1, 1) == "1",
                 "hispanic",
         if_else(applicant_ethnicity_1 == 2 &
                 is.na(applicant_ethnicity_2) &
                 str_sub(applicant_race_1, 1, 1) == "2" &
                 (str_sub(applicant_race_2, 1, 1) == "2" | is.na(applicant_race_2)),
                 "asian",
                 NA_character_), NA_character_), NA_character_), NA_character_),

       race_coapplicant =
         if_else(co_applicant_ethnicity_1 == 2 &
                 is.na(co_applicant_ethnicity_2) &
                 co_applicant_race_1 == 5 &
                 is.na(co_applicant_race_2),
                 "white",
         if_else(co_applicant_ethnicity_1 == 2 &
                 is.na(co_applicant_ethnicity_2) &
                 co_applicant_race_1 == 3 &
                 is.na(co_applicant_race_2),
                 "black",
         if_else(str_sub(co_applicant_ethnicity_1, 1, 1) == "1",
                 "hispanic",
         if_else(co_applicant_ethnicity_1 == 2 &
                 is.na(co_applicant_ethnicity_2) &
                 str_sub(co_applicant_race_1, 1, 1) == "2" &
                 (str_sub(co_applicant_race_2, 1, 1) == "2" | is.na(co_applicant_race_2)),
                 "asian",
                 NA_character_), NA_character_), NA_character_), NA_character_),

       race =
         if_else(derived_ethnicity == "Not Hispanic or Latino" & derived_race == "White", "white",
         if_else(derived_ethnicity == "Not Hispanic or Latino" & derived_race == "Black or African American", "black",
         if_else(derived_ethnicity == "Hispanic or Latino", "hispanic",
         if_else(derived_ethnicity == "Not Hispanic or Latino" & derived_race == "Asian", "asian",
                 NA_character_), NA_character_), NA_character_), NA_character_),


       sex_applicant =
         if_else(applicant_sex == 1, "male",
         if_else(applicant_sex == 2, "female",
                 NA_character_), NA_character_),

       sex_coapplicant =
         if_else(co_applicant_sex == 1, "male",
         if_else(co_applicant_sex == 2, "female",
                 NA_character_), NA_character_),

       sex =
         if_else(derived_sex == "Male", "male",
         if_else(derived_sex == "Female", "female",
                 NA_character_), NA_character_),

       single_family  = if_else(derived_dwelling_category == "Single Family (1-4 Units):Site-Built", 1, 0),
       owner_occupied = if_else(occupancy_type == 1, 1, 0),
       first_lien     = if_else(lien_status == 1, 1, 0),
       loan_status =
         if_else(action_taken == 1, "approved",
                 if_else(action_taken %in% c(2, 8), "accepted",
                         if_else(action_taken %in% c(3, 7), "denied",
                                 NA_character_), NA_character_), NA_character_),

       purpose =
         if_else(loan_purpose == 1, "purchase",
                 if_else(loan_purpose == 2, "improvement",
                         if_else(loan_purpose %in% 31:32, "refinance",
                                 NA_character_), NA_character_), NA_character_),

       income = income * 1000,

       denial_1 = as.numeric(denial_reason_1),
       denial_2 = as.numeric(denial_reason_2),
       denial_3 = as.numeric(denial_reason_3),
       denial_4 = as.numeric(denial_reason_4),

       loan = loan_amount)
 }

 files <- list.files(path %p% "feather_data/")
 for(n in 1:12){
   print(n)
   df <- feather::read_feather(path %p% "feather_data/" %p% files[n])

   if (n <= 11) df %<>% process_07_17() else df %<>% process_18()

   home_loan <- assign_row_join(home_loan, df)
 }
 feather::write_feather(home_loan, path %p% "home_loan.feather")
}

