library(glptools)
glp_load_packages()

process_loan <- function(df, geog, demographic = TRUE){

  process_fxn <- function(df) {

    denial <- df %>%
      summarise(., denial = weighted.mean(denied, weight) * 100,
                .groups = "drop")

    approved <- df %>%
      filter(denied == 0) %>%
      summarise(
        approved_amount = sum(loan * weight, na.rm = TRUE),
        rate = weighted.mean(rate, weight, na.rm = TRUE),
        rate_spread = weighted.mean(rate_spread, weight, na.rm = TRUE),
        .groups = "drop")

    originated <- df %>%
      filter(denied == 0, loan_status == "approved") %>%
      summarise(
        loan_amount = sum(loan * weight, na.rm = TRUE),
        loan_number = n(),
        median_loan = as.numeric(Hmisc::wtd.quantile(loan, weight, probs = 0.5, na.rm = TRUE)),
        .groups = "drop")

    output <- bind_df(denial, approved, originated)

    output
  }

  output <- df %>%
    group_by_at(df %cols_in% c(geog, "year")) %>%
    process_fxn() %>%
    mutate(race = "total", sex = "total")

  if (demographic) {

    df_sex_race <- df %>%
      group_by_at(df %cols_in% c(geog, "year", "race", "sex")) %>%
      process_fxn()

    df_race <- df %>%
      group_by_at(df %cols_in% c(geog, "year", "race")) %>%
      process_fxn() %>%
      mutate(sex = "total")

    df_sex <- df %>%
      group_by_at(df %cols_in% c(geog, "year", "sex")) %>%
      process_fxn() %>%
      mutate(race = "total")

    output %<>%
      bind_rows(df_sex, df_race, df_sex_race)
  }

  output %<>%
    filter(! (is.na(race) | is.na(sex))) %>%
    organize()
}

path <- "data-raw/qop/home_loan/"

home_loan <- feather::read_feather(path %p% "home_loan.feather")

# Subset to owner-occupied, single family, first lien mortgages. Add denial variable.
home_loan %<>%
  filter(
    single_family  == 1,
    owner_occupied == 1,
    first_lien     == 1,
    !is.na(loan_status),
    purpose == "purchase") %>%
  mutate(denied = if_else(loan_status == "denied", 1, 0))

home_loan %<>%
  mutate(
    rate = if_else(rate >=18, NA_real_, rate),
    rate_spread = if_else(rate_spread < -10 | rate_spread >= 20,
                          NA_real_, rate_spread))
# test <- home_loan %>%
#   filter(FIPS == "21111") %>%
#   mutate(small_loan = if_else(loan < 70000, "small", "big")) %>%
#   group_by(year, small_loan) %>%
#   summarise(n = n()) %>%
#   pivot_wider(names_from = small_loan, values_from = n) %>%
#   mutate(pct = small / (big+small) * 100)

test <- home_loan %>% filter(FIPS == "21111")

home_loan_detail <- home_loan %>%
  filter(FIPS == "21111") %>%
  mutate(race = applicant_race,
         sex = applicant_sex)

save(home_loan_detail, file = "../Projects/housing_data/home_loans_detail.RData")

# Create data frame with weight column, where co-applicants are weighted as 0.5 and spread across two rows.
home_loan_long <- home_loan %>%
  mutate(weight = if_else(coapplicant, 0.5, 1))

home_loan_long_applicants <- home_loan_long %>%
  filter(!coapplicant) %>%
  rename(
    sex = applicant_sex,
    race = applicant_race)

home_loan_long_coapplicants1 <- home_loan_long %>%
  filter(coapplicant) %>%
  rename(
    sex = applicant_sex,
    race = applicant_race)

home_loan_long_coapplicants2 <- home_loan_long %>%
  filter(coapplicant) %>%
  rename(
    sex = co_applicant_sex,
    race = co_applicant_race)

home_loan_long <- bind_rows(home_loan_long_applicants, home_loan_long_coapplicants1, home_loan_long_coapplicants2)

# Create data at the MSA and county levels
home_loan_msa_1yr <- home_loan_long %>%
  process_loan("MSA") %>%
  filter(race %in% c("total", "white", "black", "hispanic")) %>%
  COLA(approved_amount, loan_amount, median_loan, rpp = F) %>%
  per_capita_adj(approved_amount, loan_amount, loan_number) %>%
  mutate(loan_number_per_100 = loan_number_pp * 100) %>%
  select(-loan_number_pp)

home_loan_county <- home_loan_long %>%
  pull_peers(add_info = FALSE, geog = "FIPS") %>%
  process_loan(geog = "FIPS") %>%
  filter(race %in% c("total", "white", "black", "hispanic")) %>%
  {
    bind_df(
      stl_merge(., approved_amount, loan_amount, loan_number, method = "sum"),
      stl_merge(., median_loan, rate, rate_spread, method = "mean", weight_var = "loan_number"),
      stl_merge(., denial, weight_var = "loan_number"))
  } %>%
  COLA(approved_amount, loan_amount, median_loan, rpp = F) %>%
  per_capita_adj(approved_amount, loan_amount, loan_number) %>%
  mutate(loan_number_per_100 = loan_number_pp * 100) %>%
  select(-loan_number_pp)

# Create map data frame
home_loan_map <- home_loan_long %>%
  filter(FIPS == "21111") %>%
  mutate(tract = str_extract(tract, "21111.*")) %>%
  process_loan("tract", demographic = F) %>%
  complete_vector_arg(c("tract", "year", "sex", "race"), years = c("2007:2011", "2012:2019")) %>%
  COLA(approved_amount, loan_amount, median_loan, rpp = F) %>%
  mutate(across(c(approved_amount, loan_amount, loan_number), ~ replace_na(., 0))) %>%
  per_capita_adj(approved_amount, loan_amount, loan_number) %>%
  mutate(loan_number_per_100 = loan_number_pp * 100) %>%
  select(-loan_number_pp)

home_loan_map %>% process_map(denial, pop = loan_number,
                              return_name = "denial", method = "mean") %>%
  list2env(.GlobalEnv)

home_loan_map %>% process_map(median_loan, pop = loan_number,
                              return_name = "median", method = "mean") %>%
  list2env(.GlobalEnv)

home_loan_map %>% process_map(approved_amount, loan_amount, loan_number,
                              return_name = "amounts", method = "sum") %>%
  list2env(.GlobalEnv)

home_loan_map %>% process_map(approved_amount_pp, loan_amount_pp, loan_number_per_100,
                              return_name = "amounts_pp", method = "mean") %>%
  list2env(.GlobalEnv)

home_loan_map %>% process_map(rate, rate_spread,
                              return_name = "rates", method = "mean") %>%
  list2env(.GlobalEnv)

home_loan_tract <- bind_df(denial_tract, median_tract, amounts_tract, amounts_pp_tract, rates_tract)
home_loan_nh    <- bind_df(denial_nh,    median_nh,    amounts_nh,    amounts_pp_nh,    rates_nh)
home_loan_muw   <- bind_df(denial_muw,   median_muw,   amounts_muw,   amounts_pp_muw,   rates_muw)

usethis::use_data(home_loan_county, home_loan_msa_1yr, home_loan_tract, home_loan_nh, home_loan_muw, overwrite = TRUE)

rm(path, process_loan, home_loan_map, home_loan, home_loan_long,
   home_loan_long_applicants, home_loan_long_coapplicants1, home_loan_long_coapplicants2,
   denial_tract, median_tract, amounts_tract, amounts_pp_tract,
   denial_nh,    median_nh,    amounts_nh,    amounts_pp_nh,
   denial_muw,   median_muw,   amounts_muw,   amounts_pp_muw,
   rates_tract, rates_nh, rates_muw)


# Read data and recode variables
if(FALSE){
  source(path %p% "urban_race_functions/get_hmda_race.r")
  source(path %p% "urban_race_functions/get_hmda_race_pre2018.r")

  process_07_17 <- function(df) {
    df %>%
      get_hmda_race_07_17() %>%
      transmute(
        MSA,
        FIPS = state_code %p% str_pad(county_code, 3, "left", 0),
        tract = paste0(FIPS, str_remove(census_tract_number, "\\.")),
        year = as_of_year,

        coapplicant = if_else(co_applicant_sex_name == "No co-applicant", F, T),

        applicant_race,
        co_applicant_race,

        applicant_sex =
          if_else(applicant_sex_name == "Male", "male",
                  if_else(applicant_sex_name == "Female", "female",
                          NA_character_), NA_character_),

        co_applicant_sex =
          if_else(co_applicant_sex_name == "Male", "male",
                  if_else(co_applicant_sex_name == "Female", "female",
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

        rate_spread = as.numeric(rate_spread),

        loan = loan_amount_000s * 1000)
  }
  process_18 <- function(df) {
    df %>%
      get_hmda_race_18() %>%
      transmute(
        MSA,
        FIPS = county_code,
        tract = "1400000US" %p% census_tract,
        year = activity_year,

        coapplicant = if_else(co_applicant_sex == 5 & co_applicant_race_1 == 8, F, T),

        applicant_race,
        co_applicant_race,

        applicant_sex =
          if_else(applicant_sex == 1, "male",
                  if_else(applicant_sex == 2, "female",
                          NA_character_), NA_character_),

        co_applicant_sex =
          if_else(co_applicant_sex == 1, "male",
                  if_else(co_applicant_sex == 2, "female",
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

        rate = as.numeric(interest_rate),
        rate_spread = as.numeric(rate_spread),

        loan = loan_amount)
  }

  path <- "data-raw/qop/home_loan/"

 # Read CSV files and write to feather for faster processing and debugging
  files <- list.files(path %p% "07_17")

  for(n in 1:length(files)) {
    df <- read_csv(path %p% "07_17/" %p% files[n])

    df %<>%
      rename(MSA = msamd) %>%
      pull_peers(add_info = FALSE)

    file_path <- paste0(path, "feather_data/",
                        str_pad(n+6, 2, "left", "0"),
                        ".feather")

    feather::write_feather(df, file_path)

    rm(df)
    gc()
  }

 files <- list.files(path %p% "18")

 for(n in 1:length(files)) {
   df <- read_delim(path %p% "18/" %p% files[n], delim = "|")

   df %<>%
     rename(MSA = derived_msa_md) %>%
     pull_peers(add_info = FALSE)

   file_path <- paste0(path, "feather_data/",
                       n+17,
                       ".feather")

   feather::write_feather(df, file_path)

   rm(df)
   gc()
 }

 # Process feather files
 files <- list.files(path %p% "feather_data/")
 for(n in 1:length(files)){
   print(n)
   df <- feather::read_feather(path %p% "feather_data/" %p% files[n])

   if (n <= 11) df %<>% process_07_17() else df %<>% process_18()

   home_loan <- assign_row_join(home_loan, df)
 }
 feather::write_feather(home_loan, path %p% "home_loan.feather")
}

