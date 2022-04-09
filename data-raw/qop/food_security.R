library(dplyr)
library(stringr)
library(glptools)
library(tidyr)

# Function to read in the MMG data
mind_the_meal_gap <- function(folder, starting_year){

  wd <- getwd()
  directory <- paste0(wd, "/", folder)
  file_names <- list.files(directory)

  # Read file for each year
  for (y in starting_year:2019){

    # Create parameters to read in sheet based on the year
    file_path <- paste0(wd, "/", folder, "/", file_names[y-2010])

    #Accounting for different naming conventions in earlier years
    sheet_name <- paste0(y, " County")

    skip_num <- case_when(
      y %in% 2009:2017 ~ 0,
      y %in% 2018 ~ 1,
      y %in% 2019 ~ 0)

    df <- readxl::read_xlsx(file_path, sheet = sheet_name, skip = skip_num)
    # Create variables names based on the year
    food_insecure_var <- paste0(y, " Food Insecurity Rate")
    child_food_insecure_var <- paste0(y, " Child food insecurity rate")
    budget_shortfall_var <- paste0(y, " Weighted Annual Food Budget Shortfall")
    cost_per_meal_var <- paste0(y, " Cost Per Meal")
    # Tidy data frame
    df <- df %>%
      transmute(
        FIPS = str_pad(FIPS, 5, "left", "0"),
        year = y,
        food_insecurity = .data[[food_insecure_var]],
        child_food_insecurity = .data[[child_food_insecure_var]],
        budget_shortfall = .data[[budget_shortfall_var]],
        meal_cost = .data[[cost_per_meal_var]])
    output <- assign_row_join(output, df)
  }
  output
}

food_security_df <- mind_the_meal_gap("data-raw/qop/food_security/MMG", starting_year = 2012)

food_security_overall_glp <- food_security_df %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(food_insecurity, simple=T)

food_security_child_glp <- food_security_df %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(child_food_insecurity, simple=T)

food_security_budget_shortfall_glp <- food_security_df %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(budget_shortfall, simple=T)

food_security_meal_cost_glp <- food_security_df %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  stl_merge(meal_cost, simple=T)

food_security_county_merge <- merge(food_security_overall_glp, food_security_child_glp)
food_security_county_merge_2 <- merge(food_security_county_merge, food_security_budget_shortfall_glp)
food_security_county <- merge(food_security_county_merge_2, food_security_meal_cost_glp)

#Transform to percents
food_security_county <- food_security_county %>%
  mutate(food_insecurity=food_insecurity*100, child_food_insecurity=child_food_insecurity*100)

#Now add in the projections from 2020 and 2021
df_projections_2020_child <- readxl::read_xlsx("data-raw/qop/food_security/MMG_projections/FANO Projections - March 2021 - Food Insecurity - v2.xlsx", sheet = ' County - 2020 Projections') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  select(c('FIPS', '[Revised Projections – March 2021]\r\n2020 Food Insecurity  %', '[Revised Projections – March 2021]\r\n2020 Child Food Insecurity  %')) %>%
  rename('food_insecurity'='[Revised Projections – March 2021]\r\n2020 Food Insecurity  %', 'child_food_insecurity'='[Revised Projections – March 2021]\r\n2020 Child Food Insecurity  %') %>%
  mutate(year=2020, food_insecurity=food_insecurity*100, child_food_insecurity=child_food_insecurity*100) %>%
  stl_merge(child_food_insecurity, simple=T)

df_projections_2020_all <- readxl::read_xlsx("data-raw/qop/food_security/MMG_projections/FANO Projections - March 2021 - Food Insecurity - v2.xlsx", sheet = ' County - 2020 Projections') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  select(c('FIPS', '[Revised Projections – March 2021]\r\n2020 Food Insecurity  %', '[Revised Projections – March 2021]\r\n2020 Child Food Insecurity  %')) %>%
  rename('food_insecurity'='[Revised Projections – March 2021]\r\n2020 Food Insecurity  %', 'child_food_insecurity'='[Revised Projections – March 2021]\r\n2020 Child Food Insecurity  %') %>%
  mutate(year=2020, food_insecurity=food_insecurity*100, child_food_insecurity=child_food_insecurity*100) %>%
  stl_merge(food_insecurity, simple=T)

df_projections_2021_child <- readxl::read_xlsx("data-raw/qop/food_security/MMG_projections/FANO Projections - March 2021 - Food Insecurity - v2.xlsx", sheet = 'County - 2021 Projections') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  select(c('FIPS', '[Revised Projections – March 2021]\r\n2021 Food Insecurity  %', '[Revised Projections – March 2021]\r\n2021 Child Food Insecurity  %')) %>%
  rename('food_insecurity'='[Revised Projections – March 2021]\r\n2021 Food Insecurity  %', 'child_food_insecurity'='[Revised Projections – March 2021]\r\n2021 Child Food Insecurity  %') %>%
  mutate(year=2021, food_insecurity=food_insecurity*100, child_food_insecurity=child_food_insecurity*100) %>%
  stl_merge(child_food_insecurity, simple=T)

df_projections_2021_all <- readxl::read_xlsx("data-raw/qop/food_security/MMG_projections/FANO Projections - March 2021 - Food Insecurity - v2.xlsx", sheet = 'County - 2021 Projections') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS") %>%
  select(c('FIPS', '[Revised Projections – March 2021]\r\n2021 Food Insecurity  %', '[Revised Projections – March 2021]\r\n2021 Child Food Insecurity  %')) %>%
  rename('food_insecurity'='[Revised Projections – March 2021]\r\n2021 Food Insecurity  %', 'child_food_insecurity'='[Revised Projections – March 2021]\r\n2021 Child Food Insecurity  %') %>%
  mutate(year=2021, food_insecurity=food_insecurity*100, child_food_insecurity=child_food_insecurity*100) %>%
  stl_merge(food_insecurity, simple=T)

df_projections_2020 <- merge(df_projections_2020_child, df_projections_2020_all)
df_projections_2021 <- merge(df_projections_2021_child, df_projections_2021_all)

#Combine all of the data
food_security_county <- dplyr::bind_rows(food_security_county, df_projections_2020, df_projections_2021) %>%
  drop_na(food_insecurity)

usethis::use_data(food_security_county, overwrite = TRUE)
