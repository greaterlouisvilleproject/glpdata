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
    food_insecure_total <- paste0("# of Food Insecure Persons in ", y)
    child_food_insecure_var <- paste0(y, " Child food insecurity rate")
    budget_shortfall_var <- paste0(y, " Weighted Annual Food Budget Shortfall")
    cost_per_meal_var <- paste0(y, " Cost Per Meal")
    # Tidy data frame
    df <- df %>%
      transmute(
        FIPS = str_pad(FIPS, 5, "left", "0"),
        year = y,
        food_insecurity = .data[[food_insecure_var]],
        food_insecurity_total = .data[[food_insecure_total]],
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

#Method for weighting the budget shortfall based on the number of food insecure in an area
standardizing_df <- food_security_df %>%
  select(c('FIPS', 'year', 'food_insecurity_total')) %>%
  pull_peers() %>%
  stl_merge(food_insecurity_total, simple=T) %>%
  mutate(multiplier = case_when(
    (year == 2014 & food_insecurity_total >= 129530) ~ 1/(food_insecurity_total/129530),
    (year == 2014 & food_insecurity_total < 129530) ~ 129530/food_insecurity_total,
    (year == 2015 & food_insecurity_total >= 122030) ~ 1/(food_insecurity_total/122030),
    (year == 2015 & food_insecurity_total < 122030) ~ 122030/food_insecurity_total,
    (year == 2016 & food_insecurity_total >= 120100) ~ 1/(food_insecurity_total/120100),
    (year == 2016 & food_insecurity_total < 120100) ~ 120100/food_insecurity_total,
    (year == 2017 & food_insecurity_total >= 116790) ~ 1/(food_insecurity_total/116790),
    (year == 2017 & food_insecurity_total < 116790) ~ 116790/food_insecurity_total,
    (year == 2018 & food_insecurity_total >= 100450) ~ 1/(food_insecurity_total/100450),
    (year == 2018 & food_insecurity_total < 100450) ~ 100450/food_insecurity_total,
    (year == 2019 & food_insecurity_total >= 89690) ~ 1/(food_insecurity_total/89690),
    TRUE ~ 89690/food_insecurity_total))

#Now bring in the budget shortfall data:
budget_shortfall_df <- food_security_df %>%
  select(c('FIPS', 'year', 'budget_shortfall')) %>%
  pull_peers() %>%
  stl_merge(budget_shortfall, simple=T)

standardized_budget_shortfall <- merge(standardizing_df, budget_shortfall_df) %>%
  mutate(budget_shortfall_standardized = multiplier*budget_shortfall)

usethis::use_data(standardized_budget_shortfall, overwrite = TRUE)

