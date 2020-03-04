library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(purrr)

source("data-raw/helpers/process_ky_ed.R")

path <- "data-raw/education/kscreen/"

# Read in and process KSCREEN data
read_kscreen <- function(folder){

  files <- list.files(getwd() %p% "/" %p% folder)
  y <- 2013

  for(f in files){
    df <- readxl::read_xlsx(folder %p% f, skip = 1)

    df %<>%
      filter(
        str_detect(`School Name`, "District Total|State Total"),
        `Prior Setting` == "All Students") %>%
      transmute(
        district = `District Name`,
        year = y,
        demographic = Demographic,
        num_students = `Number Tested`,
        kready = `Kindergarten Ready`) %>%
      mutate(
        num_students = gsub(",", "", num_students, fixed = TRUE),
        num_students = as.numeric(num_students),
        kready = as.numeric(kready))

    output <- assign_row_join(output, df)

    y = y + 1
  }
  output
}

kready_ky <- read_kscreen(path %p% "scores/")

kready_ky %<>% clean_ky_ed(kready)


# Create estimates of the racial distribution for calculating state scores
read_kscreen_race <- function(folder){

  for(y in 2015:2017){
    df <- readxl::read_xlsx(folder %p% y %p% ".xlsx")

    df %<>%
      filter(
        str_detect(SCH_NAME, "District Total"),
        DIST_NAME == "Jefferson County")

    if  (y %in% 2015:2016) {
      df %<>%
        filter(GRADE == "Grade K") %>%
        transmute(
          year = y,
          white = WHITE_TOTAL / MEMBERSHIP_TOTAL,
          black = BLACK_TOTAL / MEMBERSHIP_TOTAL,
          hispanic = HISPANIC_TOTAL / MEMBERSHIP_TOTAL,
          asian = ASIAN_TOTAL / MEMBERSHIP_TOTAL) %>%
        mutate_at(vars(white:asian), ~ . * 100)
    } else if (y %in% 2017) {
      df %<>%
        transmute(
          year = y,
          race = recode(DISAGGGROUP, "African American" = "black", "Asian" = "asian", "Hispanic" = "hispanic", "White" = "white",
                        "All Students" = "total", .default = NA_character_),
          enrollment = TOTAL_KIND_ENROLLMENT_CNT) %>%
        filter(!is.na(race)) %>%
        spread(key = race, value = enrollment) %>%
        mutate_at(vars(white, black, hispanic, asian), ~ as.numeric(.) / as.numeric(total) * 100) %>%
        select(-total)
    }

    output <- assign_row_join(output, df)

    y = y + 1
  }
  output
}

kready_demographics <- read_kscreen_race(path %p% "race/")

kready_2014 <- kready_ky %>%
  filter(
    district == "Jefferson County",
    year == 2014,
    demographic %in% c("white", "black", "hispanic", "asian", "total")) %>%
  select(year, demographic, num_students) %>%
  spread(key = demographic, value = num_students) %>%
  mutate_at(vars(white, black, hispanic, asian), ~ . / total * 100) %>%
  select(-total) %>%
  select(year, white, black, hispanic, asian)

kready_demographics %<>% bind_rows(kready_2014, .)

kready_demographics %<>%
  gather(-year, key = "race", value = "percent")

kready_model <- kready_demographics %>%
  nest(-race) %>%
  mutate(
    reg = map(data, ~ lm(.x$percent ~ .x$year)),
    model = map(reg, broom::tidy)) %>%
  unnest(model) %>%
  select(race, term, estimate) %>%
  spread(key = term, value = estimate) %>%
  rename(intercept = `(Intercept)`, slope = `.x$year`)

kready_est <- data.frame(
  year = rep(2013:2018, length(kready_model$race)),
  race = rep(kready_model$race, each = length(2013:2018)),
  stringsAsFactors = FALSE)

kready_est %<>%
  left_join(kready_model, by = "race") %>%
  transmute(
    district = "Jefferson County",
    year,
    demographic = race,
    percent = intercept + year * slope)

kready_est %<>%
  left_join(kready_ky %>% select(-kready), by = c("district", "year", "demographic")) %>%
  left_join(kready_ky %>%
              filter(demographic == "total") %>%
              select(-demographic, -kready) %>%
              rename(total_students = num_students),
            by = c("district", "year")) %>%
  mutate(
    num_students_est = percent * total_students / 100,
    num_students_est = if_else(is.na(num_students), num_students_est, num_students)) %>%
  select(district, year, demographic, num_students_est)

# Join back to kready_ky
kready_ky %<>%
  left_join(kready_est, by = c("district", "year", "demographic")) %>%
  mutate(num_students = if_else(!is.na(num_students_est), num_students_est, num_students)) %>%
  select(-num_students_est)

kready_ky %<>%
  process_ky_ed(kready) %>%
  spread_ky_ed()

update_sysdata(kready_ky)


rm(read_kscreen, read_kscreen_race, kready_demographics, kready_est, kready_model, kready_2014,
   clean_ky_ed, process_ky_ed, spread_ky_ed, path)
