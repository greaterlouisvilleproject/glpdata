library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

source("data-raw/helpers/process_ky_ed.R")

path <- "data-raw/education/graduation/"

grad_read <- function(folder, geog = "district") {

  files <- list.files(getwd() %p% "/" %p% folder %p% "kentucky")
  y <- 2013

  var_names <- c("COHORT_RATE", "COHORT_2014", "COHORT_2015",
                "REPORTYEAR_2016", "REPORTYEAR_2017", "GRADRATE4YR")

  for (f in files) {
    df <- read_csv(folder %p% "kentucky/" %p% f)

    if (geog == "district") {
      df %<>%
        filter(
          str_detect(SCH_NAME, "District|State"),
          !is.na(DIST_NAME))
    } else if (geog == "school") {
      df %<>% filter(DIST_NAME %in% c("Jefferson County", "State"))
    }

    if (y %in% 2013) {
      df %<>%
        transmute(
          district = DIST_NAME,
          school = SCH_NAME,
          year = y,
          demographic = DISAGG_LABEL,
          num_students = COHORT_DENOMINATOR,
          graduation = COHORT_RATE)

    } else if (y %in% 2014:2017) {

      df$value <- df[[var_names[y - 2012]]]

      df %<>%
        filter(
          COHORT_TYPE == "FOUR YEAR",
          TARGET_LABEL %in% c("Actual Score", "Denominator Count")) %>%
        transmute(
          district = DIST_NAME,
          school = SCH_NAME,
          year = y,
          demographic = DISAGG_LABEL,
          TARGET_LABEL,
          value = value %>% str_replace(",", "") %>% as.numeric())

      score_df <- df %>%
        filter(TARGET_LABEL == "Actual Score") %>%
        select(-TARGET_LABEL) %>%
        rename(graduation = value)

      denom_df <- df %>%
        filter(TARGET_LABEL == "Denominator Count") %>%
        select(-TARGET_LABEL) %>%
        rename(num_students = value)

      df <- full_join(score_df, denom_df,
                      by = c("district", "school", "year", "demographic"))

    } else if (y %in% 2018) {
      df %<>%
        transmute(
          district = DIST_NAME,
          school = SCH_NAME,
          year = y,
          demographic = DEMOGRAPHIC,
          num_students = COHORT4YR,
          graduation = GRADRATE4YR)
    }

    df %<>%
      mutate(
        num_students = gsub(",", "", num_students, fixed = TRUE),
        num_students = as.numeric(num_students),
        graduation = as.numeric(graduation))

    output <- assign_row_join(output, df)

    y <- y + 1
  }
  output
}

graduation_ky  <- grad_read(path, "district")
graduation_55k <- grad_read(path, "school")

graduation_ky %<>%
  clean_ky_ed(graduation) %>%
  process_ky_ed(graduation) %>%
  spread_ky_ed()

graduation_55k %<>%
  clean_ky_ed(graduation, calc_nonfrl = FALSE) %>%
  clean_55k() %>%
  transmute(
    year,
    `High School` = school,
    `Demographic` = demographic,
    `Graduation Rate` = graduation)

# https://nces.ed.gov/programs/coe/indicator_coi.asp
grad_read_national <- function(folder) {

  files <- list.files(getwd() %p% "/" %p% folder %p% "/national")
  y <- 2013

  for (f in files) {

    df <- readxl::read_excel(folder %p% "national/" %p% f, skip = 3)

    df %<>%
      filter(if (y %in% 2013:2015) row_number() == 2 else row_number() == 3) %>%
      transmute(year = y, White, Black, Hispanic) %>%
      gather(-year, key = "race", value = "Graduation Rate") %>%
      mutate(`Graduation Rate` = as.numeric(`Graduation Rate`))

    output <- assign_row_join(output, df)

    y <- y + 1
  }
  output
}

grad_race <- grad_read_national(path)

grad_race %<>%
  transmute(
    year,
    `High School` = "National",
    Demographic =
      replace(race, race == "Black", "African American") %>%
      replace(race == "White", "White (Non-Hispanic)"),
    `Graduation Rate`)

grad_tot <- readxl::read_excel(path %p% "national/2017.xls", skip = 2)

grad_tot %<>%
  filter(row_number() == 4) %>%
  transmute(
    `2011` = `2010-11`,
    `2012` = `2011-12`,
    `2013` = `2012-13`,
    `2014` = `2013- 14`,
    `2015` = `2014- 15`,
    `2016` = `2015- 16`,
    `2017` = `2016- 17`) %>%
  gather(key = "year", value = "Graduation Rate") %>%
  transmute(
    year = as.numeric(year),
    `High School` = "National",
    Demographic = "All Students",
    `Graduation Rate` = as.numeric(`Graduation Rate`))

graduation_55k %<>%
  bind_rows(grad_tot, grad_race) %>%
  transmute(
    ` ` = row_number(),
    Year = "1/1/" %p% year,
    `High School`,
    Demographic,
    `Graduation Rate`)


update_sysdata(graduation_ky, graduation_55k)

rm(grad_read, clean_ky_ed, clean_55k, process_ky_ed, spread_ky_ed,
   grad_read_national, grad_race, grad_tot, path)
