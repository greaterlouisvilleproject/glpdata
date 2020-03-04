library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

source("data-raw/helpers/process_ky_ed.R")

path <- "data-raw/education/ccr/"

ccr_read <- function(folder, geog = "district") {

  files <- list.files(getwd() %p% "/" %p% folder)
  y <- 2012

  for(f in files){
    df <- read_csv(folder %p% f)

    if (geog == "district") {
      df %<>%
        filter(
          str_detect(SCH_NAME, "District|State"),
          !is.na(DIST_NAME))
    } else if (geog == "school") {
      df %<>% filter(DIST_NAME %in% c("Jefferson County", "State"))
    }

    if(y %in% 2012:2017){
      df %<>%
        transmute(
          district = DIST_NAME,
          school = SCH_NAME,
          year = y,
          demographic = DISAGG_LABEL,
          num_students = NBR_GRADUATES_WITH_DIPLOMA,
          ccr = PCT_CCR_NO_BONUS)
    }
    if(y %in% 2018){
      df %<>%
        transmute(
          district = DIST_NAME,
          school = SCH_NAME,
          year = y,
          demographic = DEMOGRAPHIC,
          num_students = GRADS,
          ccr = TRANSITIONRATE)
    }

    df %<>%
      mutate(
        num_students = gsub(",", "", num_students, fixed = TRUE),
        num_students = as.numeric(num_students))

    output <- assign_row_join(output, df)

    y <- y + 1
  }
  output
}

ccr_ky <- ccr_read(path, geog = "district")

ccr_ky %<>%
  clean_ky_ed(ccr) %>%
  process_ky_ed(ccr) %>%
  spread_ky_ed()

ccr_55k <- ccr_read(path, geog = "school")

ccr_55k %<>%
  clean_ky_ed(ccr, calc_nonfrl = FALSE) %>%
  clean_55k() %>%
  transmute(
    Year = "1/1/" %p% year,
    `High School` = school,
    `Demographic` = demographic,
    `College and Career Readiness` = ccr)

ccr_blank <- ccr_55k %>%
  filter(Year == "1/1/2016") %>%
  mutate(
    Year = "6/1/2016",
    `College and Career Readiness` = NA)

ccr_55k %<>%
  bind_rows(ccr_blank) %>%
  mutate(` ` = row_number()) %>%
  select(` `, everything())

update_sysdata(ccr_ky, ccr_55k)

rm(ccr_read, clean_ky_ed, clean_55k, ccr_blank, process_ky_ed, spread_ky_ed, path)
