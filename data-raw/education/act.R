library(glptools)
glp_load_packages()

source("data-raw/helpers/process_ky_ed.R")

path <- "data-raw/education/act/csv/"

act_read <- function(folder, geog = "district") {

  files <- list.files(getwd() %p% "/" %p% folder)
  y <- 2012

  for (f in files) {
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
          num_students = STDNT_TESTED_CNT,
          act_english = ENGLISH_MEAN_SCORE,
          act_math = MATHEMATICS_MEAN_SCORE,
          act_reading = READING_MEAN_SCORE,
          act_science = SCIENCE_MEAN_SCORE,
          act_composite = COMPOSITE_MEAN_SCORE)
    }
    if(y %in% 2018:2019){
      df %<>%
        transmute(
          district = DIST_NAME,
          school = SCH_NAME,
          year = y,
          demographic = DEMOGRAPHIC,
          num_students = TESTED,
          act_english = AVG_ENG,
          act_math = AVG_MA,
          act_reading = AVG_RD,
          act_science = AVG_SC,
          act_composite = AVG_COMP)
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

act_ky <- act_read(path, geog = "district")

act_ky %<>%
  clean_ky_ed(act_english:act_composite) %>%
  process_ky_ed(act_english:act_composite) %>%
  spread_ky_ed()

act_55k <- act_read(path, geog = "school")

act_55k %<>%
  clean_ky_ed(act_english:act_composite, calc_nonfrl = FALSE) %>%
  clean_55k() %>%
  gather(-school, -year, -demographic, -num_students, key = "Subject", value = "Score") %>%
  transmute(
    ` ` = row_number(),
    Year = "1/1/" %p% year,
    `High School` = school,
    `Demographic` = demographic,
    Subject = str_extract(Subject, "(?<=_).*") %>% str_to_title(),
    Score)

usethis::use_data(act_ky, act_55k, overwrite = T)

rm(act_read, clean_ky_ed, clean_55k, process_ky_ed, spread_ky_ed, path)
