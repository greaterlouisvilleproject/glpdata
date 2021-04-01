library(glptools)
glp_load_packages()

source("data-raw/helpers/process_ky_ed.R")

path <- "data-raw/education/kready/"

# Read in and process KSCREEN data
read_kscreen <- function(folder, school_level = F){

  files <- list.files(getwd() %p% "/" %p% folder)
  y <- 2014

  for(f in files){
    df <- readxl::read_xlsx(folder %p% f, skip = 1, na = c("", "***"))

    if (school_level) {
      df %<>%
        filter(`District Name` == "Jefferson County") %>%
        transmute(
          code = Code,
          school = `School Name`,
          year = y,
          demographic = Demographic,
          prior_setting = `Prior Setting`,
          num_students = `Number Tested`,
          kready = `Kindergarten Ready`)
    } else {
      df %<>%
        filter(
          str_detect(`School Name`, "District Total|State Total")) %>%
        transmute(
          district = `District Name`,
          year = y,
          demographic = Demographic,
          prior_setting = `Prior Setting`,
          num_students = `Number Tested`,
          kready = `Kindergarten Ready`)
    }

    df %<>%
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
kready_jc <- read_kscreen(path %p% "scores/", school_level = T)

kready_ky %<>% clean_ky_ed(kready)

# Demographic counts are missing for JCPS students by race for several years (2014, 2016, 2017, 2019, 2020)
# Create estimates of the racial distribution for calculating state scores.
# Use racial data for 2016-2020 and KREADY data for 2015.
read_kscreen_race <- function(folder){

  for(y in 2016:2020){

    if (y %in% 2016:2019) {
      df <- readxl::read_xlsx(folder %p% y %p% ".xlsx")

      df %<>%
        filter(
          str_detect(SCH_NAME, "District Total"),
          DIST_NAME == "Jefferson County")
    }
    else {
      df <- read_csv(folder %p% y %p% ".csv")

      df %<>%
        filter(
          str_detect(`SCHOOL NAME`, "District Total"),
          `DISTRICT NAME` == "Jefferson County")
    }


    if  (y %in% c(2016:2017, 2019)) {
      df %<>%
        filter(GRADE %in% c("Grade K", "00")) %>%
        mutate(across(c(WHITE_TOTAL, BLACK_TOTAL, HISPANIC_TOTAL, ASIAN_TOTAL, MEMBERSHIP_TOTAL), ~as.numeric(.))) %>%
        transmute(
          year = y,
          white = WHITE_TOTAL / MEMBERSHIP_TOTAL,
          black = BLACK_TOTAL / MEMBERSHIP_TOTAL,
          hispanic = HISPANIC_TOTAL / MEMBERSHIP_TOTAL,
          asian = ASIAN_TOTAL / MEMBERSHIP_TOTAL) %>%
        mutate_at(vars(white:asian), ~ . * 100)
    } else if (y == 2018) {
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
    } else if (y %in% 2020) {
      df %<>%
        filter(GRADE == "KG") %>%
        transmute(
          year = y,
          white = `Total White Students` / `Total Membership`,
          black = `Total African American Students` / `Total Membership`,
          hispanic = `Total Hispanic or Latino Students` / `Total Membership`,
          asian = `Total Asian Students` / `Total Membership`) %>%
        mutate_at(vars(white:asian), ~ . * 100)
    }

    output <- assign_row_join(output, df)

    y = y + 1
  }
  output
}

kready_demographics <- read_kscreen_race(path %p% "race/")

kready_2015 <- kready_ky %>%
  filter(
    district == "Jefferson County",
    year == 2015,
    demographic %in% c("white", "black", "hispanic", "asian", "total"),
    prior_setting == "All Students") %>%
  select(year, demographic, num_students) %>%
  spread(key = demographic, value = num_students) %>%
  mutate_at(vars(white, black, hispanic, asian), ~ . / total * 100) %>%
  select(-total) %>%
  select(year, white, black, hispanic, asian)

kready_demographics %<>% bind_rows(kready_2015, .)

kready_demographics %<>%
  gather(-year, key = "race", value = "percent")

# Create estimates for 2014:2020
kready_est <- kready_demographics %>%
  group_by(race) %>%
  nest() %>%
  mutate(
    year   =  purrr::map(data, ~ Hmisc::approxExtrap(x = .$year, y = .$percent, xout = 2014:2020, na.rm = T)$x),
    percent = purrr::map(data, ~ Hmisc::approxExtrap(x = .$year, y = .$percent, xout = 2014:2020, na.rm = T)$y)) %>%
  select(-data) %>%
  unnest(cols = c(year, percent)) %>%
  ungroup() %>%
  transmute(
    district = "Jefferson County",
    year,
    demographic = race,
    prior_setting = "All Students",
    percent)

# Convert percents to student numbers
kready_est %<>%
  left_join(kready_ky %>% select(-kready),
            by = c("district", "year", "demographic", "prior_setting")) %>%
  left_join(kready_ky %>%
              filter(demographic == "total") %>%
              select(-demographic, -kready) %>%
              rename(total_students = num_students),
            by = c("district", "year", "prior_setting")) %>%
  mutate(
    num_students_est = percent * total_students / 100,
    num_students_est = if_else(is.na(num_students), num_students_est, num_students)) %>%
  transmute(
    district, year, demographic,
    prior_setting = "All Students",
    num_students_est)


# Join estimates back to kready_ky
kready_ky %<>%
  left_join(kready_est, by = c("district", "year", "demographic", "prior_setting")) %>%
  mutate(num_students = if_else(!is.na(num_students_est), num_students_est, num_students)) %>%
  select(-num_students_est)

kready_ky %<>%
  process_ky_ed(kready) %>%
  spread_ky_ed()

usethis::use_data(kready_ky, overwrite = TRUE)

rm(read_kscreen, read_kscreen_race, kready_demographics, kready_est, kready_2015,
   clean_ky_ed, process_ky_ed, spread_ky_ed, path, clean_55k)
