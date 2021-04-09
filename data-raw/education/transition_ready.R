library(glptools)
glp_load_packages()

source("data-raw/helpers/process_ky_ed.R")

path <- "data-raw/education/transition_ready/"

tr_read <- function(folder, geog = "district") {

  files <- list.files(getwd() %p% "/" %p% folder)
  y <- 2012

  for(f in files){

    print(y)

    df <- read_csv(folder %p% f)

    if (geog == "district") {

      if (y <= 2019) {
        df %<>%
          filter(
            str_detect(SCH_NAME, "District|State"),
            !is.na(DIST_NAME))
      } else {
        df %<>%
          filter(
            str_detect(`SCHOOL NAME`, "District|State"),
            !is.na(`DISTRICT NAME`))
      }

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
          transition_ready = PCT_CCR_NO_BONUS)
    }
    if(y %in% 2018:2019){
      df %<>%
        transmute(
          district = DIST_NAME,
          school = SCH_NAME,
          year = y,
          demographic = DEMOGRAPHIC,
          num_students = GRADS,
          transition_ready = TRANSITIONRATE)
    }
    if(y %in% 2020){
      df %<>%
        transmute(
          district = `DISTRICT NAME`,
          school = `SCHOOL NAME`,
          year = y,
          demographic = DEMOGRAPHIC,
          num_students = GRADS,
          transition_ready = TRANSITIONRATE)
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

transition_ready_ky <- tr_read(path, geog = "district")

transition_ready_ky %<>%
  clean_ky_ed(transition_ready) %>%
  process_ky_ed(transition_ready) %>%
  spread_ky_ed()

transition_ready_55k <- tr_read(path, geog = "school")

transition_ready_55k %<>%
  clean_ky_ed(transition_ready, calc_nonfrl = FALSE) %>%
  clean_55k() %>%
  transmute(
    Year = "1/1/" %p% year,
    `High School` = school,
    `Demographic` = demographic,
    `Transition Readiness` = transition_ready)

transition_ready_blank <- transition_ready_55k %>%
  filter(Year == "1/1/2016") %>%
  mutate(
    Year = "6/1/2016",
    `Transition Readiness` = NA)

transition_ready_55k %<>%
  bind_rows(transition_ready_blank) %>%
  mutate(` ` = row_number()) %>%
  select(` `, everything())

usethis::use_data(transition_ready_ky, transition_ready_55k, overwrite = TRUE)

rm(tr_read, clean_ky_ed, clean_55k, transition_ready_blank, process_ky_ed, spread_ky_ed, path)
