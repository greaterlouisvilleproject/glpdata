library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(purrr)

path <- "data-raw/jobs/job_flow/"

get_qwi <- function(variables = c("FrmJbGn", "FrmJbLs", "FrmJbGnS", "FrmJbLsS", "Emp", "EmpEnd"),
                    group = "total",
                    industry_level = "A",
                    geog = "FIPS",
                    geog_codes = c("01073", FIPS_df$FIPS[2:22])) {

  if (group == "total") {
    endpoint <- "sa"
    demog_groups <- "&agegrp=A00&sex=0"
  } else if (group == "sex") {
    endpoint <- "sa"
    demog_groups <- "&agegrp=A00&sex=1&sex=2"
  } else if (group == "race") {
    endpoint <- "rh"
    demog_groups <- "&race=A0&race=A1&race=A2&ethnicity=A1&ethnicity=A2"
  }

  if (geog == "FIPS") yrs <- 2002:2017 else yrs <- 2005:2016

  urls <- crossing(years = yrs,
                   endpoint,
                   demog_groups,
                   geog_codes,
                   industry_code = "00") %>%
     mutate(
       url = paste0(
        "https://api.census.gov/data/timeseries/qwi/", endpoint, "?get=",
        paste0(variables, collapse = ","),
        "&for=county:", str_sub(geog_codes, 3, 5),"&in=state:", str_sub(geog_codes, 1, 2),
        "&year=", years,
        "&quarter=", paste0(1:4, collapse = ","),
        demog_groups,
        "&industry=", industry_code,
        "&key=52e7948461b29e2ed1f7c53ceee270e6f7d8bcfe"))

  results <- purrr::map(urls$url, httr::GET)

  parse_qwi <- function(x) {
    y <- x %>%
      httr::content(as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON() %>%
      as.data.frame(stringsAsFactors = FALSE)

    names(y) <- y[1,]
    y <- y[-1, ]
  }

  future::plan("future::multiprocess")

  results %<>%
    furrr::future_map(parse_qwi) %>%
    map_df(bind_rows)

  if (group == "total") {
    results %<>%
      mutate(
        sex = "total",
        race = "total")
  } else if (group == "sex") {
     results %<>%
       mutate(
        new_sex = NA,
        new_sex = replace(new_sex, sex == 0, "total"),
        new_sex = replace(new_sex, sex == 1, "male"),
        new_sex = replace(new_sex, sex == 2, "female"),
        sex = new_sex,
        race = "total") %>%
      select(-new_sex)
  } else if (group == "race") {
  results %<>%
    mutate(
      new_race = NA,
      new_race = replace(new_race, race == "A1" & ethnicity == "A1", "white"),
      new_race = replace(new_race, race == "A2" & ethnicity == "A1", "black"),
      new_race = replace(new_race, race == "A0" & ethnicity == "A2", "hispanic"),
      race = new_race,
      sex = "total") %>%
    select(-new_race) %>%
    filter(!is.na(race))
  }

  results %<>%
    mutate_at(variables, as.numeric) %>%
    mutate(
      FIPS = as.character(as.numeric(state %p% county)),
      year = as.numeric(year)) %>%
    group_by(FIPS, year, sex, race)

  results_sum <- results %>%
    summarise_at(vars(FrmJbGn:FrmJbLsS), sum)

  results_mean <- results %>%
    summarise_at(vars(Emp:EmpEnd), mean)

  results <- bind_df(results_sum, results_mean) %>%
    ungroup()

  if (geog == "MSA") {
    results %<>%
      left_join(MSA_FIPS, by = "FIPS") %>%
      group_by(MSA, year, race, sex) %>%
      summarise_at(vars(FrmJbGn:EmpEnd), sum) %>%
      ungroup()
  }

  results %<>%
    {if (geog == "FIPS") stl_merge(., variables, method = "sum") else .} %>%
    mutate(
      avg_emp = (Emp + EmpEnd) / 2,
      job_creation = FrmJbGn / avg_emp * 100,
      job_creation_s = FrmJbGnS / avg_emp * 100,
      job_destruction = FrmJbLs / avg_emp * 100,
      net_job_creation = job_creation - job_destruction,
      net_job_creation_stable = (FrmJbGnS - FrmJbLsS) / avg_emp * 100) %>%
    select(!!geog, year, sex, race, job_creation, job_destruction, net_job_creation,
           job_creation_s, net_job_creation_stable, avg_emp)

  results
}

job_flow_total <- get_qwi()
job_flow_sex   <- get_qwi(group = "sex")
job_flow_race  <- get_qwi(group = "race")

all_FIPS <- MSA_FIPS$FIPS %>%
  subset(. != "MERGED") %>%
  str_pad(5, "left", "0")

job_flow_msa_1yr <- get_qwi(geog = "MSA", geog_codes = all_FIPS)

job_flow_county <- bind_rows(job_flow_total, job_flow_sex, job_flow_race)

update_sysdata(job_flow_county, job_flow_msa_1yr)

rm(get_qwi, job_flow_total, job_flow_sex, job_flow_race, path)
