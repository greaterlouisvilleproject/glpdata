library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(survey)
degree_county_adult <- degree_county %>%
  process_census(cat_var = "educ", output_name = "degree",
                 age_groups = c("25_34", "35_44", "45_64", "25_64"),
                 output_percent = F, output_population = T)

test1 <- degree_county_all %>% reshape_55k1()

test2 <- degree_county_all %>% reshape_55k2()

test3 <- test %>% filter(Gender == "Both", Age == "")

write_csv(test2, "community_peer_cities.csv")


reshape_55k1 <- function(df) {
  df %<>%
    filter(FIPS == "21111", race == "total", year >= 2008) %>%
    select(year, sex, educ, degree_25_34:degree_25_64_pop) %>%
    pivot_longer(cols = degree_25_34:degree_25_64_pop,
                 names_to = "var",
                 values_to = "value") %>%
    mutate(
      age = str_extract(var, "(?<=degree_).{5}"),
      cat = if_else(str_detect(var, "pop"), "population", "Freq")) %>%
    pivot_wider(id_cols = c(year, sex, age, educ),
                names_from = cat,
                values_from = value) %>%
    mutate(
      EDUC = replace(educ, educ == "no_hs",    "Did not finish High School"),
      EDUC = replace(EDUC, educ == "hs",       "High School Graduate"),
      EDUC = replace(EDUC, educ == "some_col", "Some College"),
      EDUC = replace(EDUC, educ == "assoc",    "Associate"),
      EDUC = replace(EDUC, educ == "bach",     "Bachelor's"),
      EDUC = replace(EDUC, educ == "grad",     "Masters or better"),

      AGE = replace(age, age == "25_34", "25-34"),
      AGE = replace(AGE, age == "35_44", "35-44"),
      AGE = replace(AGE, age == "45_64", "45-64"),

      SEX = replace(sex, sex == "total", "both") %>%
        str_to_title())

  assoc_plus <- df %>%
    group_by(across(c("SEX", "AGE", "year"))) %>%
    filter(EDUC %in% c("Associate", "Bachelor's", "Masters or better")) %>%
    summarise(Freq = sum(Freq),
              population = first(population), .groups = "drop") %>%
    mutate(EDUC = "Associate +")

  bach_plus <- df %>%
    group_by(across(c("SEX", "AGE", "year"))) %>%
    filter(EDUC %in% c("Bachelor's", "Masters or better")) %>%
    summarise(Freq = sum(Freq),
              population = first(population), .groups = "drop") %>%
    mutate(EDUC = "Bachelor's +")

  output <- bind_rows(df, assoc_plus, bach_plus)

  output %<>%
    transmute(
      ` ` = row_number(),
      Year = paste0("1/1/", year),
      Gender = SEX,
      Age = AGE,
      Education = EDUC,
      Freq,
      Value = Freq  / population * 100)

  output
}

reshape_55k2 <- function(df) {

  df %<>%
    pull_peers(add_info = TRUE) %>%
    filter(baseline == 1, sex == "total", race == "total", year >= 2008) %>%
    select(city, year, educ, degree_25_64, degree_25_64_pop) %>%
    mutate(
      EDUC = replace(educ, educ == "no_hs",    "Did not finish High School"),
      EDUC = replace(EDUC, educ == "hs",       "High School Graduate"),
      EDUC = replace(EDUC, educ == "some_col", "Some College"),
      EDUC = replace(EDUC, educ == "assoc",    "Associate"),
      EDUC = replace(EDUC, educ == "bach",     "Bachelor's"),
      EDUC = replace(EDUC, educ == "grad",     "Masters or better"),

      Freq = degree_25_64,
      population = degree_25_64_pop)

  assoc_plus <- df %>%
    group_by(city, year) %>%
    filter(EDUC %in% c("Associate", "Bachelor's", "Masters or better")) %>%
    summarise(Freq = sum(Freq),
              population = first(population), .groups = "drop") %>%
    mutate(EDUC = "Associate +")

  bach_plus <- df %>%
    group_by(city, year) %>%
    filter(EDUC %in% c("Bachelor's", "Masters or better")) %>%
    summarise(Freq = sum(Freq),
              population = first(population), .groups = "drop") %>%
    mutate(EDUC = "Bachelor's +")

  output <- bind_rows(df, assoc_plus, bach_plus)

  output %<>%
    transmute(
      ` ` = row_number(),
      City = city,
      Year = paste0("1/1/", year),
      Education = EDUC,
      Value = Freq  / population * 100)

  output
}

write_csv(test, "community_louisville_attainment.csv")
#function for adding in combinations involving group totals
pumsagg <- function(df_original){

  years <- unique(df_original$year)

  for(yr in 1:length(years)){

    df <- df_original %>% filter(year == years[yr])

    edu   <- unique(df_original$EDUC)
    sex   <- unique(df_original$SEX)
    race  <- unique(df_original$RACE)
    age   <- unique(df_original$AGE)

    #Create totals based on AGE
    for(sx in 1:length(sex)){

      for(ed in 1:length(edu)){

        for(ra in 1:length(race)){
          temp <- df %>%
            filter(RACE==race[ra], SEX==sex[sx], EDUC==edu[ed]) %>%
            summarise(Freq = sum(Freq)) %>%
            transmute(
              year = years[yr],
              EDUC   = edu[ed],
              RACE = race[ra],
              SEX  = sex[sx],
              AGE  = "25-64",
              Freq)

          df <- bind_rows(df, temp)

        }
      }
    }
    age<- unique(df$AGE)


    #Create totals based on SEX
    for(ag in 1:length(age)){

      for(ed in 1:length(edu)){

        for(ra in 1:length(race)){
          temp <- df %>%
            filter(RACE==race[ra], AGE==age[ag], EDUC==edu[ed]) %>%
            summarise(Freq = sum(Freq)) %>%
            transmute(
              year = years[yr],
              EDUC   = edu[ed],
              RACE = race[ra],
              SEX  = "Both",
              AGE  = age[ag],
              Freq)

          df <- bind_rows(df, temp)

        }
      }
    }
    sex<- unique(df$SEX)

    #Create totals based on RACE
    for(ag in 1:length(age)){

      for(ed in 1:length(edu)){

        for(sx in 1:length(sex)){
          temp <- df %>%
            filter(SEX==sex[sx], AGE==age[ag], EDUC==edu[ed]) %>%
            summarise(Freq = sum(Freq)) %>%
            transmute(
              year = years[yr],
              EDUC   = edu[ed],
              RACE = "All",
              SEX  = sex[sx],
              AGE  = age[ag],
              Freq)

          df <- bind_rows(df, temp)

        }
      }
    }

    if(years[yr] == 2008){
      output <- df
    } else {
      output %<>% bind_rows(df)
    }
  }
  output
}

#Read in data
acs_micro <- feather::read_feather("data-raw/microdata/acs_micro.feather")

#Process data
acs_micro %<>%
  filter(
    year >= 2008,
    FIPS == 21111,
    age >=25 & age <= 64) %>%
  mutate(
    EDUC = replace(educ, educ == "no_hs",    "Did not finish High School"),
    EDUC = replace(EDUC, educ == "hs",       "High School Graduate"),
    EDUC = replace(EDUC, educ == "some_col", "Some College"),
    EDUC = replace(EDUC, educ == "assoc",    "Associate"),
    EDUC = replace(EDUC, educ == "bach",     "Bachelor's"),
    EDUC = replace(EDUC, educ == "grad",     "Masters or better"),

    AGE = replace(age, age >= 35, "25-34"),
    AGE = replace(AGE, age >= 35, "35-44"),
    AGE = replace(AGE, age >= 45, "45-54"),
    AGE = replace(AGE, age >= 55, "55-64"),

    RACE = str_to_title(race),

    SEX = str_to_title(sex))

#Create survey object and calculate degree holders
svy <- svydesign(ids = ~0, weights = ~PERWT, data = acs_micro)

results <- as.data.frame(svytable(~EDUC+RACE+SEX+AGE+year, svy), stringsAsFactors = FALSE)

assoc_plus <- results %>%
  group_by(RACE, SEX, AGE, year) %>%
  filter(EDUC %in% c("Associate", "Bachelor's", "Masters or better")) %>%
  summarise(Freq = sum(Freq)) %>%
  mutate(EDUC = "Associate +")

bach_plus <- results %>%
  group_by(RACE, SEX, AGE, year) %>%
  filter(EDUC %in% c("Bachelor's", "Masters or better")) %>%
  summarise(Freq = sum(Freq)) %>%
  mutate(EDUC = "Bachelor's +")

results %<>% bind_rows(assoc_plus, bach_plus)

results %<>% select(year, EDUC, RACE, SEX, AGE, Freq)

peer_results <- glpdata:::degree_55k

peer_results %<>%
  rename(
    educ = variable,
    Freq = value) %>%
  filter(educ != "total") %>%
  mutate(
    EDUC = replace(educ, educ == "no_hs",    "Did not finish High School"),
    EDUC = replace(EDUC, educ == "hs",       "High School Graduate"),
    EDUC = replace(EDUC, educ == "some_col", "Some College"),
    EDUC = replace(EDUC, educ == "assoc",    "Associate"),
    EDUC = replace(EDUC, educ == "bach",     "Bachelor's"),
    EDUC = replace(EDUC, educ == "grad",     "Masters or better"),

    AGE = replace(age, age == "25_34", "25-34"),
    AGE = replace(AGE, age == "35_44", "35-44"),
    AGE = replace(AGE, age == "45_64", "45-64"),

    SEX = str_to_title(sex))

louisville_results <- peer_results %>%
  filter(city == "Louisville") %>%
  select(year, SEX, AGE, EDUC, Freq)

louisville_results %<>%
  mutate(RACE = "total") %>%
  pumsagg() %>%
  filter(RACE == "total") %>%
  select(-RACE) %>%
  group_by(year, SEX, AGE) %>%
  mutate(total_residents = sum(Freq)) %>%
  ungroup() %>%
  group_by(year, SEX, AGE, EDUC) %>%
  mutate(value = Freq / total_residents * 100) %>%
  ungroup()

assoc_plus <- louisville_results %>%
  group_by(year, SEX, AGE) %>%
  filter(EDUC %in% c("Associate", "Bachelor's", "Masters or better")) %>%
  summarise(
    Freq = sum(Freq),
    value = sum(value)) %>%
  mutate(EDUC = "Associate +")

bach_plus <- louisville_results %>%
  group_by(year, SEX, AGE) %>%
  filter(EDUC %in% c("Bachelor's", "Masters or better")) %>%
  summarise(
    Freq = sum(Freq),
    value = sum(value)) %>%
  mutate(EDUC = "Bachelor's +")

louisville_results %<>%
  bind_rows(assoc_plus, bach_plus) %>%
  transmute(
    ` ` = row_number(),
    Year = paste0("1/1/", year),
    Gender = SEX,
    Age = AGE,
    Education = EDUC,
    Freq,
    Value = value)

peer_results %<>%
  group_by(city, year, EDUC) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  group_by(city, year) %>%
  mutate(total_residents = sum(Freq)) %>%
  ungroup() %>%
  mutate(value = Freq / total_residents * 100) %>%
  select(city, year, EDUC, value)

assoc_plus <- peer_results %>%
  group_by(year, city) %>%
  filter(EDUC %in% c("Associate", "Bachelor's", "Masters or better")) %>%
  summarise(value = sum(value)) %>%
  mutate(EDUC = "Associate +") %>%
  ungroup()

bach_plus <- peer_results %>%
  group_by(year, city) %>%
  filter(EDUC %in% c("Bachelor's", "Masters or better")) %>%
  summarise(value = sum(value)) %>%
  mutate(EDUC = "Bachelor's +") %>%
  ungroup()

peer_results %<>%
  bind_rows(assoc_plus, bach_plus) %>%
  transmute(
    ` ` = row_number(),
    City = city,
    Year = paste0("1/1/", year),
    Education = EDUC,
    Value = value)

write_csv(louisville_results, "../Dashboard/community_louisville_attainment.csv")
write_csv(peer_results, "../Dashboard/community_peer_city_attainment.csv")
#write.csv(results, file = "Educational Attainment by Race.csv")

results %<>% pumsagg()


results %<>%
  select(year, RACE, SEX, AGE, educ, Freq) %>%
  arrange(year, RACE, AGE, SEX, educ)

results$Freq <- as.numeric(results$Freq)
results$PopulationSize <- results$Freq

for(i in 1:(nrow(results)/8)){

  #get the first row of the demographic group
  j <- i*8 - 7

  #Add together education levels to find the total population size
  k <- sum(results$Freq[c(j,(j+2),(j+4):(j+7))])

  #Set Freq equal to a percentage of the population
  results$Freq[j:(j+7)] <- round(results$Freq[j:(j+7)] / k * 100, digits = 1)
}




