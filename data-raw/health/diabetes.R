library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/health/diabetes/"

diabetes_total <- read_tsv(path %p% "total.txt")

diabetes_total %<>%
  clean_wonder(method = "weight") %>%
  select(FIPS, year, diabetes_mort = rate) %>%
  mutate(race = "total", sex = "total") %>%
  organize()

bw_cancer_diabetes <- read_tsv(path %p% "bw_cancer_diabetes.txt")
bw_cancer          <- read_tsv(path %p% "bw_cancer.txt")

bw_cancer_diabetes %<>% clean_wonder(method = "weight") %>% rename(cancer_diabetes = rate)
bw_cancer          %<>% clean_wonder(method = "weight") %>% rename(cancer = rate)

bw <- bind_df(bw_cancer_diabetes, bw_cancer) %>%
  transmute(
    FIPS, year, race,
    sex = "total",
    diabetes_mort = cancer_diabetes - cancer) %>%
  organize()

diabetes_county <- bind_rows(diabetes_total, bw) %>%
  organize()

update_sysdata(diabetes_county)

rm(bw, bw_cancer, bw_cancer_diabetes, diabetes_total, path)
