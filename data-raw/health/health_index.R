library(glptools)
glp_load_packages()

source("data-raw/helpers/process_health_index.R")

path <- "data-raw/health/"

load("data/natality_county.Rda")
load("data/mortality_county.Rda")
load("data/brfss_msa_1yr.Rda")

natality_county %<>% filter(var_type == "percent") %>% select(-var_type)
brfss_msa_1yr   %<>% filter(var_type == "estimate") %>% select(-var_type)

health_index_county <- process_health_index(natality_county, mortality_county, brfss_msa_1yr)

health_index_county2 <- health_index_county %>%
  filter(year == 2017, race == "total", sex == "total") %>%
  ranking_data(health_index)

test <- ranking_data(health_index_county, health_index, years = 2005:2019)

update_sysdata(health_index_county)

rm(natality_county, mortality_county, brfss_msa_1yr, path)
