library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

source("data-raw/helpers/process_health_index.R")

path <- "data-raw/health/"

load("R/sysdata.rda")

if (!exists("natality_county"))   source(path %p% "natality.R")
if (!exists("mortality_county"))  source(path %p% "mortality.R")
if (!exists("brfss_msa_1yr"))     source(path %p% "brfss.R")

health_index_county <- process_health_index(natality_county, mortality_county, brfss_msa_1yr)

update_sysdata(health_index_county)

rm(natality_county, mortality_county, brfss_msa_1yr, path)
