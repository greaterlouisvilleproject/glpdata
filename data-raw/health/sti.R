suppressMessages(library(tidyverse))
library(glptools)

path <- "data-raw/health/sti/"

#Gather and clean data
sti_county <- read.csv(paste0(path, 'sti_total.csv')) %>%
  select(-c(Indicator,Geography)) %>%
  mutate(race='total', sex='total') %>%
  rename('year'='Year','rate_per_100000'='Rate.per.100000') %>%
  glptools::pull_peers(add_info = FALSE, subset_to_peers = TRUE, geog="FIPS")

usethis::use_data(sti_county, overwrite = TRUE)
