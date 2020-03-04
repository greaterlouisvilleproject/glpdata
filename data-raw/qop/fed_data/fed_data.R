library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

library(feather)

path <- "data-raw/qop/fed_data/"

fed_spending <- readxl::read_xlsx(path %p% "Louisville Peer City Analysis.xlsx", n_max = 51)

fed_spending$city <- str_split(fed_spending$Region, ",", simplify = TRUE)[,1]

fed_spending %<>%
  transmute(
    city,
    year = Year,
    cra_small_business = `CRA Small Business Loan Origination Amounts`,
    home_purchase_loan = `Home Purchase Loan Origination Amounts`,
    new_market_tax_credit = `New Markets Tax Credit Investments`,
    cdfi = `CDFI Investments`,
    li_housing = `Low Income Housing Tax Credit Investments`,
    home = `HOME Grants **`,
    cdbg = `CDBG Grants **`,
    public_charity = `Public Charity Grants`) %>%
  left_join(FIPS_df_one_stl, by = "city") %>%
  pull_peers_FIPS(add_info = FALSE) %>%
  stl_merge(cra_small_business:public_charity, method = "sum") %>%
  mutate(sex = "total", race = "total") %>%
  left_join(get_sysdata("population_county"), by = c("FIPS", "year", "sex", "race")) %>%
  mutate_at(vars(cra_small_business:public_charity), ~ . / population) %>%
  mutate(new_market_tax_credit = replace_na(new_market_tax_credit, 0)) %>%
  select(-population, -core_county) %>%
  organize()

update_sysdata("fed_spending")

rm(path)
