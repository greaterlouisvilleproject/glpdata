library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)

path <- "data-raw/qop/vibrancy_index/"

# County Business Pattern Data

cbp <- read_csv(path %p% "cbp16co.txt")

cbp %<>%
  mutate(FIPS = as.numeric(paste0(fipstate, fipscty))) %>%
  pull_peers_FIPS(add_info = FALSE)

ind_art_naics <- c("711510") # Independent Artists, Writers, and Performers

art_cult_naics <-
  c("712110", # Museums
    "712120", # Historical Sites
    "7111//") # Performing Arts Companies (theater, dance, musical group, other performing arts)

art_cult_ent_naics <-
  c(art_cult_naics,
    "61161/", # Fine arts schools
    "512110", # Motion picture production
    "5122//") # Sound recording

cbp %<>%
  mutate(
    ind_art = if_else(naics %in% ind_art_naics, 1, 0),
    art_cult = if_else(naics %in% art_cult_naics, 1, 0),
    art_cult_ent = if_else(naics %in% art_cult_ent_naics, 1, 0)) %>%
  filter((ind_art + art_cult + art_cult_ent) > 0) %>%
  #filter(naics %in% c("7121//", "712110", "712120", "712130", "712190")) %>%
  select(FIPS, naics, emp, empflag, est, ap, ind_art, art_cult, art_cult_ent) %>%
  mutate(year = 2016)


emp_est <- data.frame(
  naics = "712120",
  FIPS  = as.character(c(12031, 18097, 29095, 29189, 31055, 39049, 39061,
                         39113, 40109, 40143, 45045, 47157)),
  emp_est = c(13,    9.5,   52,    7.3,   3,     7.5,   59.5,
              3,     7,     9.5,   1,     57.5),
  stringsAsFactors = FALSE)

cbp %<>%
  left_join(emp_est, by = c("naics", "FIPS")) %>%
  mutate(
    emp = if_else(naics == "712120" & !is.na(empflag),
                  emp_est,
                  emp))

indep_artists <- cbp %>%
  filter(ind_art == 1) %>%
  stl_merge(emp, method = "sum") %>%
  left_join(population_df_merged %>% filter(year == 2016), by = c("FIPS", "year")) %>%
  mutate(ind_art = emp / population * 100000) %>%
  select(-emp, -population)

art_cult <- cbp %>%
  filter(art_cult == 1) %>%
  group_by(FIPS, year) %>%
  summarise(emp = sum(emp)) %>%
  ungroup() %>%
  stl_merge(emp, method = "sum") %>%
  left_join(population_df_merged %>% filter(year == 2016), by = c("FIPS", "year")) %>%
  mutate(art_cult = emp / population * 100000) %>%
  select(-emp, -population)

art_cult_ent <- cbp %>%
  filter(art_cult_ent == 1) %>%
  group_by(FIPS, year) %>%
  summarise(est = sum(est)) %>%
  ungroup() %>%
  stl_merge(est, method = "sum") %>%
  left_join(population_df_merged %>% filter(year == 2016), by = c("FIPS", "year")) %>%
  mutate(art_cult_ent = est / population * 100000) %>%
  select(-est, -population)

art_cult_payroll <- cbp %>%
  filter(art_cult == 1) %>%
  group_by(FIPS, year) %>%
  summarise(art_cult_payroll = sum(ap)) %>%
  ungroup() %>%
  stl_merge(art_cult_payroll, method = "sum") %>%
  left_join(population_df_merged %>% filter(year == 2016), by = c("FIPS", "year")) %>%
  mutate(art_cult_payroll = art_cult_payroll / population * 100000) %>%
  select(-population)


# Business Master File

bmf <- read_csv(path %p% "bmf.bm1812.csv")

arts_cult_ntee <-
  c(
  "A50", # Museums
  "A25", # Arts education
  "A27", # Community Celebration
  "A62", # Dance
  "A68", # Music
  "A6A", # Opera
  "A61", # Performing arts centers
  "A69", # Orchestras
  "A65", # Theater
  "A01", # Alliances and Advocacy
  "A11",  # Single Organization Support
  "A02",
  "A12",
  "A19")

bmf %<>%
  mutate(FIPS = as.numeric(FIPS)) %>%
  pull_peers_FIPS() %>%
  filter(NTEECC %in% arts_cult_ntee) %>%
  group_by(FIPS) %>%
  summarise(
    art_cult_org = n(),
    art_cult_income = sum(INCOME, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(year = 2016) %>%
  stl_merge(art_cult_org, art_cult_income, method = "sum") %>%
  left_join(population_df_merged %>% filter(year == 2016), by = c("FIPS", "year")) %>%
  mutate(
    art_cult_org = art_cult_org / population * 100000,
    art_cult_income = art_cult_income / population) %>%
  select(-population)


# Bind and process data frames

providers <- bind_df(indep_artists, art_cult, art_cult_ent, art_cult_payroll, bmf)

providers_z <- providers %>%
  pull_peers_FIPS() %>%
  filter(current == 1) %>%
  organize() %>%
  group_by(year) %>%
  mutate_at(vars(ind_art:art_cult_income), norm_z) %>%
  ungroup() %>%
  rename_at(vars(ind_art:art_cult_income), paste0, "_index")

providers_z %<>%
  mutate(
    art_provider_index = (ind_art_index + art_cult_index + art_cult_ent_index + art_cult_org_index) / 4,
    art_dollars_index = (art_cult_income_index + art_cult_payroll_index) / 2,
    vibrancy_index = (art_provider_index + art_dollars_index) / 2)

vibrancy_index_county <- providers_z %>%
  select(FIPS, year, art_provider_index:vibrancy_index) %>%
  mutate(sex = "total", race ="total") %>%
  organize()

update_sysdata(vibrancy_index_county)

rm(art_cult, art_cult_ent, art_cult_payroll,
   bmf, cbp, emp_est, indep_artists, providers, providers_z,
   arts_cult_ntee, art_cult_ent_naics, art_cult_naics, ind_art_naics, path)

if(FALSE){
  library(classInt)
  library(scales)
  library(ggthemes)
  library(showtext)
  library(ggplot2)

providers_bar <- providers_z %>%
  select(city, ind_art_index:art_cult_income_index) %>%
  mutate_at(vars(ind_art_index, art_cult_index, art_cult_ent_index, art_cult_org_index), funs(. * .125)) %>%
  mutate_at(vars(art_cult_income_index, art_cult_payroll_index), funs(. * .25)) %>%
  gather(-city, key = "Component", value = "Index")


cities_ord <- providers_z %>% arrange(art_index)

cities_ord <- cities_ord$city

providers_bar$city <-
  factor(providers_bar$city,
         levels = cities_ord,
         ordered = TRUE)

providers_bar$Component <-
  factor(providers_bar$Component,
         levels = c("ind_art_index", "art_cult_index", "art_cult_org_index",
                    "art_cult_ent_index", "art_cult_income_index", "art_cult_payroll_index"),
         labels = c("Independent Artists", "Arts and Culture Employees", "Arts and Culture Nonprofits",
                    "Arts, Culture, and Entertainment Firms", "Arts and Culture Contributions", "Arts and Culture Payroll"),
         ordered = TRUE)

ggplot() +
  geom_bar(data = providers_bar, aes(x = city, y = Index, fill = Component), stat = "identity") +
  scale_fill_manual(values = brewer_pal(type = "qual", palette = "Set3")(6)) +
  theme(
    text = element_text(family = "Museo Sans 300"),
    plot.title = element_text(size = 42, hjust = 0.5, margin = margin(b = 10, unit = "pt")),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(angle = 45, vjust = .5),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 12)) +
  labs(title = "Arts Index Components")

ggplot(providers_bar %>% filter(city == "Louisville")) +
  aes(x = city, y = Index, fill = Component) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = brewer_pal(type = "qual", palette = "Set3")(6)) +
  theme(
    text = element_text(family = "Museo Sans 300"),
    plot.title = element_text(size = 42, hjust = 0.5, margin = margin(b = 10, unit = "pt")),
    plot.subtitle = element_text(size = 14, hjust = 0.5),
    axis.text = element_text(size = 14),
    axis.text.x = element_text(vjust = .5),
    axis.title = element_text(size = 20),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 12)) +
  labs(title = "Arts Index, Louisville",
       subtitle  = "All values except for Arts and Culture Nonprofits are negative for Louisville. Smaller values (closer to 0) are better.")
}









