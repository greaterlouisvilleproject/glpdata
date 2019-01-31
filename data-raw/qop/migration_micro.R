library(tidyr)
library(readr)
library(dplyr)
library(stringr)
library(magrittr)
library(glptools)
library(survey)
library(feather)

path <- "data-raw/qop/migration/"

# Break the download up into smaller .csv files that can be read into R memory (run once)

if(FALSE){

  library(bigmemory)

  data <- read.big.matrix(path %+% "usa_00064.csv", header = TRUE)

  for(y in 2005:2017){
    year_col <- data[,1]
    year_lgl <- year_col == y

    output <- data[year_lgl,]

    output <- as.data.frame(output)
    write_feather(output, paste0(path, "single-year/", y, ".feather"))
  }
}

# Migration data uses MIGPUMAs instead of regular PUMAs. MIGPUMAs need to be assigned to MSAs.
# 1. Create a crosswalk from PUMAs to MIGPUMAs using downloads from IPUMS.
# 2. Load the crosswalk from PUMAs to MSAs. (Created using data from IPUMS.)
# 3. Combine crosswalks to bridge over PUMAs, creating a crosswalk from MIGPUMAs to MSAs.



# 1. Start with crosswalks from PUMAs to MIGPUMAs.
#    The crosswalk uses PUMA definitions put into use in 2012.
#    Code for earlier definitions is commented out at the bottom.

crosswalk_12 <- read_csv(path %+% "puma_migpuma1_pwpuma00.csv", skip = 2, n_max = 2378)

crosswalk_12 %<>%
  transmute(
    STATEFIP = `State of Residence (ST)`,
    PUMA,
    MIGPUMA = `PWPUMA00 or MIGPUMA1`)

# replicate the crosswalk for each year

crosswalk_12 <- data.frame(
  crosswalk_12,
  year = rep(2012:2017, each = nrow(crosswalk_12)))

PUMA_MIGPUMA <- crosswalk_12


# 2. Crosswalk from PUMAs to MSAs exists in the glptools package

MSA_PUMA <- MSA_PUMA %>% filter(year >= 2012)

# 3. Combine crosswalks to bridge over PUMAs.
#    Rename MSA to MIGMSA to differentiate from MSA of residence.
#    Filter to unique rows

MIGPUMA_MSA <- left_join(MSA_PUMA, PUMA_MIGPUMA, by = c("STATEFIP", "PUMA", "year"))

MIGPUMA_MSA %<>%
  select(year, STATEFIP, MIGPUMA, MSA) %>%
  rename(MIGMSA = MSA) %>%
  unique()


# Load and process ACS.
# Outmigration data requires using the full US sample, so the data is read in one year at a time
# and subset to observations where the

# Attempt to reproduce ACS overall migration data. To change back to educational attainment:
# Subset to age 25+ to mirror FactFinder's migration by education status universe.
#


for(y in 2013:2017){
  acs <- read_feather(paste0(path, "single-year/", y, ".feather"))

  # Assign MSA and FIPS codes. Process race and education variables.
  acs %<>%
    process_microdata(pull_peers = FALSE) # %>%
    # filter(age >= 25)

  # Join crosswalk to data.
  acs %<>%
    left_join(MIGPUMA_MSA,
              by = c("year", "MIGPLAC1" = "STATEFIP", "MIGPUMA1" = "MIGPUMA"))

  # Create migration variables.
  #
  # migration = 1 if the person moved MIGPUMAs.
  # (In the case of Lou, Jefferson County is one MIGPUMA).
  #
  # moved_msa = 1 if the person moved between MIGPUMAs,
  # and the person's MSA of residence (MSA) is different than their
  # previous MSA of residence.
  # (Non-peer MSAs are coded as 0 for MSA and MIGMSA)
  #
  # moved_msa = 1 if the person moved out of Jefferson County.
  #
  # https://usa.ipums.org/usa-action/variables/MIGRATE1
  acs %<>%
    mutate(
      moved = if_else(MIGRATE1D >= 24, 1, 0),
      moved = replace(moved, MIGRATE1D == 0, NA),

      MIGMSA = replace(MIGMSA, is.na(MIGMSA), 0),

      moved_MSA = if_else(moved & MIGMSA != MSA, 1, 0),

      moved_lou = if_else(moved & MIGPLAC1 == 21 & MIGPUMA1 == 1700, 1, 0)) # %>%
    #select(-MIGRATE1, MIGRATE1D, MIGPLAC1, MIGPUMA1)

  #recode education to bachelor's or higher
  acs %<>%
    mutate(
      bach_plus = if_else(educ %in% c("bach", "grad"), 1, 0),
      bach_plus = replace(bach_plus, is.na(educ), NA))

  # Create output data frames.
  #
  # Inmigration df is subset to everyone living in a peer MSA
  #
  # Outmigration df recodes MIGMSA to MSA and is subset to everyone
  # who has moved within the last year (whether within the same MIGPUMA/MSA or not)

  if(y == 2013){
    inmig_output <- acs %>%
      pull_peers_MSA()

    outmig_output <- acs %>%
      select(-MSA) %>%
      rename(MSA = MIGMSA) %>%
      pull_peers_MSA()

  } else{
    inmig <- acs %>%
      pull_peers_MSA()

    inmig_output %<>% bind_rows(inmig)

    outmig <- acs %>%
      select(-MSA) %>%
      rename(MSA = MIGMSA) %>%
      pull_peers_MSA()

    outmig_output %<>% bind_rows(outmig)
  }
}

# Shortcut to avoid waiting a long time for code above to run:
write_feather(inmig_output, path %+% "inmig_micro.feather")
write_feather(outmig_output, path %+% "outmig_micro.feather")

inmig_output <- read_feather(path %+% "inmig_micro.feather")
outmig_output <- read_feather(path %+% "outmig_micro.feather")



# Attempt to recreate ACS overall migration data

# Subset inmigration data to current Louisville residents
inmig_df <- inmig_output %>%
  filter(FIPS == 21111,
         moved == 1)

# Subset outmigration data to previous Louisville residents
outmig_df <- outmig_output %>%
  filter(moved_lou == 1)

# Create survey objects
inmig_svy <- svydesign(ids = ~0,
                       weights = ~PERWT,
                       data = inmig_df)
outmig_svy <- svydesign(ids = ~0,
                        weights = ~PERWT,
                        data = outmig_df)

# Create survey output
inmig  <- svytable(~year, design = inmig_svy)
outmig <- svytable(~year, design = outmig_svy)

inmig %<>% as.data.frame()
outmig %<>% as.data.frame()

# COMPARE TO glpdata::mig %>% filter(FIPS == 21111, year >= 2013)



# Survey output by education status
inmig_svy <- svydesign(ids = ~0,
                       weights = ~PERWT,
                       data = inmig_output)
outmig_svy <- svydesign(ids = ~0,
                        weights = ~PERWT,
                        data = outmig_output)

inmig_edu  <- svytable(~moved_MSA+MSA+year+bach_plus, design = inmig_svy)
outmig_edu <- svytable(~moved_MSA+MSA+year+bach_plus, design = outmig_svy)

inmig_edu %<>% as.data.frame()
outmig_edu %<>% as.data.frame()

inmig_edu %<>%
  spread(key = bach_plus, value = Freq) %>%
  rename(
    inmig_bach_plus = `1`) %>%
  filter(moved_MSA == 1) %>%
  select(-moved_MSA, -`0`) %>%
  mutate_at(vars(MSA, year), unfactor)

outmig_edu %<>%
  spread(key = bach_plus, value = Freq) %>%
  rename(
    outmig_bach_plus = `1`) %>%
  filter(moved_MSA == 1) %>%
  select(-moved_MSA, -`0`) %>%
  mutate_at(vars(MSA, year), unfactor)

migration_edu <- bind_df(inmig_edu, outmig_edu)

usethis::use_data(migration, overwrite = TRUE)


# Crosswalk for 2005 to 2012 is not used. Below is the code to process it.

#crosswalk_05 <- read_csv(path %+% "PUMA2000_MIGPUMA_crosswalk.csv")

#crosswalk_05 %<>%
#  transmute(
#    STATEFIP = `State code (STATEFIP)`,
#    PUMA = `PUMA (PUMA)`,
#    MIGPUMA = `PUMA of Migration (MIGPUMA or MIGPUMA1)`,
#
#    PUMA = paste0("c(", gsub("-", ":", PUMA), ")"))

#for(i in 1:nrow(crosswalk_05)){
#  df <- data.frame(
#    STATEFIP = crosswalk_05[i,]$STATEFIP,
#    PUMA = eval(parse(text = crosswalk_05[i,]$PUMA)),
#    MIGPUMA = crosswalk_05[i,]$MIGPUMA)
#  if(i == 1){
#    output <- df
#  } else {
#    output %<>% bind_rows(df)
#  }
#}

#crosswalk_05 <- output

#crosswalk_05 <- data.frame(
#  crosswalk_05,
#  year = rep(2005:2012, each = nrow(crosswalk_05)))
