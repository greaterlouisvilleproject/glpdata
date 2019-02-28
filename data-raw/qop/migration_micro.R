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

#crosswalk_05 <- output

#crosswalk_05 <- data.frame(
#  crosswalk_05,
#  year = rep(2005:2012, each = nrow(crosswalk_05)))


# 1. Start with crosswalks from PUMAs to MIGPUMAs.
#    The crosswalk uses PUMA definitions put into use in 2012.
#    Code for earlier definitions is commented out at the bottom.


# The 2005 crosswalk contains ranges in individual cells.
# These ranges are expanded across multiple rows.

crosswalk_05 <- read_csv(path %+% "PUMA2000_MIGPUMA_crosswalk.csv")

crosswalk_05 %<>%
  transmute(
    STATEFIP = `State code (STATEFIP)`,
    PUMA = `PUMA (PUMA)`,
    PUMA = paste0("c(", gsub("-", ":", PUMA), ")"),
    MIGPUMA = `PUMA of Migration (MIGPUMA or MIGPUMA1)`)

for(i in 1:nrow(crosswalk_05)){
  df <- data.frame(
    STATEFIP = crosswalk_05[i,]$STATEFIP,
    PUMA = eval(parse(text = crosswalk_05[i,]$PUMA)),
    MIGPUMA = crosswalk_05[i,]$MIGPUMA)
  if(i == 1){
    output <- df
  } else {
    output %<>% bind_rows(df)
  }
}

crosswalk_05 <- output


crosswalk_12 <- read_csv(path %+% "puma_migpuma1_pwpuma00.csv", skip = 2, n_max = 2378)

crosswalk_12 %<>%
  transmute(
    STATEFIP = `State of Residence (ST)`,
    PUMA,
    MIGPUMA = `PWPUMA00 or MIGPUMA1`)

# replicate the crosswalk for each year

crosswalk_05 <- data.frame(
  crosswalk_05,
  year = rep(2005:2011, each = nrow(crosswalk_05)))

crosswalk_12 <- data.frame(
  crosswalk_12,
  year = rep(2012:2017, each = nrow(crosswalk_12)))

PUMA_MIGPUMA <- bind_rows(crosswalk_05, crosswalk_12)


# 2. Crosswalk from PUMAs to MSAs exists in the glptools package

MSA_PUMA <- MSA_PUMA %>% filter(year >= 2005)

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


for(y in 2005:2017){
  acs <- read_feather(paste0(path, "single-year/", y, ".feather"))

  # Assign MSA and FIPS codes. Process race and education variables.
  acs %<>%
    process_microdata(pull_peers = FALSE)  %>%
    select(-SERIAL, -HHWT, -PERNUM) %>%
    filter(age >= 25)

  # Join crosswalk to data.
  acs %<>%
    left_join(MIGPUMA_MSA,
              by = c("year", "MIGPLAC1" = "STATEFIP", "MIGPUMA1" = "MIGPUMA"))

  # Create migration variables.
  #
  # moved_MIGPUMA
  #   moved_MIGPUMA = 1 if the person moved between MIGPUMAs.
  #     (In the case of Lou, Jefferson County is one MIGPUMA).
  #   moved_MIGPUMA = 0 if the person did not move MIGPUMAs.
  #   moved_MIGPUMA = NA if the person is less than one year old.
  #
  # moved_MSA
  #   moved_MSA = 1 if the person moved between MSAs, one or both of which is a peer.
  #     (Non-peer MSAs are coded as 0 for MSA and MIGMSA. People who move from a non-peer MSA
  #     to another non-peer MSA are not captured in moved_MSA, though they are not relevant for
  #     the analysis.)
  #
  # suburbanized
  #   suburbanized = 1 if the person moved from the core county to an outer county within the MSA.
  #   (Only verified for Louisville.)
  #
  # urbanized
  #   burbanized = 1 if the person moved form an outer county within the MSA to the core county.
  #   (Only verified for Louisville.)
  #
  # https://usa.ipums.org/usa-action/variables/MIGRATE1
  acs %<>%
    mutate(
      moved_MIGPUMA = if_else(MIGRATE1D >= 24, 1, 0),
      moved_MIGPUMA = replace(moved_MIGPUMA, MIGRATE1D == 0, NA),

      MIGMSA = replace(MIGMSA, is.na(MIGMSA), 0),

      moved_MSA = if_else(moved_MIGPUMA == 1 & MIGMSA != MSA, 1, 0),

      suburbanized = if_else(moved_MIGPUMA == 1 & moved_MSA == 0 & is.na(FIPS), 1, 0),
      urbanized    = if_else(moved_MIGPUMA == 1 & moved_MSA == 0 & !is.na(FIPS), 1, 0)) %>%
    select(-MIGRATE1, -MIGRATE1D, -MIGPLAC1, -MIGPUMA1)

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

  if(y == 2005){
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

# Subset to people who moved MSA
inmig_output %<>%
  filter(moved_MSA == 1)

outmig_output %<>%
  filter(moved_MSA == 1)

# Create survey objects
inmig_svy <- svydesign(ids = ~0,
                       weights = ~PERWT,
                       data = inmig_output)
outmig_svy <- svydesign(ids = ~0,
                        weights = ~PERWT,
                        data = outmig_output)

# Create survey output
inmig  <- svytable(~year+MSA+bach_plus, design = inmig_svy)
outmig <- svytable(~year+MSA+bach_plus, design = outmig_svy)


# Clean data frames
inmig %<>% as.data.frame(stringsAsFactors = FALSE)
outmig %<>% as.data.frame(stringsAsFactors = FALSE)

inmig %<>%
  rename(in_mig_msa_bach_plus = Freq)

outmig %<>%
  rename(out_mig_msa_bach_plus = Freq)

mig_MSA <- full_join(inmig, outmig, by = c("year", "MSA", "bach_plus"))

mig_MSA %<>%
  filter(bach_plus == 1) %>%
  select(-bach_plus) %>%
  mutate(
    year = as.numeric(year),
    net_mig_msa_bach_plus = in_mig_msa_bach_plus - out_mig_msa_bach_plus)

usethis::use_data(mig_MSA, overwrite = TRUE)



# Calculate "suburbanization flows"
inmig_output <- read_feather(path %+% "inmig_micro.feather")
outmig_output <- read_feather(path %+% "outmig_micro.feather")

inmig_output %<>% filter(MSA == 31140, urbanized == TRUE)
outmig_output %<>% filter(MSA == 31140, suburbanized == TRUE)

# Create survey objects
inmig_svy <- svydesign(ids = ~0,
                       weights = ~PERWT,
                       data = inmig_output)
outmig_svy <- svydesign(ids = ~0,
                        weights = ~PERWT,
                        data = outmig_output)

# Create survey output
inmig  <- svytable(~year+bach_plus, design = inmig_svy)
outmig <- svytable(~year+bach_plus, design = outmig_svy)

# Clean data frames
inmig %<>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  rename(in_mig = Freq)

outmig %<>%
  as.data.frame(stringsAsFactors = FALSE) %>%
  rename(out_mig = Freq)

lou_mig_all <- full_join(inmig, outmig, by = c("year", "bach_plus")) %>%
  group_by(year) %>%
  summarise(
    urban    = sum(in_mig),
    suburban = sum(out_mig)) %>%
  mutate(
    year = as.numeric(year),
    net_suburb = suburban - urban) %>%
  mutate_at(vars(urban:net_suburb), funs(rollmeanr), 3) %>%
  filter(!is.na(urban)) %>%
  gather(urban:net_suburb, key = "key", value = "value") %>%
  mutate(
    key = factor(key,
           levels = c("urban", "suburban", "net_suburb"),
           labels = c("into Jeff Co", "out of Jeff Co", "Net Suburbs")))


lou_mig_bach <- full_join(inmig, outmig, by = c("year", "bach_plus")) %>%
  filter(bach_plus == 1) %>%
  select(-bach_plus) %>%
  rename(
    urban_bach    = in_mig,
    suburban_bach = out_mig) %>%
  mutate(
    year = as.numeric(year),
    net_suburb_bach = suburban_bach - urban_bach) %>%
  mutate_at(vars(urban_bach:net_suburb_bach), funs(rollmeanr), 3) %>%
  filter(!is.na(urban_bach)) %>%
  gather(urban_bach:net_suburb_bach, key = "key", value = "value") %>%
  mutate(
    key = factor(key,
                 levels = c("urban_bach", "suburban_bach", "net_suburb_bach"),
                 labels = c("into Jeff Co", "out of Jeff Co", "Net Suburbs")))


g <- ggplot(lou_mig_all,
       aes(x = year, y = value, group = key, color = key)) +
  geom_line(size = 2) +
  ylim(c(0, 10000))


g %<>% glptools:::tl_style(plot_title = "Net suburbanization", y_title = "residents",
                           caption_text = "", subtitle_text = "", cat_names = "")

g <- g + theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
         guides(color = guide_legend(title = "Moved:"))




g <- ggplot(lou_mig_bach,
            aes(x = year, y = value, group = key, color = key)) +
  geom_line(size = 2) +
  ylim(c(-500, 2500))


g %<>% glptools:::tl_style(plot_title = "Net suburbanization, bachelor's +", y_title = "residents",
                           caption_text = "", subtitle_text = "", cat_names = "")

g <- g + theme(legend.position = "right", legend.text = element_text(size = 12), legend.title = element_text(size = 12)) +
  guides(color = guide_legend(title = "Moved:"))




  rename(
    `0`,
    `1`
  ))
  filter(bach_plus == 1) %>%
  select(-bach_plus) %>%
  mutate(
    year = as.numeric(year),
    net_mig = in_mig - out_mig)





library(ggplot2)
library(scales)

trend_single(mig_MSA_graph, net_mig,
             rollmean = 3, xmin = 2006, plot_title = "Migration, Bachelor's +, MSA level")

trend_single(glpdata::mig %>% pull_peers_FIPS, net_mig_bach_plus, rollmean = 3, xmin = 2014,
             plot_title = "Migration, Bachelor's +, County")


# Suburbanization??





