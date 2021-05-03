#make the county and msa documents for internet access

library(glptools)
glp_load_packages()

#read data
acs_micro <- feather::read_feather("/Users/mraisle/Documents/OneDrive - University of North Carolina at Chapel Hill/glpdata/data-raw/microdata/acs_micro_repwts.feather")

#the method i took here is a bit long winded .  . . case_when worked best to get the binary
#that I wanted for some variables but T,F was how I wanted it evaluated for survey_by_demog, hence this approach
acs_micro_internet <- acs_micro %>%
  filter(year > 2012) %>%
  mutate(
    MSA = as.character(MSA),
    int_acc = if_else(CINETHH == 1 | CINETHH == 2, T, F),
    int_acc = replace(int_acc, CINETHH == 0, NA),

    hspd_int = case_when(
        CINETHH == 0 ~ NA_real_,
        CINETHH == 3 & CIHISPEED == 0 ~ 0, #seems like it's marked NA if the household doesn't have internet access. We want it to be no High Speed access
        CIHISPEED > 9 & CIHISPEED < 20 ~ 1,
        CIHISPEED == 20 ~ 0
      ),
    computer = if_else(CILAPTOP == 1, T, F),
    computer = replace(computer, CILAPTOP == 0, NA),

    tablet = if_else(CITABLET == 1, T, F),
    tablet = replace(tablet, CITABLET == 0, NA),

    smrtphone = if_else(CISMRTPHN == 1, T, F),
    smrtphone = replace(smrtphone, CISMRTPHN == 0, NA),

    #ok so below is if someone has a computer or tablet at home
    comp_tab = case_when(
      computer == 1 | tablet == 1 ~ 1,
      computer == 0 & tablet == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    #ok so below is if someone has a computer,tablet, or smartphone
    comp_tab_smrt = case_when(
      computer == 1 | tablet == 1 | smrtphone == 1 ~ 1,
      computer == 0 & tablet == 0 & smrtphone == 0 ~ 0,
      TRUE ~ NA_real_),
    #this is seeing if someone has a computer or tablet and high speed internet
    internet_and_device = case_when(
      comp_tab == 1 & hspd_int == 1 ~ 1,
      comp_tab == 0 | hspd_int == 0 ~ 0,
      TRUE ~ NA_real_),
  )

#convert necessary ones to true false
acs_micro_internet$internet_and_device %<>%
  as.logical(acs_micro_internet$internet_and_device)
acs_micro_internet$comp_tab_smrt %<>%
  as.logical(acs_micro_internet$comp_tab_smrt)
acs_micro_internet$comp_tab %<>%
  as.logical(acs_micro_internet$comp_tab)
acs_micro_internet$hspd_int %<>%
  as.logical(acs_micro_internet$hspd_int)


#internet access
internetaccess_county  <- survey_by_demog(acs_micro_internet, "int_acc")
internetaccess_MSA5yr  <- survey_by_demog(acs_micro_internet, "int_acc",geog = "MSA")
#high speed internet access
hispdaccess_county  <- survey_by_demog(acs_micro_internet, "hspd_int")
hispdaccess_MSA5yr  <- survey_by_demog(acs_micro_internet, "hspd_int",geog = "MSA")
#computer or tablet access
comptab_county  <- survey_by_demog(acs_micro_internet, "comp_tab")
comptab_MSA5yr  <- survey_by_demog(acs_micro_internet, "comp_tab",geog = "MSA")
#computer, smartphone, or tablet access
comptabsmrt_county  <- survey_by_demog(acs_micro_internet, "comp_tab_smrt")
comptabsmrt_MSA5yr  <- survey_by_demog(acs_micro_internet, "comp_tab_smrt",geog = "MSA")
#internet_and_device
internetanddevice_county  <- survey_by_demog(acs_micro_internet, "internet_and_device")
internetanddevice_MSA5yr  <- survey_by_demog(acs_micro_internet, "internet_and_device", geog = "MSA")

#merge all county together:
mergeCols <- c("FIPS","year","sex","race","var_type")

digitalaccess_county <- inner_join(internetaccess_county, hispdaccess_county,
                           by = mergeCols)
digitalaccess_county %<>% inner_join(comptab_county,
                                   by = mergeCols)
digitalaccess_county %<>% inner_join(comptabsmrt_county,
                                     by = mergeCols)
digitalaccess_county %<>% inner_join(internetanddevice_county,
                                     by = mergeCols)

#merge all MSA together:
MSACols <- c("MSA","year","sex","race","var_type")

digitalaccess_MSA5yr <- inner_join(internetaccess_MSA5yr, hispdaccess_MSA5yr,
                                   by = MSACols)
digitalaccess_MSA5yr %<>% inner_join(comptab_MSA5yr,
                                     by = MSACols)
digitalaccess_MSA5yr %<>% inner_join(comptabsmrt_MSA5yr,
                                     by = MSACols)
digitalaccess_MSA5yr %<>% inner_join(internetanddevice_MSA5yr,
                                     by = MSACols)

usethis::use_data(digitalaccess_county, digitalaccess_MSA5yr, overwrite = TRUE)


#data frames to look at/test
look <- acs_micro_internet %>% filter(FIPS == "37119")
look2 <- acs_micro_internet2 %>% filter(FIPS == "37119")
look <- acs_micro_internet %>% filter(year == "2018")
look %<>%
  select(int_acc, internet_and_device, comp_tab_smrt, everything())
acs_micro_2012 <- acs_micro_internet %>% filter(year == 2012)
