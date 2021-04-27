library(glptools)
glp_load_packages()

#B28002 - internet access at household level
#2017-2019 (so initial ? ask was 2013)
#this details what type of internet subscription people have, if they have internet
#access without a subscription, or if they have no internet access.
#this corresponds with CINETHH, CIHISPEED, and CIDIAL in IPUMS
internet_household   <- build_census_var_df("acs5", "B28002")


#B28001 - computer in household
#same timeline as others
compdevice_household   <- build_census_var_df("acs5", "B28001")


#B28003 - computer and internet access at houeshold level
#2017-2019 (so initial ? ask was 2013)
#Details if they have internet subscription (and what type) or not with a computer
#or if they have no computer

compinternet_household   <- build_census_var_df("acs5", "B28003")

#b28004 - internet presence and type by income
#same timeline as others

incomeinternet_household <- build_census_var_df("acs5", "B28004")

#b28005 - age by computer and internet - individual
#same timeline as others

ageinternetcomp_individual <- build_census_var_df("acs5", "B28005")


#b28006 - computer and internet by education
#same timeline as others

eduinternetcomp_individual <- build_census_var_df("acs5", "B28006")

#b28009 - computer and internet by race
#same timeline as others

raceinternetcomp_individual <- build_census_var_df("acs5", "B28009")


#process age internet - for computer and broadband acess together

age <- get_census(ageinternetcomp_individual, "tract")

test <- age_nototal %>% filter(tract == "21111000200")
test2 <- test %>% filter(age_group == "under_18")
test2 <- test2 %>% filter(var_type == "estimate")
test2 <- test %>% filter(year =="2015")

#clear out totals
totals <- c("B28005_001E","B28005_001M","B28005_002E",
            "B28005_008E", "B28005_014E","B28005_002M",
            "B28005_008M", "B28005_014M",
            "B28005_009E", "B28005_015E", "B28005_003E", "B28005_009M", "B28005_015M", "B28005_003M")

age_nototal <- age %>%
  subset(variable %not_in% totals)


#make categorical variable

age_nototal  %<>% mutate(compinternet = if_else(str_detect(label,"broadband Internet subscription"), T, F))

#^ for some reason str_detect is failing this way
#ok try it a different way

#age_nototal$compinternet <- if_else(str_detect(age_nototal$label,"broadband Internet subscription"), T, F)

#process census
clean_age <- age_nototal %>%
  process_census(cat_var = "compinternet",
                 output_name = "access", age_groups = c("under_18", "18_64", "65_plus"))


#process map
map_age <- clean_age%>%
  process_map(access_under_18, access_18_64, access_65_plus,return_name = "access_by_age") %>%
  list2env(.GlobalEnv)




mymap <- make_map(list(access_by_age_muw, access_by_age_nh, access_by_age_tract),
         var = "access_under_18",
         hover_name = "Computer and High Speed Internt Under 18",
         legend_title = "Youth with Computer and Broadband Internet",
         continuous = T)

saveWidget(mymap,
           file="/Users/mraisle/Documents/OneDrive - University of North Carolina at Chapel Hill/meganGLP/maps/18access.html")

usethis::use_data(access_by_age_muw, access_by_age_nh, access_by_age_tract, overwrite = TRUE)



