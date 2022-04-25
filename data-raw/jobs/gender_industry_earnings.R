
library(censusapi)
library(tidyverse)
library(glpdata)
library(glptools)

var_df <- crossing(FIPS = unique(FIPS_df_two_stl$FIPS),
                   year = 2005:2020,
                   name = "timeseries/qwi/sa")

get_fxn <- function(data) {
  getCensus(
    name = data$name,
    vars = c("Emp", "EmpEnd", "EarnBeg", "industry", "sex"),
    region = "county:" %p% str_sub(data$FIPS, 3, 5),
    regionin = "state:" %p% str_sub(data$FIPS, 1, 2),
    time = data$year,
    key = "52e7948461b29e2ed1f7c53ceee270e6f7d8bcfe") %>%
    nest(results = everything())
}

future::plan(future::multisession)

earnings_gender = var_df %>%
  group_by(row_number()) %>%
  nest() %>%
  mutate(results = furrr::future_map_dfr(data, get_fxn)) %>%
  select(-data) %>%
  unnest(results) %>%
  unnest(results) %>%
  ungroup()

earnings_gender_intermediate <- earnings_gender %>%
  mutate(FIPS = state %p% county,
         year = as.numeric(str_sub(time, 1, 4)),
         Emp = as.numeric(Emp),
         EmpEnd = as.numeric(EmpEnd),
         EarnBeg = as.numeric(EarnBeg),
         Average_q_employment = (Emp+EmpEnd)/2,
         wages = EarnBeg * 12,
         sex = ifelse(sex == "0", "total", sex),
         sex = ifelse(sex == "1", "male", sex),
         sex = ifelse(sex == "2", "female", sex)) %>%
  na.omit() %>%
  group_by(FIPS,year, sex,industry) %>%
  summarize(jobs=mean(Average_q_employment),
            wages = mean(wages)) %>%
  spread(sex, jobs)

percent_women_industry <- earnings_gender_intermediate %>%
  select(-c(wages, male, total)) %>%
  na.omit()

percent_men_industry <- earnings_gender_intermediate %>%
  select(-c(wages, female, total)) %>%
  na.omit()

percent_total_industry <- earnings_gender_intermediate %>%
  select(-c(wages, female, male)) %>%
  na.omit()

industry_county_intermediate <- merge(percent_women_industry, percent_men_industry)
industry_county <- merge(industry_county_intermediate, percent_total_industry)

industry_county_gender <- industry_county %>%
  mutate(percent_female=(female/total)*100)

industry_labels <- read.csv('/Users/MatthewZdun/Downloads/label_industry.csv')

industry_county_gender <- merge(industry_county_gender, industry_labels)

#Now we want to see the industries that have seen the greatest increases in the share of women

industry_county_gender_percent_change_2005 <- industry_county_gender %>%
  spread(year,percent_female) %>%
  select(-c('2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019','2020')) %>%
  na.omit() %>%
  rename('percent_female_2005'='2005')

industry_county_gender_percent_change_2020 <- industry_county_gender %>%
  spread(year,percent_female) %>%
  select(-c('2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019')) %>%
  na.omit() %>%
  rename('percent_female_2020'='2020')

industry_county_gender_percent_change <- merge(industry_county_gender_percent_change_2005, industry_county_gender_percent_change_2020, by=c('FIPS','industry')) %>%
  select(c('FIPS','industry','label.x','percent_female_2005','percent_female_2020')) %>%
  rename('description'='label.x') %>%
  mutate(percentage_point_increase_women=percent_female_2020-percent_female_2005)

#Let's bring in average hourly wage data for the industries
industry_gender_earnings <- read.csv('data-raw/jobs/gender_industry_earnings/industry_earnings.csv')

industry_gender_earnings <- industry_county_gender_percent_change %>%
  left_join(industry_gender_earnings) %>%
  mutate(majority_female_2020 = case_when( #flag for majority female industries in 2020
    percent_female_2020 > 50 ~ 1,
    TRUE ~ 0)) %>%
  mutate(large_increase_female = case_when( #flag for industries that saw a large increase in the share of
    #women between 2005 and 2020 (more than 5 percentage points)
     percentage_point_increase_women > 5 ~ 1,
    TRUE ~ 0))

#Average hourly earnings in the majority-female industries in Louisville and its peer cities
average_hourly_earnings_female_industries <- industry_gender_earnings %>%
  filter(majority_female_2020==1) %>%
  group_by(FIPS) %>%
  na.omit() %>%
  summarize(mean(average_hr_earnings_feb_22)) %>%
  stl_merge(`mean(average_hr_earnings_feb_22)`, simple=T)

#Average hourly earnings in the majority-male industries in Louisville and its peer cities
average_hourly_earnings_male_industries <- industry_gender_earnings %>%
  filter(majority_female_2020==0) %>%
  group_by(FIPS) %>%
  na.omit() %>%
  summarize(mean(average_hr_earnings_feb_22)) %>%
  stl_merge(`mean(average_hr_earnings_feb_22)`, simple=T)

#Average hourly earnings in the industries that saw large increases in the share of women in Louisville and its peer cities
average_hourly_earnings_female_swing_industries <- industry_gender_earnings %>%
  filter(large_increase_female==1) %>%
  group_by(FIPS) %>%
  na.omit() %>%
  summarize(mean(average_hr_earnings_feb_22)) %>%
  stl_merge(`mean(average_hr_earnings_feb_22)`, simple=T)

#Number of industries that saw large increases in the share of women in Louisville and its peer cities
female_swing_industries <- industry_gender_earnings %>%
  filter(large_increase_female==1) %>%
  group_by(FIPS) %>%
  summarize(n()) %>%
  stl_merge(`n()`, simple=T)

gender_swing_industries_merged <- merge(average_hourly_earnings_female_swing_industries, female_swing_industries) %>%
  rename('avg_hr_earnings'='mean(average_hr_earnings_feb_22)', 'num_industries'='n()') %>%
  mutate(year=2020)

louisville_changes_women <- industry_county_gender_percent_change %>%
  filter(FIPS==21111) %>%
  arrange(desc(percent_increase_women))

##Some Visualizations
install.packages('directlabels')
library(directlabels)

#Scatterplot with the number of industries seeing large increases in the share of women vs. the average hourly earnings in those industries

#First, we need to get the GLP ranking data
scatter_df <- ranking_data(gender_swing_industries_merged, variables=c('num_industries','avg_hr_earnings'))
scatter <- ggplot(scatter_df, aes(x=avg_hr_earnings, y=num_industries)) +
  geom_point() +
  theme_minimal() +
  geom_text(aes(label=FIPS), size=2, nudge_x = 0.4, nudge_y = 0.4)

#Now for the dumbbell plot showing biggest shifts
dumbbell_df_female <- louisville_changes_women %>%
  head(n=5) %>%
  arrange(percent_increase_women)


dumbbell_df_female$description <- factor(dumbbell_df_female$description, levels=as.character(dumbbell_df_female$description))

install.packages('ggalt')
library(ggalt)

dumbbell <- ggplot(dumbbell_df_female, aes(y=description, x=percent_female_2005, xend=percent_female_2020,label=percent_female_2005)) +
  geom_dumbbell(size=3, color="#e3e2e1",
                colour_x = "light blue", colour_xend = "pink") +
  geom_text(nudge_x = -2,size=3)+
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05),panel.grid.major.y=element_blank())+
  geom_vline(xintercept = 50, color='black',linetype='dotted')

dumbbell

dumbbell_df_male <- louisville_changes_women %>%
  filter(description=='Insurance and Employee Benefit Funds'|
           description=='Textile Furnishings Mills'|
           description=='Other Information Services'|
           description=='Electronic Shopping and Mail-Order Houses'|
           description=='Data Processing, Hosting, and Related Services') %>%
  arrange(percent_increase_women)

dumbbell2 <- ggplot(dumbbell_df_male, aes(y=description, x=percent_female_2005, xend=percent_female_2020,label=percent_female_2005)) +
  geom_dumbbell(size=3, color="#e3e2e1",
                colour_x = "pink", colour_xend = "light blue") +
  geom_text(nudge_x = -2,size=3)+
  theme_minimal() +
  theme(panel.grid.major.x=element_line(size=0.05),panel.grid.major.y=element_blank())+
  geom_vline(xintercept = 50, color='black',linetype='dotted')

dumbbell2

