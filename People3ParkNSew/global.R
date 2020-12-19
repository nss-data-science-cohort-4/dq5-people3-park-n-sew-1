library(shiny)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(RColorBrewer)

options(scipen = 999)

#Load in Census Data
Nashville_census_df_list <- readRDS('data/census_df.rds')
Memphis_census_df_list <- readRDS('data/memphis_census_df.rds')
Seattle_census_df_list <- readRDS('data/seattle_census_df.rds')
Atlanta_census_df_list <- readRDS('data/atlanta_census_df.rds')
Boise_census_df_list <- readRDS('data/boise_census_df.rds')
Minneapolis_census_df_list <- readRDS('data/minneapolis_census_df.rds')

#Convert census data into data frames for each demographic type 
Nashville_age_df <- data.frame(Nashville_census_df_list[1])
Nashville_race_df <- data.frame(Nashville_census_df_list[2])
Nashville_edu_df <- data.frame(Nashville_census_df_list[3])

Memphis_age_df <- data.frame(Memphis_census_df_list[1])
Memphis_race_df <- data.frame(Memphis_census_df_list[2])
Memphis_edu_df <- data.frame(Memphis_census_df_list[3])

Seattle_age_df <- data.frame(Seattle_census_df_list[1])
Seattle_race_df <- data.frame(Seattle_census_df_list[2])
Seattle_edu_df <- data.frame(Seattle_census_df_list[3])

Atlanta_age_df <- data.frame(Atlanta_census_df_list[1])
Atlanta_race_df <- data.frame(Atlanta_census_df_list[2])
Atlanta_edu_df <- data.frame(Atlanta_census_df_list[3])

Boise_age_df <- data.frame(Boise_census_df_list[1])
Boise_race_df <- data.frame(Boise_census_df_list[2])
Boise_edu_df <- data.frame(Boise_census_df_list[3])

Minneapolis_age_df <- data.frame(Minneapolis_census_df_list[1])
Minneapolis_race_df <- data.frame(Minneapolis_census_df_list[2])
Minneapolis_edu_df <- data.frame(Minneapolis_census_df_list[3])


#Create functions to pivot and clean data groups
pivot_age <- function(age_df) {
  Datalong_age <- pivot_longer(age_df, cols = age_under_20_years:age_60_years_and_over, names_to = "age_group")
  Datalong_age$age_group <- factor(Datalong_age$age_group,levels = c("age_under_20_years",
                                                                     "age_20_29_years",
                                                                     "age_30_39_years",
                                                                     "age_40_49_years",
                                                                     "age_50_59_years",
                                                                     "age_60_years_and_over"))
  Datalong_age
}
pivot_race <- function(race_df) {
  Datalong_race <- pivot_longer(race_df, cols = white:two_or_more_races_excluding_other_and_three_or_more, names_to = "race_group")
  Datalong_race$race_group <- factor(Datalong_race$race_group,levels = c("white",
                                                                         "black_or_african_american",
                                                                         "american_indian_or_alaska_native",
                                                                         "asian",
                                                                         "native_hawaiian_or_pacific_islander",
                                                                         "other_race",
                                                                         "two_or_more_races",
                                                                         "two_or_more_races_including_other",
                                                                         "two_or_more_races_excluding_other_and_three_or_more"))
  Datalong_race
}
pivot_edu <- function(edu_df) {
  Datalong_edu <- pivot_longer(edu_df, cols = no_high_school_diploma:doctorate_degree, names_to = "edu_group")
  Datalong_edu$edu_group <- factor(Datalong_edu$edu_group,levels = c("no_high_school_diploma",
                                                                     "high_school_graduate",
                                                                     "some_college_no_degree",
                                                                     "associates_degree",
                                                                     "bachelors_degree",
                                                                     "masters_degree",
                                                                     "doctorate_degree",
                                                                     "professional_degree"))
  Datalong_edu
}

#Use functions to create Datalong dataframes for each city
Nashville_Datalong_age <- pivot_age(Nashville_age_df)
Nashville_Datalong_race <- pivot_race(Nashville_race_df)
Nashville_Datalong_edu <- pivot_edu(Nashville_edu_df)

Memphis_Datalong_age <- pivot_age(Memphis_age_df)
Memphis_Datalong_race <- pivot_race(Memphis_race_df)
Memphis_Datalong_edu <- pivot_edu(Memphis_edu_df)

Atlanta_Datalong_age <- pivot_age(Atlanta_age_df)
Atlanta_Datalong_race <- pivot_race(Atlanta_race_df)
Atlanta_Datalong_edu <- pivot_edu(Atlanta_edu_df)

Boise_Datalong_age <- pivot_age(Boise_age_df)
Boise_Datalong_race <- pivot_race(Boise_race_df)
Boise_Datalong_edu <- pivot_edu(Boise_edu_df)

Minneapolis_Datalong_age <- pivot_age(Minneapolis_age_df)
Minneapolis_Datalong_race <- pivot_race(Minneapolis_race_df)
Minneapolis_Datalong_edu <- pivot_edu(Minneapolis_edu_df)

Seattle_Datalong_age <- pivot_age(Seattle_age_df)
Seattle_Datalong_race <- pivot_race(Seattle_race_df)
Seattle_Datalong_edu <- pivot_edu(Seattle_edu_df)

#Write function to load in user files
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


