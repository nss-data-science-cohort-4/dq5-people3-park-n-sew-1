library(shiny)
library(tidyverse)
library(ggplot2)
library(shinydashboard)
library(wesanderson)
library(RColorBrewer)

options(scipen = 999)

census_df_list <- readRDS('../data/census_df.rds')


age_df <- data.frame(census_df_list[1])
race_df <- data.frame(census_df_list[2])
edu_df <- data.frame(census_df_list[3])

#Transposing Dataframes
#age_df <- t(age_df)[-1,]
#race_df <- t(race_df)[-1,]
#edu_df <- t(edu_df)[-1,]

#Pivot Longer on Age
Datalong_age <- pivot_longer(age_df, cols = age_under_20_years:age_60_years_and_over, names_to = "age_group")
#View(Datalong_age)

#setting up factors so that bar chart in right order
Datalong_age$age_group <- factor(Datalong_age$age_group,levels = c("age_under_20_years", 
                                                       "age_20_29_years", 
                                                       "age_30_39_years", 
                                                       "age_40_49_years",
                                                       "age_50_59_years",
                                                       "age_60_years_and_over"))

#Pivot Longer on Race
Datalong_race <- pivot_longer(race_df, cols = white:two_or_more_races_excluding_other_and_three_or_more, names_to = "race_group")
#View(Datalong_race)

#setting up race factors so that bar chart in right order
Datalong_race$race_group <- factor(Datalong_race$race_group,levels = c("white", 
                                                                       "black_or_african_american", 
                                                                       "american_indian_or_alaska_native", 
                                                                       "asian",
                                                                       "native_hawaiian_or_pacific_islander",
                                                                       "other_race",
                                                                       "two_or_more_races",
                                                                       "two_or_more_races_including_other",
                                                                       "two_or_more_races_excluding_other_and_three_or_more"))

#Pivot Longer on Edu
Datalong_edu <- pivot_longer(edu_df, cols = no_high_school_diploma:doctorate_degree, names_to = "edu_group")

#setting up factors so that bar chart in right order
Datalong_edu$edu_group <- factor(Datalong_edu$edu_group,levels = c("no_high_school_diploma", 
                                                                   "high_school_graduate",
                                                                   "some_college_no_degree",
                                                                   "associates_degree", 
                                                                   "bachelors_degree",
                                                                   "masters_degree",
                                                                   "doctorate_degree",
                                                                   "professional_degree"))


#View(age_df)
#View(race_df)
#View(edu_df)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

colnames(data.frame(read_excel_allsheets('../data/Example_Profile2.xlsx')[2]))

#Create Color palettes
pal <- c("#FFD5B3", "#C66F00")


