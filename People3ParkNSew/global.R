library(shiny)
library(tidyverse)
library(ggplot2)
options(scipen = 999)
library(shinydashboard)

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
View(Datalong_age)

#setting up factors so that bar chart in right order
Datalong_age$age_group <- factor(Datalong_age$age_group,levels = c("age_under_20_years", 
                                                       "age_20_29_years", 
                                                       "age_30_39_years", 
                                                       "age_40_49_years",
                                                       "age_50_59_years",
                                                       "age_60_years_and_over"))



#View(age_df)
#View(race_df)
View(edu_df)

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}
