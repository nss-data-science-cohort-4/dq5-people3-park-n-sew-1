library(shiny)
library(tidyverse)
library(ggplot2)
library(shinydashboard)

census_df_list <- readRDS('../data/census_df.rds')


age_df <- data.frame(census_df_list[1])
race_df <- data.frame(census_df_list[2])
edu_df <- data.frame(census_df_list[3])

View(age_df)
View(race_df)
View(edu_df)
