shinyServer(function(input, output, session) {

  #Create reactive elements, which will be used to create unique names for DataFrames, respective to user input for city    
  age_df <- reactive({
    paste0(input$usercity,"_age_df")
  })
  edu_df <- reactive({
    paste0(input$usercity,"_edu_df")
  })
  race_df <- reactive({
    paste0(input$usercity,"_race_df")
  })
  Datalong_age <- reactive({
    paste0(input$usercity,"_Datalong_age")
  })
  Datalong_race <- reactive({
    paste0(input$usercity,"_Datalong_race")
  })
  Datalong_edu <- reactive({
    paste0(input$usercity,"_Datalong_edu")
  })
  
    
    #******** USER INPUT TAB ***********
  
    #Making a table for each user input sheet 
    output$downloadData <- downloadHandler(
      filename = 'User_Company_Template.xlsx',
      content = function(file) {
        file.copy(from = 'Example_Profile_Download.xlsx', to = file)
        #command = paste('cp duck.jpg', file)
        #system(command)
      })
    
    
    
    
    
    #******** RACE TAB ***********
 
    #Your City Race Pie Chart
    output$nash_race_pie <- renderPlot({
      pie_race_data <- data.frame(
        group=c('Not Hispanic or Latino','Hispanic or Latino'),
        value=c(eval(as.name(race_df()))[2,2], eval(as.name(race_df()))[3,2])
      )
      
      pie_race_data <- pie_race_data %>% 
        arrange(desc(group)) %>%
        mutate(prop = value / sum(pie_race_data$value) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop)
      
      ggplot(pie_race_data, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        labs(title = paste("Percent Hispanic or Latino in", input$usercity)) +
        theme(plot.title = element_text(hjust = 0.5, size = 14,face="bold")) +
        scale_fill_manual(values=c("#FFD5B3", "#C66F00"))
    })
    
    #Company Race Pie Chart
    output$company_race_pie <- renderPlot({
      user_data_race <- read_excel_allsheets(input$user_file$datapath)[2]
      user_data_race <- data.frame(user_data_race)
      
      pie_race_data <- data.frame(
        group=c('Not Hispanic or Latino','Hispanic or Latino'),
        value=c(user_data_race[2,2],user_data_race[3,2])
      )
      
      pie_race_data <- pie_race_data %>% 
        arrange(desc(group)) %>%
        mutate(prop = value / sum(pie_race_data$value) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop)
      
      ggplot(pie_race_data, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        labs(title = "Percent Hispanic or Latino in Your Company") +
        theme(plot.title = element_text(hjust = 0.5, size = 14,face="bold")) +
        scale_fill_manual(values=c("#FFD5B3", "#C66F00"))
    })
      
    
    #Your City df bar charts for race
    #Three plots - Total, Non-Hispanic or Latino, Hispanic or Latino
    output$race_total_plot <- renderPlot({
      ggplot(data = eval(as.name(Datalong_race()))[1:9,]) + 
        geom_col(aes(x = race_group, y = value, fill=value)) +
        labs(title = paste("Number of People per Race and Ethnicity in", input$usercity), 
             x = "Race and Ethnicity Groups", y = "Total") +
        scale_x_discrete(labels=c("white" = "White", 
                                  "black_or_african_american" = "Black or African American", 
                                  "american_indian_or_alaska_native" = "American Indian or Alaska Native", 
                                  "asian" = "Asian",
                                  "native_hawaiian_or_pacific_islander" = "Native Hawaiian or Pacific Islander",
                                  "other_race" = "Other Race",
                                  "two_or_more_races" = "Two or More Races",
                                  "two_or_more_races_including_other" = "Two or More Races (inc. 'Other')",
                                  "two_or_more_races_excluding_other_and_three_or_more" = "Two and Three or More Races")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })

    output$not_hispanic_plot <- renderPlot({
      ggplot(data = eval(as.name(Datalong_race()))[10:18,]) + 
        geom_col(aes(x = race_group, y = value, fill=value)) +
        labs(title = paste("Race of Non-Hispanic or Latino in", input$usercity), 
             x = "Race", y = "Total") +
        scale_x_discrete(labels=c("white" = "White", 
                                  "black_or_african_american" = "Black or African American", 
                                  "american_indian_or_alaska_native" = "American Indian or Alaska Native", 
                                  "asian" = "Asian",
                                  "native_hawaiian_or_pacific_islander" = "Native Hawaiian or Pacific Islander",
                                  "other_race" = "Other Race",
                                  "two_or_more_races" = "Two or More Races",
                                  "two_or_more_races_including_other" = "Two or More Races (inc. 'Other')",
                                  "two_or_more_races_excluding_other_and_three_or_more" = "Two and Three or More Races")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    output$hispanic_plot <- renderPlot({
      ggplot(data = eval(as.name(Datalong_race()))[19:27,]) + 
        geom_col(aes(x = race_group, y = value, fill=value)) +
        labs(title = paste("Race of Hispanic or Latino in", input$usercity), 
             x = "Race", y = "Total") +
        scale_x_discrete(labels=c("white" = "White", 
                                  "black_or_african_american" = "Black or African American", 
                                  "american_indian_or_alaska_native" = "American Indian or Alaska Native", 
                                  "asian" = "Asian",
                                  "native_hawaiian_or_pacific_islander" = "Native Hawaiian or Pacific Islander",
                                  "other_race" = "Other Race",
                                  "two_or_more_races" = "Two or More Races",
                                  "two_or_more_races_including_other" = "Two or More Races (inc. 'Other')",
                                  "two_or_more_races_excluding_other_and_three_or_more" = "Two and Three or More Races")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    #Making side by side bar chart comparing age 
    #Three plots - Total, Non-Hispanic or Latino, Hispanic or Latino
    output$user_race_total<- renderPlot({
      #making data frame
      user_data_race <- read_excel_allsheets(input$user_file$datapath)[2]
      user_data_race <- data.frame(user_data_race)
      
      #pivot longer
      user_data_race <- pivot_longer(user_data_race, cols = Race.White.alone:Race.Two.races.excluding.Some.other.race..and.three.or.more.races, names_to = "race_group")
      
      #setting factors so bar chart in right order
      user_data_race$race_group <- factor(user_data_race$race_group,levels = c("Race.White.alone", 
                                                                     "Race.Black.or.African.American", 
                                                                     "Race.American.Indian.and.Alaska.Native", 
                                                                     "Race.Asian",
                                                                     "Race.Native.Hawaiian.and.Other.Pacific.Islander",
                                                                     "Race.Some.other.race",
                                                                     "Race.Two.or.more.races.",
                                                                     "Race.Two.races.including.Some.other.race",
                                                                     "Race.Two.races.excluding.Some.other.race..and.three.or.more.races"
                                                                     ))
      #plotting bar chart
      ggplot(data = user_data_race[1:9,]) + 
        geom_col(aes(x = race_group, y = value, fill=value)) +
        labs(title = "Number of People per Race for Your Company", 
             x = "Race", y = "Total") +
        scale_x_discrete(labels=c("Race.White.alone" = "White", 
                                  "Race.Black.or.African.American" = "Black or African American", 
                                  "Race.American.Indian.and.Alaska.Native" = "American Indian or Alaska Native", 
                                  "Race.Asian" = "Asian",
                                  "Race.Native.Hawaiian.and.Other.Pacific.Islander" = "Native Hawaiian or Pacific Islander",
                                  "Race.Some.other.race" = "Other Race",
                                  "Race.Two.or.more.races." = "Two or More Races",
                                  "Race.Two.races.including.Some.other.race" = "Two or More Races (inc. 'Other')",
                                  "Race.Two.races.excluding.Some.other.race..and.three.or.more.races" = "Two and Three or More Races")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    output$user_not_hispanic<- renderPlot({
      #making data frame
      user_data_race <- read_excel_allsheets(input$user_file$datapath)[2]
      user_data_race <- data.frame(user_data_race)
      
      #pivot longer
      user_data_race <- pivot_longer(user_data_race, cols = Race.White.alone:Race.Two.races.excluding.Some.other.race..and.three.or.more.races, names_to = "race_group")
      
      #setting factors so bar chart in right order
      user_data_race$race_group <- factor(user_data_race$race_group,levels = c("Race.White.alone", 
                                                                             "Race.Black.or.African.American", 
                                                                             "Race.American.Indian.and.Alaska.Native", 
                                                                             "Race.Asian",
                                                                             "Race.Native.Hawaiian.and.Other.Pacific.Islander",
                                                                             "Race.Some.other.race",
                                                                             "Race.Two.or.more.races.",
                                                                             "Race.Two.races.including.Some.other.race",
                                                                             "Race.Two.races.excluding.Some.other.race..and.three.or.more.races"))
      #plotting bar chart
      ggplot(data = user_data_race[10:18,]) + 
        geom_col(aes(x = race_group, y = value, fill=value)) +
        labs(title = "Race of Non-Hispanic or Latino for Your Company", 
             x = "Race", y = "Total") +
        scale_x_discrete(labels=c("Race.White.alone" = "White", 
                                  "Race.Black.or.African.American" = "Black or African American", 
                                  "Race.American.Indian.and.Alaska.Native" = "American Indian or Alaska Native", 
                                  "Race.Asian" = "Asian",
                                  "Race.Native.Hawaiian.and.Other.Pacific.Islander" = "Native Hawaiian or Pacific Islander",
                                  "Race.Some.other.race" = "Other Race",
                                  "Race.Two.or.more.races." = "Two or More Races",
                                  "Race.Two.races.including.Some.other.race" = "Two or More Races (inc. 'Other')",
                                  "Race.Two.races.excluding.Some.other.race..and.three.or.more.races" = "Two and Three or More Races")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    
    output$user_hispanic<- renderPlot({
      #making data frame
      user_data_race <- read_excel_allsheets(input$user_file$datapath)[2]
      user_data_race <- data.frame(user_data_race)
      
      #pivot longer
      user_data_race <- pivot_longer(user_data_race, cols = Race.White.alone:Race.Two.races.excluding.Some.other.race..and.three.or.more.races, names_to = "race_group")
      
      #setting factors so bar chart in right order
      user_data_race$race_group <- factor(user_data_race$race_group,levels = c("Race.White.alone", 
                                                                             "Race.Black.or.African.American", 
                                                                             "Race.American.Indian.and.Alaska.Native", 
                                                                             "Race.Asian",
                                                                             "Race.Native.Hawaiian.and.Other.Pacific.Islander",
                                                                             "Race.Some.other.race",
                                                                             "Race.Two.or.more.races.",
                                                                             "Race.Two.races.including.Some.other.race",
                                                                             "Race.Two.races.excluding.Some.other.race..and.three.or.more.races"))
      #plotting bar chart
      ggplot(data = user_data_race[19:27,]) + 
        geom_col(aes(x = race_group, y = value, fill=value)) +
        labs(title = "Race of Hispanic or Latino for Your Company", 
             x = "Race", y = "Total") +
        scale_x_discrete(labels=c("Race.White.alone" = "White", 
                                  "Race.Black.or.African.American" = "Black or African American", 
                                  "Race.American.Indian.and.Alaska.Native" = "American Indian or Alaska Native", 
                                  "Race.Asian" = "Asian",
                                  "Race.Native.Hawaiian.and.Other.Pacific.Islander" = "Native Hawaiian or Pacific Islander",
                                  "Race.Some.other.race" = "Other Race",
                                  "Race.Two.or.more.races." = "Two or More Races",
                                  "Race.Two.races.including.Some.other.race" = "Two or More Races (inc. 'Other')",
                                  "Race.Two.races.excluding.Some.other.race..and.three.or.more.races" = "Two and Three or More Races")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })   
    
    
    
    
    
    
    
    
    
    
    
    #******** EDUCATION TAB ***********
    
    
    #Your City Age Pie Chart
    output$nash_edu_pie <- renderPlot({
      pie_edu_data <- data.frame(
        group=c('Male','Female'),
        value=c(eval(as.name(edu_df()))[2,2], eval(as.name(edu_df()))[3,2])
      )
      
      pie_edu_data <- pie_edu_data %>% 
        arrange(desc(group)) %>%
        mutate(prop = value / sum(pie_edu_data$value) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop)
      
      ggplot(pie_edu_data, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        labs(title = paste("Percent Male and Female in", input$usercity)) +
        theme(plot.title = element_text(hjust = 0.5, size = 14,face="bold")) +
        scale_fill_manual(values=c("#FFD5B3", "#C66F00"))
      #theme(legend.position = "none") +
      #geom_text(aes(label = prop), color = "white", size=6)
    })
    
    
    
    #Company Age Pie Chart
    output$company_edu_pie <- renderPlot({
      user_data_edu <- read_excel_allsheets(input$user_file$datapath)[3]
      user_data_edu <- data.frame(user_data_edu)
      
      pie_edu_data <- data.frame(
        group=c('Male','Female'),
        value=c(user_data_edu[2,2],user_data_edu[3,2])
      )
      
      pie_edu_data <- pie_edu_data %>% 
        arrange(desc(group)) %>%
        mutate(prop = value / sum(pie_edu_data$value) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop)
     
      
      ggplot(pie_edu_data, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        labs(title = "Percent Male and Female in Your Company") +
        theme(plot.title = element_text(hjust = 0.5, size = 14,face="bold")) +
        scale_fill_manual(values=c("#FFD5B3", "#C66F00"))
      #theme(legend.position = "none") +
      #geom_text(aes(label = prop), color = "white", size=6)
    })
    
    
    
    #Your City df bar charts for edu
    output$edu_total_plot <- renderPlot({
      ggplot(data = eval(as.name(Datalong_edu()))[1:8,]) + 
        geom_col(aes(x = edu_group, y = value, fill=value)) +
        labs(title = paste("Number of People per Education Level in", input$usercity), 
             x = "Education Level", y = "Total") +
        scale_x_discrete(labels=c("no_high_school_diploma" = "No High School Diploma", 
                                  "high_school_graduate" = "High School Graduate",
                                  "some_college_no_degree" = "Some College",
                                  "associates_degree" = "Associate Degree", 
                                  "bachelors_degree" = "Bachelor's Degree",
                                  "masters_degree" = "Master's Degree",
                                  "professional_degree" = "Professional Degree",
                                  "doctorate_degree" = "Doctorate Degree")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    output$edu_male_plot <- renderPlot({
      ggplot(data = eval(as.name(Datalong_edu()))[9:16,]) + 
        geom_col(aes(x = edu_group, y = value, fill=value)) +
        labs(title = paste("Number of Males per Education Level in", input$usercity), 
             x = "Education Level", y = "Total") +
        scale_x_discrete(labels=c("no_high_school_diploma" = "No High School Diploma", 
                                  "high_school_graduate" = "High School Graduate",
                                  "some_college_no_degree" = "Some College",
                                  "associates_degree" = "Associate Degree", 
                                  "bachelors_degree" = "Bachelor's Degree",
                                  "masters_degree" = "Master's Degree",
                                  "professional_degree" = "Professional Degree",
                                  "doctorate_degree" = "Doctorate Degree")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    output$edu_female_plot <- renderPlot({
      ggplot(data = eval(as.name(Datalong_edu()))[17:24,]) + 
        geom_col(aes(x = edu_group, y = value, fill=value)) +
        labs(title = paste("Number of Females per Education Level in", input$usercity), 
             x = "Education Level", y = "Total") +
        scale_x_discrete(labels=c("no_high_school_diploma" = "No High School Diploma", 
                                  "high_school_graduate" = "High School Graduate",
                                  "some_college_no_degree" = "Some College",
                                  "associates_degree" = "Associate Degree", 
                                  "bachelors_degree" = "Bachelor's Degree",
                                  "masters_degree" = "Master's Degree",
                                  "professional_degree" = "Professional Degree",
                                  "doctorate_degree" = "Doctorate Degree")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    #Making side by side bar chart comparing age 
    output$user_edu_total<- renderPlot({
      #making data frame
      user_data_edu <- read_excel_allsheets(input$user_file$datapath)[3]
      user_data_edu <- data.frame(user_data_edu)
      
      #pivot longer
      user_data_edu <- pivot_longer(user_data_edu, cols = Education.No.High.School.Diploma:Education.Doctorate.degree, names_to = "edu_group")
      
      #setting factors so bar chart in right order
      user_data_edu$edu_group <- factor(user_data_edu$edu_group,levels = c("Education.No.High.School.Diploma", 
                                                                           "Education.High.school.graduate..includes.equivalency.",
                                                                           "Education.Some.college..no.degree",
                                                                           "Education.Associate.s.degree", 
                                                                           "Education.Bachelor.s.degree",
                                                                           "Education.Master.s.degree",
                                                                           "Education.Professional.school.degree",
                                                                           "Education.Doctorate.degree"))
      #plotting bar chart
      ggplot(data = user_data_edu[1:8,]) + 
        geom_col(aes(x = edu_group, y = value, fill=value)) +
        labs(title = "Number of People per Education Level for Your Company", 
             x = "Education Level", y = "Total") +
        scale_x_discrete(labels=c("Education.No.High.School.Diploma" = "No High School Diploma", 
                                  "Education.High.school.graduate..includes.equivalency." = "High School Graduate",
                                  "Education.Some.college..no.degree" = "Some College",
                                  "Education.Associate.s.degree" = "Associate Degree", 
                                  "Education.Bachelor.s.degree" = "Bachelor's Degree",
                                  "Education.Master.s.degree" = "Master's Degree",
                                  "Education.Professional.school.degree" = "Professional Degree",
                                  "Education.Doctorate.degree" = "Doctorate Degree")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    output$user_edu_male<- renderPlot({
      #making data frame
      user_data_edu <- read_excel_allsheets(input$user_file$datapath)[3]
      user_data_edu <- data.frame(user_data_edu)
      
      #pivot longer
      user_data_edu <- pivot_longer(user_data_edu, cols = Education.No.High.School.Diploma:Education.Doctorate.degree, names_to = "edu_group")
      
      #setting factors so bar chart in right order
      user_data_edu$edu_group <- factor(user_data_edu$edu_group,levels = c("Education.No.High.School.Diploma", 
                                                                           "Education.High.school.graduate..includes.equivalency.",
                                                                           "Education.Some.college..no.degree",
                                                                           "Education.Associate.s.degree", 
                                                                           "Education.Bachelor.s.degree",
                                                                           "Education.Master.s.degree",
                                                                           "Education.Professional.school.degree",
                                                                           "Education.Doctorate.degree"))
      #plotting bar chart
      ggplot(data = user_data_edu[9:16,]) + 
        geom_col(aes(x = edu_group, y = value, fill=value)) +
        labs(title = "Number of Males per Education Level for Your Company", 
             x = "Education Level", y = "Total") +
        scale_x_discrete(labels=c("Education.No.High.School.Diploma" = "No High School Diploma", 
                                  "Education.High.school.graduate..includes.equivalency." = "High School Graduate",
                                  "Education.Some.college..no.degree" = "Some College",
                                  "Education.Associate.s.degree" = "Associate Degree", 
                                  "Education.Bachelor.s.degree" = "Bachelor's Degree",
                                  "Education.Master.s.degree" = "Master's Degree",
                                  "Education.Professional.school.degree" = "Professional Degree",
                                  "Education.Doctorate.degree" = "Doctorate Degree")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    
    output$user_edu_female<- renderPlot({
      #making data frame
      user_data_edu <- read_excel_allsheets(input$user_file$datapath)[3]
      user_data_edu <- data.frame(user_data_edu)
      
      #pivot longer
      user_data_edu <- pivot_longer(user_data_edu, cols = Education.No.High.School.Diploma:Education.Doctorate.degree, names_to = "edu_group")
      
      #setting factors so bar chart in right order
      user_data_edu$edu_group <- factor(user_data_edu$edu_group,levels = c("Education.No.High.School.Diploma", 
                                                                           "Education.High.school.graduate..includes.equivalency.",
                                                                           "Education.Some.college..no.degree",
                                                                           "Education.Associate.s.degree", 
                                                                           "Education.Bachelor.s.degree",
                                                                           "Education.Master.s.degree",
                                                                           "Education.Professional.school.degree",
                                                                           "Education.Doctorate.degree"))
      #plotting bar chart
      ggplot(data = user_data_edu[17:24,]) + 
        geom_col(aes(x = edu_group, y = value, fill=value)) +
        labs(title = "Number of Females per Education Level for Your Company", 
             x = "Education Level", y = "Total") +
        scale_x_discrete(labels=c("Education.No.High.School.Diploma" = "No High School Diploma", 
                                  "Education.High.school.graduate..includes.equivalency." = "High School Graduate",
                                  "Education.Some.college..no.degree" = "Some College",
                                  "Education.Associate.s.degree" = "Associate Degree", 
                                  "Education.Bachelor.s.degree" = "Bachelor's Degree",
                                  "Education.Master.s.degree" = "Master's Degree",
                                  "Education.Professional.school.degree" = "Professional Degree",
                                  "Education.Doctorate.degree" = "Doctorate Degree")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    
    
    
    
    
    
    
    
    
    
    #******** AGE TAB ***********
    
    #Your City Age Pie Chart
    output$nash_age_pie <- renderPlot({
        pie_data <- data.frame(
            group=c('Male','Female'),
            value=c(eval(as.name(age_df()))[2,2], eval(as.name(age_df()))[3,2])
        )
        
        pie_data <- pie_data %>% 
            arrange(desc(group)) %>%
            mutate(prop = value / sum(pie_data$value) *100) %>%
            mutate(ypos = cumsum(prop)- 0.5*prop)
        
        ggplot(pie_data, aes(x="", y=value, fill=group)) +
            geom_bar(stat="identity", width=1) +
            coord_polar("y", start=0) +
            theme_void() +
            labs(title = paste("Percent Male and Female in", input$usercity)) +
          theme(plot.title = element_text(hjust = 0.5, size = 14,face="bold")) +
          scale_fill_manual(values=c("#FFD5B3", "#C66F00"))
            #theme(legend.position = "none") +
            #geom_text(aes(label = prop), color = "white", size=6)
    })
    
    
    
    #Company Age Pie Chart
    output$company_age_pie <- renderPlot({
        user_data1 <- read_excel_allsheets(input$user_file$datapath)[1]
        user_data1 <- data.frame(user_data1)
        
        pie_data <- data.frame(
            group=c('Male','Female'),
            value=c(user_data1[2,2],user_data1[3,2])
        )
        
        pie_data <- pie_data %>% 
            arrange(desc(group)) %>%
            mutate(prop = value / sum(pie_data$value) *100) %>%
            mutate(ypos = cumsum(prop)- 0.5*prop)
        
        #View(pie_data)
        
        ggplot(pie_data, aes(x="", y=value, fill=group)) +
            geom_bar(stat="identity", width=1) +
            coord_polar("y", start=0) +
            theme_void() +
            labs(title = "Percent Male and Female in Your Company") +
          theme(plot.title = element_text(hjust = 0.5, size = 14,face="bold")) +
          scale_fill_manual(values=c("#FFD5B3", "#C66F00"))
        #theme(legend.position = "none") +
        #geom_text(aes(label = prop), color = "white", size=6)
    })
    
    #Your City df bar charts for age
    output$age_total_plot <- renderPlot({
        ggplot(data = eval(as.name(Datalong_age()))[1:6,]) + 
            geom_col(aes(x = age_group, y = value, fill=value)) +
            labs(title = paste("Number of People per Age Group in", input$usercity), 
                 x = "Age Groups", y = "Total") +
            scale_x_discrete(labels=c("age_under_20_years" = "Under 20", "age_20_29_years" = "20 to 29",
                                      "age_30_39_years" = "30 to 39", "age_40_49_years" = "40 to 49",
                                      "age_50_59_years" = "50 to 59", "age_60_years_and_over" = "Over 60")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    output$age_male_plot <- renderPlot({
        ggplot(data = eval(as.name(Datalong_age()))[7:12,]) + 
            geom_col(aes(x = age_group, y = value, fill=value)) +
            labs(title = paste("Number of Males per Age Group in", input$usercity), 
                 x = "Age Groups", y = "Total") +
            scale_x_discrete(labels=c("age_under_20_years" = "Under 20", "age_20_29_years" = "20 to 29",
                                      "age_30_39_years" = "30 to 39", "age_40_49_years" = "40 to 49",
                                      "age_50_59_years" = "50 to 59", "age_60_years_and_over" = "Over 60")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    output$age_female_plot <- renderPlot({
        ggplot(data = eval(as.name(Datalong_age()))[13:18,]) + 
            geom_col(aes(x = age_group, y = value, fill=value)) +
            labs(title = paste("Number of Females per Age Group in", input$usercity), 
                 x = "Age Groups", y = "Total") +
            scale_x_discrete(labels=c("age_under_20_years" = "Under 20", "age_20_29_years" = "20 to 29",
                                      "age_30_39_years" = "30 to 39", "age_40_49_years" = "40 to 49",
                                      "age_50_59_years" = "50 to 59", "age_60_years_and_over" = "Over 60")) +
        theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
              plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
              axis.title=element_text(size=14)) +
        scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    #Making side by side bar chart comparing age 
    output$user_age_total<- renderPlot({
        #making data frame
        user_data1 <- read_excel_allsheets(input$user_file$datapath)[1]
        user_data1 <- data.frame(user_data1)
        
        #pivot longer
        user_data1 <- pivot_longer(user_data1, cols = Age.Under.20.years: Age.60.years.and.over, names_to = "age_group")
        
        #setting factors so bar chart in right order
        user_data1$age_group <- factor(user_data1$age_group,levels = c("Age.Under.20.years", 
                                                                        "Age.20.to.29.years", 
                                                                        "Age.30.to.39.years", 
                                                                        "Age.40.to.49.years",
                                                                        "Age.50.to.59.years",
                                                                        "Age.60.years.and.over"))
        #plotting bar chart
        ggplot(data = user_data1[1:6,]) + 
            geom_col(aes(x = age_group, y = value, fill=value)) +
            labs(title = "Number of People per Age Group for Your Company", 
                 x = "Age Groups", y = "Total") +
            scale_x_discrete(labels=c("Age.Under.20.years" = "Under 20", "Age.20.to.29.years" = "20 to 29",
                                      "Age.30.to.39.years" = "30 to 39", "Age.40.to.49.years" = "40 to 49",
                                      "Age.50.to.59.years" = "50 to 59", "Age.60.years.and.over" = "Over 60")) +
          theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
                plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
                axis.title=element_text(size=14)) +
          scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    output$user_age_male<- renderPlot({
        #making data frame
        user_data1 <- read_excel_allsheets(input$user_file$datapath)[1]
        user_data1 <- data.frame(user_data1)
        
        #pivot longer
        user_data1 <- pivot_longer(user_data1, cols = Age.Under.20.years: Age.60.years.and.over, names_to = "age_group")
        
        #setting factors so bar chart in right order
        user_data1$age_group <- factor(user_data1$age_group,levels = c("Age.Under.20.years", 
                                                                       "Age.20.to.29.years", 
                                                                       "Age.30.to.39.years", 
                                                                       "Age.40.to.49.years",
                                                                       "Age.50.to.59.years",
                                                                       "Age.60.years.and.over"))
        #plotting bar chart
        ggplot(data = user_data1[7:13,]) + 
            geom_col(aes(x = age_group, y = value, fill=value)) +
            labs(title = "Number of Males per Age Group for Your Company", 
                 x = "Age Groups", y = "Total") +
            scale_x_discrete(labels=c("Age.Under.20.years" = "Under 20", "Age.20.to.29.years" = "20 to 29",
                                      "Age.30.to.39.years" = "30 to 39", "Age.40.to.49.years" = "40 to 49",
                                      "Age.50.to.59.years" = "50 to 59", "Age.60.years.and.over" = "Over 60")) +
          theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
                plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
                axis.title=element_text(size=14)) +
          scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
    
    
    output$user_age_female<- renderPlot({
        #making data frame
        user_data1 <- read_excel_allsheets(input$user_file$datapath)[1]
        user_data1 <- data.frame(user_data1)
        
        #pivot longer
        user_data1 <- pivot_longer(user_data1, cols = Age.Under.20.years: Age.60.years.and.over, names_to = "age_group")
        
        #setting factors so bar chart in right order
        user_data1$age_group <- factor(user_data1$age_group,levels = c("Age.Under.20.years", 
                                                                       "Age.20.to.29.years", 
                                                                       "Age.30.to.39.years", 
                                                                       "Age.40.to.49.years",
                                                                       "Age.50.to.59.years",
                                                                       "Age.60.years.and.over"))
        #plotting bar chart
        ggplot(data = user_data1[13:18,]) + 
            geom_col(aes(x = age_group, y = value, fill=value)) +
            labs(title = "Number of Females per Age Group for Your Company", 
                 x = "Age Groups", y = "Total") +
            scale_x_discrete(labels=c("Age.Under.20.years" = "Under 20", "Age.20.to.29.years" = "20 to 29",
                                      "Age.30.to.39.years" = "30 to 39", "Age.40.to.49.years" = "40 to 49",
                                      "Age.50.to.59.years" = "50 to 59", "Age.60.years.and.over" = "Over 60")) +
          theme(axis.text.x = element_text(angle = 30, vjust=0.9, hjust=0.9),
                plot.title = element_text(hjust = 0.5, size = 18,face="bold"), axis.text=element_text(size=12),
                axis.title=element_text(size=14)) +
          scale_fill_gradient(low= "#FFD5B3", high= "#C66F00")
    })
})