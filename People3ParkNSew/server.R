shinyServer(function(input, output, session) {
    
    
    #******** ABOUT TAB ***********
    
    
    
    
    #******** MATT'S TAB ***********
    
    #Making a table for each user input sheet 
    output$user_age<- renderTable({
        user_data1 <- read_excel_allsheets(input$user_file$datapath)[1]
        
        if (is.null(user_data1))
        {return(NULL)}
        else
        {return(user_data1)}
    })
    
    output$user_race<- renderTable({
        user_data2 <- read_excel_allsheets(input$user_file$datapath)[2]
        
        if (is.null(user_data2))
        {return(NULL)}
        else
        {return(user_data2)}
    })
    
    output$user_edu<- renderTable({
        user_data3 <- read_excel_allsheets(input$user_file$datapath)[3]
        
        if (is.null(user_data3))
        {return(NULL)}
        else
        {return(user_data3)}
    }) 
    
    output$downloadData <- downloadHandler(
      filename = 'User_Company_Template.xlsx',
      content = function(file) {
        file.copy(from = 'Example_Profile_Download.xlsx', to = file)
        #command = paste('cp duck.jpg', file)
        #system(command)
      })
    
    
    
    #******** SAVANNAH'S TAB (DELETE LATER) ***********
    #Savannah's Output for her Page
    output$plot2 <- renderPlot({
        hist(mtcars$mpg, col ="lightblue1", breaks=input$sav_slider )
    })
    
    
    
    
    
    
    
    #******** RACE TAB ***********
    
    # >>> MATTTTT YOOUR WORK GOES RIGHT HERE :) <<<<<<<
 
    #Nashville Age Pie Chart
    output$nash_race_pie <- renderPlot({
      pie_race_data <- data.frame(
        group=c('Not Hispanic or Latino','Hispanic or Latino'),
        value=c(race_df[2,2],race_df[3,2])
      )
      
      pie_race_data <- pie_race_data %>% 
        arrange(desc(group)) %>%
        mutate(prop = value / sum(pie_race_data$value) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop)
      
      ggplot(pie_race_data, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        labs(title = "Percent Hispanic or Latino in Nashville")
      #theme(legend.position = "none") +
      #geom_text(aes(label = prop), color = "white", size=6)
    })
    
    
    
    #Company Age Pie Chart
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
      
      #View(pie_race_data)
      
      ggplot(pie_race_data, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        labs(title = "Percent Hispanic or Latino in Your Company")
      #theme(legend.position = "none") +
      #geom_text(aes(label = prop), color = "white", size=6)
    })
    
    
    
    #nashville df bar charts for race
    output$race_total_plot <- renderPlot({
      ggplot(data = Datalong_race[1:9,]) + 
        geom_col(aes(x = race_group, y = value)) +
        labs(title = "Number of People per Race and Ethnicity in Nashville", 
             x = "Race and Ethnicity Groups", y = "Total") +
        scale_x_discrete(labels=c("white" = "White", 
                                  "black_or_african_american" = "Black or African American", 
                                  "american_indian_or_alaska_native" = "American Indian or Alaska Native", 
                                  "asian" = "Asian",
                                  "native_hawaiian_or_pacific_islander" = "Native Hawaiian or Pacific Islander",
                                  "other_race" = "Other Race",
                                  "two_or_more_races" = "Two or More Races",
                                  "two_or_more_races_including_other" = "Two or More Races (including 'Other')",
                                  "two_or_more_races_excluding_other_and_three_or_more" = "Two or More Races (excluding 'Other') and Three or More Races"))
    })
    
    output$not_hispanic_plot <- renderPlot({
      ggplot(data = Datalong_race[10:18,]) + 
        geom_col(aes(x = race_group, y = value)) +
        labs(title = "Race of Non-Hispanic or Latino in Nashville", 
             x = "Race", y = "Total") +
        scale_x_discrete(labels=c("white" = "White", 
                                  "black_or_african_american" = "Black or African American", 
                                  "american_indian_or_alaska_native" = "American Indian or Alaska Native", 
                                  "asian" = "Asian",
                                  "native_hawaiian_or_pacific_islander" = "Native Hawaiian or Pacific Islander",
                                  "other_race" = "Other Race",
                                  "two_or_more_races" = "Two or More Races",
                                  "two_or_more_races_including_other" = "Two or More Races (including 'Other')",
                                  "two_or_more_races_excluding_other_and_three_or_more" = "Two or More Races (excluding 'Other') and Three or More Races"))
    })
    
    output$hispanic_plot <- renderPlot({
      ggplot(data = Datalong_race[19:27,]) + 
        geom_col(aes(x = race_group, y = value)) +
        labs(title = "Race of Hispanic or Latino in Nashville", 
             x = "Race", y = "Total") +
        scale_x_discrete(labels=c("white" = "White", 
                                  "black_or_african_american" = "Black or African American", 
                                  "american_indian_or_alaska_native" = "American Indian or Alaska Native", 
                                  "asian" = "Asian",
                                  "native_hawaiian_or_pacific_islander" = "Native Hawaiian or Pacific Islander",
                                  "other_race" = "Other Race",
                                  "two_or_more_races" = "Two or More Races",
                                  "two_or_more_races_including_other" = "Two or More Races (including 'Other')",
                                  "two_or_more_races_excluding_other_and_three_or_more" = "Two or More Races (excluding 'Other') and Three or More Races"))
    })
    
    #Making side by side bar chart comparing age 
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
        geom_col(aes(x = race_group, y = value)) +
        labs(title = "Number of People per Race for Your Company", 
             x = "Race", y = "Total") +
        scale_x_discrete(labels=c("Race.White.alone" = "White", 
                                  "Race.Black.or.African.American" = "Black or African American", 
                                  "Race.American.Indian.and.Alaska.Native" = "American Indian or Alaska Native", 
                                  "Race.Asian" = "Asian",
                                  "Race.Native.Hawaiian.and.Other.Pacific.Islander" = "Native Hawaiian or Pacific Islander",
                                  "Race.Some.other.race" = "Other Race",
                                  "Race.Two.or.more.races." = "Two or More Races",
                                  "Race.Two.races.including.Some.other.race" = "Two or More Races (including 'Other')",
                                  "Race.Two.races.excluding.Some.other.race..and.three.or.more.races" = "Two or More Races (excluding 'Other') and Three or More Races"))
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
        geom_col(aes(x = race_group, y = value)) +
        labs(title = "Race of Non-Hispanic or Latino in Your Company", 
             x = "Race", y = "Total") +
        scale_x_discrete(labels=c("Race.White.alone" = "White", 
                                  "Race.Black.or.African.American" = "Black or African American", 
                                  "Race.American.Indian.and.Alaska.Native" = "American Indian or Alaska Native", 
                                  "Race.Asian" = "Asian",
                                  "Race.Native.Hawaiian.and.Other.Pacific.Islander" = "Native Hawaiian or Pacific Islander",
                                  "Race.Some.other.race" = "Other Race",
                                  "Race.Two.or.more.races." = "Two or More Races",
                                  "Race.Two.races.including.Some.other.race" = "Two or More Races (including 'Other')",
                                  "Race.Two.races.excluding.Some.other.race..and.three.or.more.races" = "Two or More Races (excluding 'Other') and Three or More Races"))
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
        geom_col(aes(x = race_group, y = value)) +
        labs(title = "Race of Hispanic or Latino in Nashville", 
             x = "Race", y = "Total") +
        scale_x_discrete(labels=c("Race.White.alone" = "White", 
                                  "Race.Black.or.African.American" = "Black or African American", 
                                  "Race.American.Indian.and.Alaska.Native" = "American Indian or Alaska Native", 
                                  "Race.Asian" = "Asian",
                                  "Race.Native.Hawaiian.and.Other.Pacific.Islander" = "Native Hawaiian or Pacific Islander",
                                  "Race.Some.other.race" = "Other Race",
                                  "Race.Two.or.more.races." = "Two or More Races",
                                  "Race.Two.races.including.Some.other.race" = "Two or More Races (including 'Other')",
                                  "Race.Two.races.excluding.Some.other.race..and.three.or.more.races" = "Two or More Races (excluding 'Other') and Three or More Races"))
    })   
    
    
    
    
    
    
    
    #******** EDUCATION TAB ***********
    
    
    #Nashville Age Pie Chart
    output$nash_edu_pie <- renderPlot({
      pie_edu_data <- data.frame(
        group=c('Male','Female'),
        value=c(age_df[2,2],age_df[3,2])
      )
      
      pie_edu_data <- pie_edu_data %>% 
        arrange(desc(group)) %>%
        mutate(prop = value / sum(pie_edu_data$value) *100) %>%
        mutate(ypos = cumsum(prop)- 0.5*prop)
      
      ggplot(pie_edu_data, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        labs(title = "Percent Male and Female in Nashville")
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
      
      #View(pie_edu_data)
      
      ggplot(pie_edu_data, aes(x="", y=value, fill=group)) +
        geom_bar(stat="identity", width=1) +
        coord_polar("y", start=0) +
        theme_void() +
        labs(title = "Percent Male and Female in Your Company")
      #theme(legend.position = "none") +
      #geom_text(aes(label = prop), color = "white", size=6)
    })
    
    
    
    #nashville df bar charts for edu
    output$edu_total_plot <- renderPlot({
      ggplot(data = Datalong_edu[1:8,]) + 
        geom_col(aes(x = edu_group, y = value)) +
        labs(title = "Number of People per Education Level in Nashville", 
             x = "Education Level", y = "Total") +
        scale_x_discrete(labels=c("no_high_school_diploma" = "No High School Diploma", 
                                  "high_school_graduate" = "High School Graduate",
                                  "some_college_no_degree" = "Some College",
                                  "associates_degree" = "Associate Degree", 
                                  "bachelors_degree" = "Bachelor's Degree",
                                  "masters_degree" = "Master's Degree",
                                  "professional_degree" = "Professional Degree",
                                  "doctorate_degree" = "Doctorate Degree"))
    })
    
    output$edu_male_plot <- renderPlot({
      ggplot(data = Datalong_edu[9:16,]) + 
        geom_col(aes(x = edu_group, y = value)) +
        labs(title = "Number of Males per Education Level in Nashville", 
             x = "Education Level", y = "Total") +
        scale_x_discrete(labels=c("no_high_school_diploma" = "No High School Diploma", 
                                  "high_school_graduate" = "High School Graduate",
                                  "some_college_no_degree" = "Some College",
                                  "associates_degree" = "Associate Degree", 
                                  "bachelors_degree" = "Bachelor's Degree",
                                  "masters_degree" = "Master's Degree",
                                  "professional_degree" = "Professional Degree",
                                  "doctorate_degree" = "Doctorate Degree"))
    })
    
    output$edu_female_plot <- renderPlot({
      ggplot(data = Datalong_edu[17:24,]) + 
        geom_col(aes(x = edu_group, y = value)) +
        labs(title = "Number of Females per Education Level in Nashville", 
             x = "Education Level", y = "Total") +
        scale_x_discrete(labels=c("no_high_school_diploma" = "No High School Diploma", 
                                  "high_school_graduate" = "High School Graduate",
                                  "some_college_no_degree" = "Some College",
                                  "associates_degree" = "Associate Degree", 
                                  "bachelors_degree" = "Bachelor's Degree",
                                  "masters_degree" = "Master's Degree",
                                  "professional_degree" = "Professional Degree",
                                  "doctorate_degree" = "Doctorate Degree"))
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
        geom_col(aes(x = edu_group, y = value)) +
        labs(title = "Number of People per Education Level for Your Company", 
             x = "Education Level", y = "Total") +
        scale_x_discrete(labels=c("no_high_school_diploma" = "No High School Diploma", 
                                  "high_school_graduate" = "High School Graduate",
                                  "some_college_no_degree" = "Some College",
                                  "associates_degree" = "Associate Degree", 
                                  "bachelors_degree" = "Bachelor's Degree",
                                  "masters_degree" = "Master's Degree",
                                  "professional_degree" = "Professional Degree",
                                  "doctorate_degree" = "Doctorate Degree"))
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
        geom_col(aes(x = edu_group, y = value)) +
        labs(title = "Number of Males per Education Level for Your Company", 
             x = "Education Level", y = "Total") +
        scale_x_discrete(labels=c("no_high_school_diploma" = "No High School Diploma", 
                                  "high_school_graduate" = "High School Graduate",
                                  "some_college_no_degree" = "Some College",
                                  "associates_degree" = "Associate Degree", 
                                  "bachelors_degree" = "Bachelor's Degree",
                                  "masters_degree" = "Master's Degree",
                                  "professional_degree" = "Professional Degree",
                                  "doctorate_degree" = "Doctorate Degree"))
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
        geom_col(aes(x = edu_group, y = value)) +
        labs(title = "Number of Females per Education Level for Your Company", 
             x = "Education Level", y = "Total") +
        scale_x_discrete(labels=c("no_high_school_diploma" = "No High School Diploma", 
                                  "high_school_graduate" = "High School Graduate",
                                  "some_college_no_degree" = "Some College",
                                  "associates_degree" = "Associate Degree", 
                                  "bachelors_degree" = "Bachelor's Degree",
                                  "masters_degree" = "Master's Degree",
                                  "professional_degree" = "Professional Degree",
                                  "doctorate_degree" = "Doctorate Degree"))
    })
    
    
    
    
    
    
    
    #******** AGE TAB ***********
    
    #Nashville Age Pie Chart
    output$nash_age_pie <- renderPlot({
        pie_data <- data.frame(
            group=c('Male','Female'),
            value=c(age_df[2,2],age_df[3,2])
        )
        
        pie_data <- pie_data %>% 
            arrange(desc(group)) %>%
            mutate(prop = value / sum(pie_data$value) *100) %>%
            mutate(ypos = cumsum(prop)- 0.5*prop)
        
        ggplot(pie_data, aes(x="", y=value, fill=group)) +
            geom_bar(stat="identity", width=1) +
            coord_polar("y", start=0) +
            theme_void() +
            labs(title = "Percent Male and Female in Nashville")
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
            labs(title = "Percent Male and Female in Your Company")
        #theme(legend.position = "none") +
        #geom_text(aes(label = prop), color = "white", size=6)
    })
    
    
    
    #nashville df bar charts for age
    output$age_total_plot <- renderPlot({
        ggplot(data = Datalong_age[1:6,]) + 
            geom_col(aes(x = age_group, y = value)) +
            labs(title = "Number of People per Age Group in Nashville", 
                 x = "Age Groups", y = "Total") +
            scale_x_discrete(labels=c("age_under_20_years" = "Under 20", "age_20_29_years" = "20 to 29",
                                      "age_30_39_years" = "30 to 39", "age_40_49_years" = "40 to 49",
                                      "age_50_59_years" = "50 to 59", "age_60_years_and_over" = "Over 60"))
    })
    
    output$age_male_plot <- renderPlot({
        ggplot(data = Datalong_age[7:12,]) + 
            geom_col(aes(x = age_group, y = value)) +
            labs(title = "Number of Males per Age Group in Nashville", 
                 x = "Age Groups", y = "Total") +
            scale_x_discrete(labels=c("age_under_20_years" = "Under 20", "age_20_29_years" = "20 to 29",
                                      "age_30_39_years" = "30 to 39", "age_40_49_years" = "40 to 49",
                                      "age_50_59_years" = "50 to 59", "age_60_years_and_over" = "Over 60"))
    })
    
    output$age_female_plot <- renderPlot({
        ggplot(data = Datalong_age[13:18,]) + 
            geom_col(aes(x = age_group, y = value)) +
            labs(title = "Number of Females per Age Group in Nashville", 
                 x = "Age Groups", y = "Total") +
            scale_x_discrete(labels=c("age_under_20_years" = "Under 20", "age_20_29_years" = "20 to 29",
                                      "age_30_39_years" = "30 to 39", "age_40_49_years" = "40 to 49",
                                      "age_50_59_years" = "50 to 59", "age_60_years_and_over" = "Over 60"))
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
            geom_col(aes(x = age_group, y = value)) +
            labs(title = "Number of People per Age Group for Your Company", 
                 x = "Age Groups", y = "Total") +
            scale_x_discrete(labels=c("Age.Under.20.years" = "Under 20", "Age.20.to.29.years" = "20 to 29",
                                      "Age.30.to.39.years" = "30 to 39", "Age.40.to.49.years" = "40 to 49",
                                      "Age.50.to.59.years" = "50 to 59", "Age.60.years.and.over" = "Over 60"))
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
            geom_col(aes(x = age_group, y = value)) +
            labs(title = "Number of Males per Age Group for Your Company", 
                 x = "Age Groups", y = "Total") +
            scale_x_discrete(labels=c("Age.Under.20.years" = "Under 20", "Age.20.to.29.years" = "20 to 29",
                                      "Age.30.to.39.years" = "30 to 39", "Age.40.to.49.years" = "40 to 49",
                                      "Age.50.to.59.years" = "50 to 59", "Age.60.years.and.over" = "Over 60"))
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
            geom_col(aes(x = age_group, y = value)) +
            labs(title = "Number of Females per Age Group for Your Company", 
                 x = "Age Groups", y = "Total") +
            scale_x_discrete(labels=c("Age.Under.20.years" = "Under 20", "Age.20.to.29.years" = "20 to 29",
                                      "Age.30.to.39.years" = "30 to 39", "Age.40.to.49.years" = "40 to 49",
                                      "Age.50.to.59.years" = "50 to 59", "Age.60.years.and.over" = "Over 60"))
    })
    
    

    

    
  
    #******** MENU DROPDOWN TAB (DELETE LATER) ***********
    
    # for display of mtcars dataset summary statistics in the "Menu item A page"
    output$summary <- renderPrint({
        summary(mtcars)
    })
    
    
})