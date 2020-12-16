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
    
    
    
    
    
    #******** SAVANNAH'S TAB (DELETE LATER) ***********
    #Savannah's Output for her Page
    output$plot2 <- renderPlot({
        hist(mtcars$mpg, col ="lightblue1", breaks=input$sav_slider )
    })
    
    
    
    
    
    
    
    #******** RACE TAB ***********
    
    # >>> MATTTTT YOOUR WORK GOES RIGHT HERE :) <<<<<<<
    
    
    
    
    
    
    
    
    #******** EDUCATION TAB ***********
    
    #I WILL WORK HERE 
    
    
    
    
    
    
    
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
        
        View(pie_data)
        
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