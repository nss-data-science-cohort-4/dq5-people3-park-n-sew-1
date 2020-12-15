shinyServer(function(input, output, session) {
    
    
    # for display of mtcars dataset in the "Data Page"
    output$data <- renderTable({
        mtcars
    })
    
    
    # Matt's Output for his Page
    #output$user_data_output <- renderTable({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
       # user_data <- input$user_file
        
        #if (is.null(user_data))
            #return(NULL)
        
       #user_age <- read_excel_allsheets(user_data$datapath)[1]
       # user_race <- read_excel_allsheets(user_data$datapath)[2]
        #user_edu <- read_excel_allsheets(user_data$datapath)[3]
        #user_age
        #user_race
        #user_edu
    
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
        
    
    
    
    
    
    
    
    #Savannah's Output for her Page
    output$plot2 <- renderPlot({
        hist(mtcars$mpg, col ="lightblue1", breaks=input$sav_slider )
    })
  
    
    
    # for display of mtcars dataset summary statistics in the "Menu item A page"
    output$summary <- renderPrint({
        summary(mtcars)
    })
    
})