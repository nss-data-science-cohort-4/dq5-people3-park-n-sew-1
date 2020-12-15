shinyServer(function(input, output, session) {
    
    
    # for display of mtcars dataset in the "Data Page"
    #output$data <- renderTable({
    #    mtcars
    #})
    
    
    
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
    
    #Testing Reactive Element
    rval_age <- reactive({ 
        req(input$user_file) ## ?req #  require that the input is available
        
        #Read in a dataframe of age data on users's upload.
        #This uses a function to read file as a list of dataframes.
        user_age_df <- t(data.frame(read_excel_allsheets(input$user_file$datapath)[1]))
        colnames(user_age_df) <- as.character(user_age_df[1, ])
        user_age_df <- data.frame(user_age_df[-1,])
        
        return(user_age_df)
    })

    #output$contents <- renderTable({
    #    data()
   # })
    
    #output$MyPlot <- renderPlot({
        # for a histogram: remove the second variable (it has to be numeric as well):
        # x    <- data()[, c(input$xcol, input$ycol)]
        # bins <- nrow(data())
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        # Correct way:
        # x    <- data()[, input$xcol]
        # bins <- nrow(data())
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
        
        # I Since you have two inputs I decided to make a scatterplot
       # x <- data()[, c(input$xcol, input$ycol)]
       # plot(x)
        
    #})
#})
    
    
    
    
    
   # rval_age <- reactive({
   #     data.frame(read_excel_allsheets(input$user_file$datapath)[1]) %>%
   #     t()
   # })
    
    
    #Savannah testing trying to make 3 outputs 1 for each data frame    
    #Use reactive dataframe to create plots.
    output$user_age <- renderPlot({
        rval_age() %>%
        ggplot(aes(x=rownames(), y=value)) + geom_bar()
        
      #  if (is.null(user_age))
      #      {return(NULL)}
      #  else
       #     {return(user_age)}
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