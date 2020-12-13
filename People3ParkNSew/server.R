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
    
    #Savannah testing trying to make 3 outputs 1 for each data frame    
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