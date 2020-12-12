shinyServer(function(input, output, session) {
    
    
    # for display of mtcars dataset in the "Data Page"
    output$data <- renderTable({
        mtcars
    })
    
    
    
    # Matt's Output for his Page
    output$plot <- renderPlot({
        hist(mtcars$mpg, col ="lightblue1", breaks=input$matt_slider )
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