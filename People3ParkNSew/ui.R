# Navigation through the tabs at the top of the page
# These navigation tabs may server as individual pages, may have contents or widgets or further menu based navigation

# use inverse = T , for black background of nav bar menu and light fonts
# use selected argument to specify which tab should show up when app loads
# use position="fixed-bottom" - navbar at the bottom
# use position="static-top" - for scrollable bar
# use position="fixed-top" - bar is fixed but page is scrollable
# use collapsible = T if the app is used on tab where resolution is less. It will appear as collapsible menu
# Important note that a navbarmenu cannot be the first tab/page
shinyUI(navbarPage(title = "People 3 Title Here", 
                   
                   tabPanel("About Page", 
                            h4("Eventually, we will add some text here that looks nice and explains our app.")
                   ),
                   
                   
                   tabPanel("Matt's Tab",
                            sidebarLayout(
                                sidebarPanel(
                                    sliderInput("matt_slider", "Select no. of BINs", min = 5, max = 20,value = 10)
                                ),
                                mainPanel(
                                    plotOutput("plot")
                                )
                            )
                   ),
                   
                   
                   tabPanel("Savannah's tab",
                            sidebarLayout(
                                sidebarPanel(
                                    sliderInput("sav_slider", "Select no. of BINs", min = 5, max = 20,value = 10)
                                ),
                                mainPanel(
                                    plotOutput("plot2")
                                )
                            )
                   ),
                   
                   ##Saving this incase you want to use a dropdown
                   ## Use navbarmenu to get the tab with menu capabilities
                   navbarMenu("Menu Options",
                              tabPanel("Menu item A - Summary stats", verbatimTextOutput("summary")),
                              tabPanel("Menu item B - Link to code",
                                       h4(HTML(paste("Thanks for watching the video. Reference code available at the following", a(href="https://github.com/aagarw30/R-Shinyapp-Tutorial/tree/master/shinylayouts/navbarpage%20demo", "link"), "."))),
                                       h4(HTML(paste("In case you have questions", a(href="mailto:aagarw30@gmail.com", "email me"), ".")))
                                       
                              ))
))