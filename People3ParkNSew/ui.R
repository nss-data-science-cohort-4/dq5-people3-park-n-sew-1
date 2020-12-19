
shinyUI(navbarPage(title = "People 3", inverse = TRUE,
                   
                   
                   # ********** ABOUT TAB **********
                   tabPanel("ABOUT", 
                            img(src='people3_logo.png', height="60%", width="60%", align = "center"),
                            h2('Hi, welcome to People3!'),
                            h4('Our goal at People3 is simple–to help people navigate people differences. And we do that through providing customized and impactful diversity and inclusion training, consulting, and research services that improve the people relationships within organizations.'),
                            h4('This app is designed to compare your company\'s demographics with the demographics of your surrounding city.'),
                            h4('The demographic data used in this app was collected from the US Census.'),
                            h4('To get started, visit the \"Upload Your Data\" Page.'),
                            h4('Once you have your data uploaded you can use the tabs to explore the demographics in your company.')
                   ),
                   
                   # ********** Upload Data TAB **********
                   tabPanel("UPLOAD YOUR DATA",
                            sidebarLayout(
                                sidebarPanel(
                                  selectInput("usercity",
                                              "Where are you from?",
                                              choices = c("Atlanta", "Boise", "Memphis", "Nashville", "Minneapolis", "Seattle"),
                                              selected = "Nashville"),
                                  fileInput("user_file", 
                                              "Upload your File",
                                              accept = c(".xls", ".xlsx")
                                              )
                                ),
                                mainPanel(
                                  h2('How to Input Your Data:'),
                                  h4('1. Click the \"Download\" button below and download the excel template.'),
                                  h4('2. Fill in the three excel sheets with your 
                                       company\'s data (the totals should self-calculate).'),
                                  h4('3. Save the filled-in template to your computer.'),
                                  h4('4. Using the \"Browse...\" button (to the left) upload the file.'),
                                  h4('5. You are ready to start using our web app!'),
                                  h2('Sample Template:'),
                                  img(src='capture.PNG', height="80%", width="80%", align = "center"),
                                  h6(""),
                                  downloadButton("downloadData", "Download Template")
                                )
                            )
                   ),
                   
                   
                   
                   
                   # ********** RACE TAB **********
                   tabPanel("RACE",
                            sidebarLayout(
                              sidebarPanel(
                                plotOutput('nash_race_pie'),
                                plotOutput('company_race_pie')
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Total", plotOutput("race_total_plot"), plotOutput("user_race_total")),
                                            tabPanel("Not Hispanic or Latino", plotOutput("not_hispanic_plot"), plotOutput("user_not_hispanic")),
                                            tabPanel("Hispanic or Latino", plotOutput("hispanic_plot"), plotOutput("user_hispanic"))
                                )
                              )
                            )
                            
                   ),
                   
                   
                   
                   
                   
                   # ********** EDUCATION TAB **********
                   tabPanel("EDUCATION",
                            sidebarLayout(
                              sidebarPanel(
                                plotOutput('nash_edu_pie'),
                                plotOutput('company_edu_pie')
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Total", plotOutput("edu_total_plot"), 
                                                     h1(" "),
                                                     plotOutput("user_edu_total")),
                                            tabPanel("Male", plotOutput("edu_male_plot"), 
                                                     h1(" "),
                                                     plotOutput("user_edu_male")),
                                            tabPanel("Female", plotOutput("edu_female_plot"), 
                                                     h1(" "),
                                                     plotOutput("user_edu_female"))
                                )
                              )
                            )
                            
                   ),
                   
                   
                   
                   
                   # ********** AGE TAB **********
                   tabPanel("AGE",
                            sidebarLayout(
                              sidebarPanel(
                                plotOutput('nash_age_pie'),
                                plotOutput('company_age_pie')
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Total", plotOutput("age_total_plot"), plotOutput("user_age_total")),
                                            tabPanel("Male", plotOutput("age_male_plot"), plotOutput("user_age_male")),
                                            tabPanel("Female", plotOutput("age_female_plot"), plotOutput("user_age_female"))
                                )
                              )
                            )
                            
                   )
                   
))