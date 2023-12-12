library(shiny)
library(plotly)
library(ggplot2)
library(shinydashboard)
#install.packages("shinysky")
library(readr)
library(readxl)
library(stringr)
library(dplyr)
library(purrr)                   # requirement packages for Functional Programming Tools
library(rlang)                   # requirement packages for Rmarkdown
library(DT)                      # interface to the JavaScript library DataTables (https://datatables.net/)
library(r2d3)
library(tokenizers)
library(stopwords)
library(wordcloud2)
library(tm)
library(wordcloud)
library(stopwords)


#install.packages('rsconnect')
#library(rsconnect)
#rsconnect::setAccountInfo(name='rishavkant',token='A154C0AA2E4C4E221E51BA6B4D90CAA4',secret='rl1v+kpo2/7/EnnktlP0vu5xUzj2UMSVnCiPopwb')


#----------------------------------------------------------------------#



ui <- dashboardPage(title = "www.rishavkant.com",
                    
                    # Header
                    dashboardHeader(title = "Job Aid: H1B Insights Dashboard for Students and Professionals",titleWidth = 700),
                    
                    # Side bar of the Dashboard
                    dashboardSidebar(
                      selectInput(
                        inputId = "var_dept",
                        label = "Department:",
                        choices =  unique(final_data_combined$DEPARTMENT),
                        selected = "Information",
                        selectize = FALSE) ,
                      
                      sliderInput('var_top', 'Select Top Company', min = 5, max = 30, step = 1, value = 1),
                     # selectInput('var_year','Select Year For Analysis' , choices = unique(final_data_combined$Fiscal.Year)),
                     # textInput("text_input1", "Search Your Fav Company Hiring History", ""),
                      fileInput("file_input_1", "Upload your Resume for Company Analysis"),
                    #  fileInput("file", "Choose a PDF file"),
                     # selectInput("variable_select", "Select a variable1:", ""),
                     # selectInput("variable_select2", "Select a variable2:", ""),
                      
                      
                      
                      
                      # Side menu of the Dashboard  
                      sidebarMenu(
                        actionLink("remove", icon = icon("sync-alt"),"Remove detail tabs"),
                        menuItem("Source Code", icon = icon("github"), href = "https://github.com/kantrishav/Equity-and-ETF-Global-News-Sentiment-Analysis-R-Python"),
                        menuItem("About Me", icon = icon("linkedin"), href = "https://www.linkedin.com/in/rishav-kant-185775131/"),
                        menuItem("Video", icon = icon("youtube"), href = "https://www.youtube.com/watch?v=B2JzHv4FOTU")
                      )
                    ),
                    
                    # The body of the dashboard
                    dashboardBody(
                      tabsetPanel(
                        
                        tabPanel(title = "Choose Your Prefer Industry To Know Your Top Companies You Should Apply Jobs For",
                                 value = "page1",
                                 fluidRow(
                                   valueBoxOutput("company_count_value"),
                                   valueBoxOutput("total_flights"),
                                   
                                   valueBoxOutput("ty_ly_growth") 
                                 ),
                                 fluidRow(
                                   #column(width = 4,plotOutput("distPlot3" , height = 200)),
                                   column(width = 12,plotlyOutput("plot_line_dept" , height = 280)),
                                   column(width = 6,plotlyOutput("distPlot9" , height = 400)),
                                   column(width = 6,plotlyOutput("plot6" , height = 400)),
                                   
                                   column(width = 12,plotlyOutput("plot4_1" , height = 400))
                                   
                                 )
                        )
                        ,  tabPanel('Free Resume Keyword Generator' ,
                                    
                              
                                    fluidRow(
                                      column(width = 1, actionButton("generate_wordcloud", "Analyze and Generate Keywords"))
                                      #column(width = 4,textAreaInput("text_input1", "Paste the Job Description to Find Out Resume KeyWords" , width = "40%" , height= "600px" ),height = 400 ),
                                   #column(width = 7,wordcloud2Output("wordcloud" , width = "40%", height = "300px") , height = 400 )  
                                   
                                   ),
                                   fluidRow(
                                     #column(width = 1, actionButton("generate_wordcloud", "Analyze and Generate Keywords")),
                                     column(width = 7,textAreaInput("text_input1", "Paste the Job Description to Find Out Resume KeyWords" , width = "100%" , height= "600px" ),height = 400 ),
                                     column(width = 5,wordcloud2Output("wordcloud" , width = "100%", height = "600px") , height = 400 )  
                                     
                                   )
                                   
                                   )
                   
                        
                        
                        
                      )
                      
                      
                      
                      
                      
                    )
)
