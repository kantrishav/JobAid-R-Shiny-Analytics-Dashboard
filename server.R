library(shiny)
library(plotly)
library(ggplot2)
library(shinydashboard)

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

source("global.R", local = TRUE)

server <- function(input, output , session) {
  
  
  
  
#  nlp_text <- reactive({ data.frame(tokenize_words(input$text_input1,stopwords = stopwords::stopwords("en")))})
  

  
  
  nlp_text <- reactive({
    df <- data.frame(tokenize_words(input$text_input1, stopwords = stopwords::stopwords("en")))
    colnames(df)[1] <- "Value"  # Change the column name of the first column to "Value"
    df
  })
  
  output$output_table <- renderTable({
    nlp_text()
  })
  
  
  
  output$wordcloud <- renderWordcloud2({
    req(input$generate_wordcloud)
    
    # Combine all words into a single character vector
    words <- unlist(nlp_text()$Value)
    
    # Create a corpus
    corpus <- Corpus(VectorSource(words))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("en"))
    corpus <- tm_map(corpus, stripWhitespace)

 
    dtm <- DocumentTermMatrix(corpus)
    
    # Convert the document-term matrix to a matrix
    m <- as.matrix(dtm)
    
    word_freqs <- sort(rowSums(m), decreasing = TRUE)
    
    word_freq_df <- data.frame(word = names(table(words)), freq = as.numeric(table(words)))
    word_freq_df <- word_freq_df %>% subset(freq >= 2 )
    
    wordcloud2(word_freq_df, size = 1, backgroundColor = "white")
    
  })
  
  
  
  
  

  
  
  
  
  
  filtered_data <- reactive({ filter(data_rank, DEPARTMENT == input$var_dept) })
  
 # filtered_data <- reactive({ filter(data_rank, Fiscal.Year == input$var_year) })
  
  
  
  line_chart_data <- reactive({
    
    data_rank %>% group_by(EMP , Fiscal.Year ) %>% summarise(Approval_Count = sum(Initial.Approvals)) %>% arrange(desc(Approval_Count)) %>%
      ungroup() %>% mutate(Rank = row_number(desc(Approval_Count)))
  })
  
  
  aggr_data <- reactive({ filtered_data() %>% group_by(EMP) %>% summarise(Approval_Count = sum(Initial.Approvals)) %>% arrange(desc(Approval_Count)) %>% ungroup() %>% mutate(Rank = row_number(desc(Approval_Count))) })
  
  
  #aggr_data <- reactive({ filtered_data() %>% group_by(EMP , Fiscal.Year ) %>% summarise(Approval_Count = sum(Initial.Approvals)) %>% arrange(desc(Approval_Count)) %>% ungroup() %>% mutate(Rank = row_number(desc(Approval_Count))) })
  
  
  ranked_data <- reactive({
    aggr_data() %>% filter(Rank <= input$var_top)
  })
  
  
  
  
  
  
  output$distPlot9 <- renderPlotly({
    
    ranked_data() %>%  arrange(desc(Approval_Count)) %>%  plot_ly( x = ~ reorder(EMP ,-Approval_Count) , y = ~Approval_Count, type = 'bar', color = ~EMP ,
                                                                   text = ~EMP, textposition = 'auto' ,  showlegend = FALSE ,
                                                                   marker = list(color = '#000000', line = list(color = '#000000', width = 1.5))) %>% layout(
                                                                     title = paste(" Here is your Top" , input$var_top ," Companies In Your Industry For Jobs") , xaxis = list(title = "Company's Name", showticklabels = FALSE) )
  })
  
  output$distPlot3 <- renderPlot({
    ranked_data()  %>% ggplot(aes(x = reorder(EMP, Approval_Count) , y = Approval_Count , fill = EMP)) +
      geom_bar(stat = "identity") +
      coord_flip() + geom_text(aes(label = Approval_Count)) + theme(legend.position = "none") })
  
  
  
  output$plot4_1 <-  renderPlotly( {
    
    line_chart_data() %>% filter(EMP %in% unique(ranked_data()$EMP))  %>% arrange(Fiscal.Year) %>% plot_ly( x = ~Fiscal.Year, y = ~Approval_Count, color = ~EMP , fill = 'tozeroy' , type = 'scatter', mode = 'lines+markers') %>%
      layout(title=  paste("How well your Top" , input$var_top ,"Companies hiring International OPT/CPT Workers over the time ?"))
    
  })
  
  output$plot4 <-  renderPlot( {
    
    line_chart_data() %>% filter(EMP %in% unique(ranked_data()$EMP)) %>% ggplot( aes(x = Fiscal.Year , y = Approval_Count , color = EMP ) ) + geom_line(size = 2)
    
  })
  
  
  
  
  #_----------------------------------------
  
  
  output$data_table_1 <- renderDataTable({
    file <- input$file_input_1
    
    if (!is.null(file)) {
      df <- read_excel(file$datapath)
      datatable(df)
    }
  })
  
  
  
  
  
  observe({
    file <- input$file_input_1
    
    if (!is.null(file)) {
      df <- read_excel(file$datapath)
      updateSelectInput(session, "variable_select", choices = colnames(df))
      updateSelectInput(session, "variable_select2", choices = colnames(df))
    }
  })
  
  
  
  
  output$scatter_plot <- renderPlot({
    file <- input$file_input_1
    
    if (!is.null(file)) {
      df <- read_excel(file$datapath)
      x_var <- input$variable_select
      y_var <- input$variable_select2
      
      
      ggplot(df, aes_string(x = x_var, y = y_var)) +
        geom_point() + labs(title = paste("Scatter Plot of", x_var, "vs", y_var))
      
    }
  })
  
  #---------------------------------------
  
  
  

  
  #----------------------------
  
  filtered_dept <- reactive({
    filter(data_rank, DEPARTMENT == input$var_dept)
  })
  
  
  
  #----------------------- ValueBox Testing ------------------------------------------------------------------
  
  
  m1 <- reactive({
    filter(data_23, DEPARTMENT == input$var_dept)
  })
  
  
  m2 <- reactive({
    filter(data_22, DEPARTMENT == input$var_dept)
  })
  
  
  
  

  
  output$ty_ly_growth <- renderValueBox({
    
    m1_app <- sum(m1()$Initial.Approvals)
    m2_app <- sum(m2()$Initial.Approvals)
    
    gwth <- (m1_app - m2_app)/m2_app*100
    
      valueBox( paste(round(gwth,2),'%')  ,icon = icon("circle-down"), color = "red",subtitle = "# Last Year vs This Year Hiring Growth")
  })
  
  
  
  
  
  
  
  output$company_count_value <- renderValueBox({
    
    emp_count <- nrow(table(unique(filtered_dept()$EMP)))
    
      valueBox(emp_count , icon = icon("building"), color = "green",subtitle = "#No. of Companies Sponosoring H1B Visa")
  })
  
  
  
  
  
  output$total_flights <- renderValueBox({
    
    sum(filtered_dept()$Initial.Approvals) %>% valueBox(icon = icon("chart-bar"), color = "purple",subtitle = "Number of Total Sponsorships")
  })
  
  
  
  
  
  
  
  #---------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  output$plot_line_dept <-  renderPlotly( {
    
    filtered_dept() %>% group_by(Fiscal.Year) %>% summarise(Total_Sponsorship = sum(Initial.Approvals)) %>% arrange(Fiscal.Year)  %>% plot_ly( x = ~Fiscal.Year, y = ~Total_Sponsorship , fill = 'tozeroy' , fillcolor  = '#FF8674' , type = 'scatter', mode = 'lines+markers') %>%
      add_trace(
        type = 'scatter',
        mode = 'text',
        text = ~Total_Sponsorship,
        textposition = 'top',
        showlegend = FALSE , 
        marker = list(color = 'black')
      ) %>%
      layout(
        title = "What is trend of US Giants sponosoring International Talent in last one decade?"  , 
        xaxis = list(title = "")
      )
    
    
  })
  
  #-------------------------------
  
  output$plot5 <- renderPlotly({
    line_chart_data() %>%
      filter(EMP %in% unique(ranked_data()$EMP)) %>%
      plot_ly(
        x = ~ as.integer(Fiscal.Year),
        y = ~ as.integer(Approval_Count),
        color = ~EMP,
        mode = "lines",
        line = list(width = 2)  # Adjust the line width as needed
      )
  })
  
  
  
  
  
  data_scat_plot <- reactive({ filter(data_scatter_plot, DEPARTMENT == input$var_dept) })
  
  
  # output$plot6 <- renderPlotly({
  #   
  #   plot_ly(data_scat_plot(), x = ~ Approval_Count, y = ~app_growth , text = ~EMP, type = 'scatter', mode = 'markers', color = ~ EMP, colors = 'Reds',
  #           marker = list(size = ~ag, opacity = 0.8 , sizeref = 2) ,  showlegend = FALSE) %>%
  #     layout(
  #       title = "What are the other hot companies with to which you should apply?"   , 
  #       xaxis = list(title = "")
  #     )
  #   
  # })
  
  
  
  output$plot6 <- renderPlotly({
    
    plot_ly(data_scat_plot(), x = ~ Approval_Count, y = ~ag , text = ~EMP, type = 'scatter', mode = 'markers', color = ~ EMP, colors = 'Reds',
            marker = list(size = ~Approval_Count/100, opacity = 0.8 , sizeref = 2) ,  showlegend = FALSE) %>%
      layout(
        title = "What are the other hot companies with to which you should apply?"   , 
        xaxis = list(title = "No. of H1B Sponsorships") ,  yaxis = list(title = "%Growth - Last Year vs Current Year")
      )
    
  })
  
  
  
  
}




