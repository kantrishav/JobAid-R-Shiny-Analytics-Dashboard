
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
final_data_combined <- read.csv("H1B_COMBINED_DATA.csv")


final_data_combined <- final_data_combined %>% filter(is.na(Initial.Approvals) == FALSE)



#------------------------ DATA TRANSFORMATION FOR SCATTER PLOT----------------------------#
dept_map <- final_data_combined[,12:13]
data_2021 <- final_data_combined %>% filter(Fiscal.Year == 2021) %>%  group_by(EMP) %>% summarise(App_2021 =  sum(Initial.Approvals))
data_2022 <- final_data_combined %>% filter(Fiscal.Year == 2022) %>% group_by(EMP) %>% summarise(App_2022 =  sum(Initial.Approvals))
app_ly_cy <- merge(data_2021 , data_2022 )
app_ly_cy <- app_ly_cy %>% filter(App_2021 > 10 & App_2022 > 10 )  %>% mutate(app_growth = ((App_2022 - App_2021)/App_2021)*100 )
data_app_rej  <- final_data_combined %>% group_by(EMP) %>% summarise(Approval_Count = sum(Initial.Approvals) , Rejection_Count =  sum(Initial.Denials))
data_scatter_plot <- merge(app_ly_cy , data_app_rej)
data_scatter_plot <- merge(data_scatter_plot,dept_map)

#-----------------------------------------------------END HERE-----------------------------#



# data_2021 <- final_data_combined %>% filter(Fiscal.Year == 2021) %>%  group_by(EMP) %>% summarise(App_2021 =  sum(Initial.Approvals))
# data_2022 <- final_data_combined %>% filter(Fiscal.Year == 2022) %>% group_by(EMP) %>% summarise(App_2022 =  sum(Initial.Approvals))
# app_ly_cy <- merge(data_2021 , data_2022 )
# app_ly_cy <- app_ly_cy %>% filter(App_2021 > 100 & App_2022 > 100 )  %>% mutate(app_growth = ((App_2022 - App_2021)/App_2021)*100 )
# data_app_rej  <- final_data_combined %>% group_by(EMP) %>% summarise(Approval_Count = sum(Initial.Approvals) , Rejection_Count =  sum(Initial.Denials))
# data_scatter_plot <- merge(app_ly_cy , data_app_rej)
# 









#data_scatter_plot <- data_scatter_plot %>% filter(App_2021 > 100 & App_2022 > 100)

#View(data_scatter_plot)

data_scatter_plot[is.na(data_scatter_plot)] <- 0



data_scatter_plot <- data_scatter_plot %>% mutate(ag = abs(app_growth))

data_scatter_plot$ag <- round(data_scatter_plot$ag)

#plot_ly(data_scatter_plot, x = ~ Approval_Count, y = ~app_growth , text = ~EMP, type = 'scatter', mode = 'markers', color = ~ EMP, colors = 'Reds',
#        marker = list(size = ~ag, opacity = 0.8 , sizeref = 2))




data_rank <- final_data_combined 

data_cnt <- final_data_combined %>% filter(Fiscal.Year == 2023)

company_count <- nrow(table(unique(data_cnt$EMP)))


data_2023 <- final_data_combined %>% filter(Fiscal.Year == 2023)



yoy_growth <- (sum(data_2023$Initial.Approvals) -sum(data_2022$App_2022))  / sum(data_2022$App_2022) * 100



data_23 <- final_data_combined %>% filter(Fiscal.Year == 2023)
data_22 <- final_data_combined %>% filter(Fiscal.Year == 2022)







