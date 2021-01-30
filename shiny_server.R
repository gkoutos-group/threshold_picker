library(shiny)

#this_file_working_dir <- 'C:\\Users\\vroth\\Desktop\\ThresholdLoss'
#print(this_file_working_dir)
#setwd(this_file_working_dir)

#library(rsconnect)
#deployApp()

source('ui.R')
source('server.R')

shinyApp(ui, server)
