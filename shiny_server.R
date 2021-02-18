library(shiny)
library(profvis)

### to deploy ###
#this_file_working_dir <- 'C:\\Users\\vroth\\Desktop\\ThresholdLoss'
#print(this_file_working_dir)
#setwd(this_file_working_dir)
#library(rsconnect)
#deployApp()

### to run ###
#shinyApp(ui, server)

### to profile ###
profvis({
  runApp('C:/Users/vroth/Google Drive/Projetos/UoB/threshold_picker', display.mode='normal')
})
