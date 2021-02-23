library(shiny)


### to deploy ###
this_file_working_dir <- 'C:\\Users\\vroth\\Google Drive\\Projetos\\UoB\\threshold_picker'
#print(this_file_working_dir)
setwd(this_file_working_dir)
#library(rsconnect)
#deployApp()


if(FALSE) { # debug
  library(profvis)
  ### to profile ###
  profvis({
    runApp('C:/Users/vroth/Google Drive/Projetos/UoB/threshold_picker', display.mode='normal')
  })
} else { # normal execution
  source("ui.R")
  source("server.R")
  shinyApp(ui, server)
}

