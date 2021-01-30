library(shiny)
library(pROC)
library(ModelMetrics)

server <- function(input, output) {
  # returns the dataset
  df <- reactive({
    if(is.null(input$file_input$datapath)) {
      read.csv('sample_dataset.csv')
    } else {
      read.csv(input$file_input$datapath)
    }
  })
  
  # dummy function to obtain the current execution time as timestamp followed by ms
  gen_report_id <- function() {
    str_replace_all(paste(as.numeric(as.POSIXct(as.character(Sys.time())))), '[.]', '')
  }
  
  threshold_to_sensitivity <- function() {
    return(tpr(df()[[input$true_variable]], df()[[input$predicted_scores]], cutoff=input$threshold_slider))
  }
  
  threshold_to_specificity <- function() {
    return(tnr(df()[[input$true_variable]], df()[[input$predicted_scores]], cutoff=input$threshold_slider))
  }
  
  # predict the datapoitns for a given threshold
  predict_for_threshold <- function(threshold) {
    return(as.integer(df()[[input$predicted_scores]] >= threshold))
  }
  
  # calculates the lifetime cost
  cost_overall <- function(threshold) {
    cm <- confusion_matrix(threshold)
    
    return(cm[2, 2] * input$tp_cost + 
             cm[2, 1] * input$fp_cost + 
             cm[1, 1] * input$tn_cost + 
             cm[1, 2] * input$fn_cost)
  }
  
  # calculates the overal financial cost
  cost_cash_overall <- function(threshold) {
    cm <- confusion_matrix(threshold)
    
    return(cm[2, 2] * input$tp_cost_cash + 
             cm[2, 1] * input$fp_cost_cash + 
             cm[1, 1] * input$tn_cost_cash + 
             cm[1, 2] * input$fn_cost_cash)
  }
  
  # returns the confusion matrix
  confusion_matrix <- function(threshold) {
    return(confusionMatrix(df()[[input$true_variable]], predict_for_threshold(threshold)))
  }
  
  # plot the auc
  plot_auc <- function() {
    roc_full_resolution <- roc(df()[[input$true_variable]], df()[[input$predicted_scores]], levels=c(0, 1))
    #rounded_scores <- round(df[[predicted_scores]], digits=1)
    #roc_rounded <- roc(df[[true_variable]], rounded_scores)
    p <- plot(roc_full_resolution, print.auc=TRUE)
    #lines(roc_rounded, col="red", type='b')
    #text(0.4, 0.43, labels=sprintf("AUC: %0.3f", auc(roc_rounded)), col="red")
    points(x=threshold_to_specificity(), y=threshold_to_sensitivity(), pch=17, col='red')
    return(p)
  }
  
  # return the different points of significant threshold
  important_points <- function() {
    return(sort(c(0, 1, unique(df()[[input$predicted_scores]]))))
  }
  
  # return the plot with the costs
  plot_cost <- function() {
    x = important_points()
    y = sapply(x, cost_overall) # obtain lifetime vector
    y_cash = sapply(x, cost_cash_overall) # obtain cash vector
    
    # budget lines
    budget <- input$budget * nrow(df())
    budget_cash <- input$budget_cash * nrow(df())
    
    # y axis boundaries
    ymi <- min(min(y), min(y_cash), budget, budget_cash)
    yma <- max(max(y), max(y_cash), budget, budget_cash)

    # plot main plot
    p <- plot(x, y, type='o', main='Loss curve', xlab='Threshold', ylab='Cost', col='blue', ylim=c(ymi, yma))
    # plot financial costs
    lines(x, y_cash, type='o', col='green', lty=2)
    # vertical line for the slider
    abline(v=input$threshold_slider, col='red')
    
    # horizontal lines
    abline(h=budget, col='chocolate4')
    abline(h=budget_cash, col='chartreuse4', lty=2)
    
    # legends
    legend("topright", 
           legend=c('Life', 'Financial', 'Lifetime budget', 'Financial budget', 'Selected threshold'), 
           col=c('blue', 'green', 'chocolate4', 'chartreuse4', 'red'), 
           lty=c(1, 2, 1, 2, 1),
           cex=0.8)
    
    
    
    return(p)
  }
  
  #output$costs <- renderText({
  #  paste(paste("TP cost:",
  #              input$tp_cost), 
  #        paste("FP cost:",
  #              input$fp_cost), 
  #        paste("TN cost:",
  #              input$tn_cost), 
  #        paste("FN cost:",
  #              input$fn_cost), 
  #        sep='\n')
  #})
  
  output$costs <- renderText({ 
    cm <- confusion_matrix(input$threshold_slider)
    
    paste(paste0("TP cost: ",
                 input$tp_cost,
                 " x ",
                 cm[2, 2],
                 " = ",
                 cm[2, 2] * input$tp_cost), 
          paste0("FP cost: ",
                 input$fp_cost,
                 " x ",
                 cm[2, 1],
                 " = ",
                 cm[2, 1] * input$fp_cost), 
          paste0("TN cost: ",
                 input$tn_cost,
                 " x ",
                 cm[1, 1],
                 " = ",
                 cm[1, 1] * input$tn_cost), 
          paste0("FN cost: ",
                 input$fn_cost,
                 " x ",
                 cm[1, 2],
                 " = ",
                 cm[1, 2] * input$fn_cost), 
          sep='\n')
  })
  
  output$costs_cash <- renderText({ 
    cm <- confusion_matrix(input$threshold_slider)
    
    paste(paste0("TP cost: ",
                 input$tp_cost_cash,
                 " x ",
                 cm[2, 2],
                 " = ",
                 cm[2, 2] * input$tp_cost_cash), 
          paste0("FP cost: ",
                 input$fp_cost_cash,
                 " x ",
                 cm[2, 1],
                 " = ",
                 cm[2, 1] * input$fp_cost_cash), 
          paste0("TN cost: ",
                 input$tn_cost_cash,
                 " x ",
                 cm[1, 1],
                 " = ",
                 cm[1, 1] * input$tn_cost_cash), 
          paste0("FN cost: ",
                 input$fn_cost_cash,
                 " x ",
                 cm[1, 2],
                 " = ",
                 cm[1, 2] * input$fn_cost_cash), 
          sep='\n')
  })
  
  output$auc_plot <- renderPlot({plot_auc()})
  
  loaded_file_output <- function() {
    if(is.null(input$file_input$datapath)) {
      "Using sample data"
    } else {
      paste("Using uploaded file:", input$file_input$name)
    }
  }
  
  output$dataset_info <- renderText({loaded_file_output()})
  
  output$info <- renderText({paste(paste('Threshold:', input$threshold_slider),
                                   paste('Sensitivity:', 
                                         threshold_to_sensitivity()),
                                   paste('Specificity:', 
                                         threshold_to_specificity()),
                                   paste('Threshold cost:',
                                         cost_overall(input$threshold_slider)),
                                   sep='\n')})
  
  output$cost_plot <- renderPlot({plot_cost()})
  
  output$confusion_matrix <- renderTable({
    d <- confusion_matrix(input$threshold_slider)
    colnames(d) <- c('Label Negative', 'Label Positive')
    row.names(d) <- c('Predicted Negative', 'Predicted Positive')
    d
  }, rownames=T)
}