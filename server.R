library(shiny)
library(pROC)
library(ModelMetrics)
library(PRROC)
library(ggplot2)
library(plyr)

server <- function(input, output) {
  # returns the dataset
  df <- reactive({
    if (is.null(input$file_input$datapath)) {
      tdf <- read.csv('sample_dataset.csv')
    } else {
      tdf <- read.csv(input$file_input$datapath)
    }
    tdf[[input$true_variable]] <-
      as.integer(tdf[[input$true_variable]] == input$true_variable_label)
    return(tdf)
  })
  
  # dummy function to obtain the current execution time as timestamp followed by ms
  gen_report_id <- function() {
    str_replace_all(paste(as.numeric(as.POSIXct(
      as.character(Sys.time())
    ))), '[.]', '')
  }
  
  get_number_predicted_positive <- reactive({
    return(sum(df()[[input$predicted_scores]] > input$threshold_slider))
  })
  
  threshold_to_precision <- function() {
    if (get_number_predicted_positive() == 0) {
      return(1)
    }
    return(ppv(df()[[input$true_variable]], df()[[input$predicted_scores]], cutoff =
                 input$threshold_slider))
  }
  
  threshold_to_nne <- function() {
    return(1 / ppv(df()[[input$true_variable]], df()[[input$predicted_scores]], cutoff =
                     input$threshold_slider))
  }
  
  threshold_to_sensitivity <- function() {
    return(tpr(df()[[input$true_variable]], df()[[input$predicted_scores]], cutoff =
                 input$threshold_slider))
  }
  
  threshold_to_specificity <- function() {
    return(tnr(df()[[input$true_variable]], df()[[input$predicted_scores]], cutoff =
                 input$threshold_slider))
  }
  
  threshold_to_f1score <- function() {
    return(f1Score(df()[[input$true_variable]], df()[[input$predicted_scores]], cutoff =
                     input$threshold_slider))
  }
  
  threshold_to_accuracy <- function() {
    return(sum(df()[[input$true_variable]] == as.integer(df()[[input$predicted_scores]] > input$threshold_slider)) / nrow(df()))
  }
  
  # predict the datapoitns for a given threshold
  predict_for_threshold <- function(threshold) {
    return(as.integer(df()[[input$predicted_scores]] > threshold))
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
    
    return(
      cm[2, 2] * input$tp_cost_cash +
        cm[2, 1] * input$fp_cost_cash +
        cm[1, 1] * input$tn_cost_cash +
        cm[1, 2] * input$fn_cost_cash
    )
  }
  
  # returns the confusion matrix
  confusion_matrix <- function(threshold) {
    return(confusionMatrix(df()[[input$true_variable]], predict_for_threshold(threshold)))
  }
  
  # plot the auc
  plot_auc <- function() {
    roc_full_resolution <-
      roc(df()[[input$true_variable]],
          df()[[input$predicted_scores]],
          levels = c(0, 1),
          direction = '<')
    #rounded_scores <- round(df[[predicted_scores]], digits=1)
    #roc_rounded <- roc(df[[true_variable]], rounded_scores)
    p <- plot(roc_full_resolution,
              main = 'AUC',
              print.auc = TRUE)
    #lines(roc_rounded, col="red", type='b')
    #text(0.4, 0.43, labels=sprintf("AUC: %0.3f", auc(roc_rounded)), col="red")
    points(
      x = threshold_to_specificity(),
      y = threshold_to_sensitivity(),
      pch = 17,
      col = 'red'
    )
    return(p)
  }
  
  # return the different points of significant threshold
  important_points <- function() {
    return(sort(c(0, 1, unique(
      round(df()[[input$predicted_scores]], digits = 2)
    ))))
  }
  
  # return the plot with the costs
  plot_cost <- function() {
    x = important_points()
    y = sapply(x, cost_overall) # obtain lifetime vector
    y_cash = sapply(x, cost_cash_overall) # obtain cash vector
    
    # budget lines
    budget <- input$budget * nrow(df())
    budget_cash <- input$budget_cash * nrow(df())
    
    par(mar = c(5, 5, 3, 5))
    # plot main plot
    ymi <- min(min(y), budget)
    yma <- max(max(y), budget)
    p <-
      plot(
        x,
        y,
        type = 'o',
        main = 'Loss curve',
        xlab = 'Threshold',
        ylab = 'Lifetime cost',
        col = 'blue',
        ylim = c(ymi, yma)
      )
    abline(h = budget, col = 'chocolate4')
    
    par(new = T)
    # plot financial costs
    ymi_cash <- min(min(y_cash), budget_cash)
    yma_cash <- max(max(y_cash), budget_cash)
    
    plot(
      x,
      y_cash,
      type = 'o',
      col = 'green',
      lty = 2,
      xaxt = 'n',
      yaxt = 'n',
      ylab = "",
      xlab = "",
      ylim = c(ymi_cash, yma_cash)
    )
    abline(h = budget_cash, col = 'chartreuse4', lty = 2)
    axis(side = 4)
    mtext('Financial cost', side = 4, line = 3)
    
    
    # vertical line for the slider
    abline(v = input$threshold_slider, col = 'red')
    
    # legends
    if (input$threshold_slider >= 0.5) {
      position_legend <- 'topleft'
    } else {
      position_legend <- 'topright'
    }
    legend(
      position_legend,
      legend = c(
        'Life',
        'Financial',
        'Lifetime budget',
        'Financial budget',
        'Selected threshold'
      ),
      col = c('blue', 'green', 'chocolate4', 'chartreuse4', 'red'),
      lty = c(1, 2, 1, 2, 1),
      cex = 0.8
    )
    
    return(p)
  }
  
  # return the precision - recall plot
  plot_precision_recall <- function() {
    c0 <-
      df()[[input$predicted_scores]][df()[[input$true_variable]] == 0]
    c1 <-
      df()[[input$predicted_scores]][df()[[input$true_variable]] == 1]
    pbase <- pr.curve(
      scores.class0 = c1,
      scores.class1 = c0,
      curve = T
    )
    p <-
      plot(
        x = pbase$curve[, 1],
        y = pbase$curve[, 2],
        type = 'l',
        main = 'Precision x Recall curve',
        xlab = 'Recall (Sensitivity)',
        ylab = 'Precision',
        ylim = c(0, 1),
        xlim = c(0, 1)
      )
    points(
      x = threshold_to_sensitivity(),
      y = threshold_to_precision(),
      pch = 17,
      col = 'red'
    )
    if (get_number_predicted_positive() == 0) {
      text(0.5,
           0.5,
           'Plot may not be reliable (due to number of predicted positive is 0)')
    }
    return(p)
  }
  
  # return the precision - recall plot
  plot_nne_recall <- function() {
    c0 <-
      df()[[input$predicted_scores]][df()[[input$true_variable]] == 0]
    c1 <-
      df()[[input$predicted_scores]][df()[[input$true_variable]] == 1]
    pbase <- pr.curve(
      scores.class0 = c1,
      scores.class1 = c0,
      curve = T
    )
    p <-
      plot(
        x = pbase$curve[, 1],
        y = 1 / pbase$curve[, 2],
        type = 'l',
        main = 'NNE x Recall curve',
        xlab = 'Recall (Sensitivity)',
        ylab = 'Number Needed to Evaluate (NNE)',
        xlim = c(0, 1)
      )
    points(
      x = threshold_to_sensitivity(),
      y = 1 / threshold_to_precision(),
      pch = 17,
      col = 'red'
    )
    if (get_number_predicted_positive() == 0) {
      text(
        0.5,
        min(1 / pbase$curve[, 2]) + (max(1 / pbase$curve[, 2]) - min(1 / pbase$curve[, 2])) /
          2,
        'Plot may not be reliable (due to number of predicted positive is 0)'
      )
    }
    return(p)
  }
  
  # histogram of predictions
  plot_hist_pred <- function() {
    e <- cbind(df())
    
    e[[input$true_variable]] <-
      as.integer(as.character(e[[input$true_variable]]))
    e$internals___ <-
      as.integer(e[[input$predicted_scores]] >= input$threshold_slider)
    e$internals_class___ <-
      as.integer(e$internals___ == e[[input$true_variable]])
    
    e$internals___ <- as.factor(e$internals___)
    e$internals_class___ <- as.factor(e$internals_class___)
    e$internals___ <-
      revalue(e$internals___, replace = c("0" = "N", "1" = "P"))
    e$internals_class___ <-
      revalue(e$internals_class___, replace = c("0" = "F", "1" = "T"))
    
    e$internals___ <- as.character(e$internals___)
    e$internals_class___ <- as.character(e$internals_class___)
    
    e$internals_class___ <-
      paste(e$internals_class___, e$internals___, sep = "")
    e$internals_class___ <-
      factor(e$internals_class___, levels = c("FN", "FP", "TN", "TP"))
    
    p <- ggplot(e,
                aes_string(x = input$predicted_scores,
                           fill = "internals_class___")) +
      geom_histogram(position = "stack", bins = input$hist_bins) +
      ggtitle("Histogram of predicted scores") + labs(x = "Predicted scores", fill =
                                                        "Class") +
      geom_vline(xintercept = input$threshold_slider,
                 color = "red") +
      theme_bw() + xlim(0, 1) + scale_fill_discrete(drop = FALSE)
    
    return(p)
  }
  
  plot_f1_score <- function() {
    x = important_points()
    y = sapply(x, function(x) {
      f1Score(df()[[input$true_variable]], df()[[input$predicted_scores]], cutoff =
                x)
    }) # obtain f1 score vector
    
    # plot main plot
    p <-
      plot(
        x,
        y,
        type = 'l',
        main = 'F1-score curve',
        xlab = 'Threshold',
        ylab = 'F1-score',
        col = 'blue',
        ylim = c(0, 1),
        xlim = c(0, 1)
      )
    # vertical line for the slider
    abline(v = input$threshold_slider, col = 'red')
    return(p)
  }
  
  get_basic_info <- function() {
    return(paste(
      paste('Threshold:', input$threshold_slider),
      paste(
        'Threshold life cost:',
        cost_overall(input$threshold_slider)
      ),
      paste(
        'Threshold financial cost:',
        cost_cash_overall(input$threshold_slider)
      ),
      sep = '\n'
    ))
  }
  
  get_set_of_metrics <- function() {
    return(paste(
      paste('Threshold:', input$threshold_slider),
      paste('Accuracy:', threshold_to_accuracy()),
      paste(
        'Precision (PPV):',
        if (get_number_predicted_positive() == 0)
          "NA (no positive prediction)"
        else
          threshold_to_precision()
      ),
      paste(
        'Sensitivity (TPR, Recall, Probability of detection, Power):',
        threshold_to_sensitivity()
      ),
      paste(
        'Specificity (TNR, Selectivity):',
        threshold_to_specificity(),
        "*"
      ),
      paste('F-1 score:', threshold_to_f1score()),
      sep = '\n'
    ))
  }
  
  get_costs_calc <- function() {
    cm <- confusion_matrix(input$threshold_slider)
    return(paste(
      paste0("TP: ",
             input$tp_cost,
             " x ",
             cm[2, 2],
             " = ",
             cm[2, 2] * input$tp_cost),
      paste0("FP: ",
             input$fp_cost,
             " x ",
             cm[2, 1],
             " = ",
             cm[2, 1] * input$fp_cost),
      paste0("TN: ",
             input$tn_cost,
             " x ",
             cm[1, 1],
             " = ",
             cm[1, 1] * input$tn_cost),
      paste0("FN: ",
             input$fn_cost,
             " x ",
             cm[1, 2],
             " = ",
             cm[1, 2] * input$fn_cost),
      sep = '\n'
    ))
  }
  
  get_costs_calc_cash <- function() {
    cm <- confusion_matrix(input$threshold_slider)
    return(paste(
      paste0(
        "TP: ",
        input$tp_cost_cash,
        " x ",
        cm[2, 2],
        " = ",
        cm[2, 2] * input$tp_cost_cash
      ),
      paste0(
        "FP: ",
        input$fp_cost_cash,
        " x ",
        cm[2, 1],
        " = ",
        cm[2, 1] * input$fp_cost_cash
      ),
      paste0(
        "TN: ",
        input$tn_cost_cash,
        " x ",
        cm[1, 1],
        " = ",
        cm[1, 1] * input$tn_cost_cash
      ),
      paste0(
        "FN: ",
        input$fn_cost_cash,
        " x ",
        cm[1, 2],
        " = ",
        cm[1, 2] * input$fn_cost_cash
      ),
      sep = '\n'
    ))
  }
  
  loaded_file_output <- function() {
    if (is.null(input$file_input$datapath)) {
      "Using sample data"
    } else {
      paste("Using uploaded file:", input$file_input$name)
    }
  }
  
  # info/text
  output$dataset_info <- renderText({
    loaded_file_output()
  })
  output$info <- renderText({
    get_basic_info()
  })
  output$costs <- renderText({
    get_costs_calc()
  })
  output$costs_cash <- renderText({
    get_costs_calc_cash()
  })
  output$metrics <- renderText({
    get_set_of_metrics()
  })
  
  # confusion matrix
  output$confusion_matrix <- renderTable({
    d <- confusion_matrix(input$threshold_slider)
    colnames(d) <- c('Label Negative', 'Label Positive')
    row.names(d) <- c('Predicted Negative', 'Predicted Positive')
    d
  }, rownames = T)
  
  # plots
  output$auc_plot <- renderPlot({
    plot_auc()
  })
  output$cost_plot <- renderPlot({
    plot_cost()
  })
  output$precision_recall <- renderPlot({
    plot_precision_recall()
  })
  output$nne_recall <- renderPlot({
    plot_nne_recall()
  })
  output$f1_score <- renderPlot({
    plot_f1_score()
  })
  output$hist_pred <- renderPlot({
    plot_hist_pred()
  })
}