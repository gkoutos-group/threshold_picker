library(shiny)
library(pROC)
library(ModelMetrics)
library(PRROC)
library(ggplot2)
library(plyr)
library(tidyverse)
library(readxl)

source(here::here('compare_models.R'))

server <- function(input, output, session) {
  # returns the dataset
  df <- reactive({
    if (is.null(input$file_input$datapath)) {
      tdf <- read.csv('sample_dataset.csv')
    } else if(grepl(".xls$|.xlsx$", input$file_input$datapath)) {
      tdf <- read_excel(input$file_input$datapath, sheet=ifelse(input$sheet == "", 1, input$sheet))
    } else {
      tdf <- read.csv(input$file_input$datapath)
    }
    if(input$true_variable %in% colnames(tdf)) {
      tdf <- tdf[!is.na(tdf[[input$true_variable]]), ]
      tdf[[input$true_variable]] <-
        as.integer(tdf[[input$true_variable]] == input$true_variable_label)
      if(input$predicted_scores %in% colnames(tdf)) {
        tdf <- tdf[!is.na(tdf[[input$predicted_scores]]), ]
      }
    }
    return(tdf)
  })
  
  check_column_in_df <- function(c) {
    if(!c %in% colnames(df())) {
      "Invalid column"
    }
  }
  
  output$true_variable_info <- renderText({
    check_column_in_df(input$true_variable)
  })
  
  output$predicted_scores_info <- renderText({
    check_column_in_df(input$predicted_scores)
  })
  
  output$predicted_scores_2_info <- renderText({
    if(input$use_predicted_2) {
      check_column_in_df(input$predicted_scores_2)
    }
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
  
  threshold_to_npv <- function() {
    return(npv(df()[[input$true_variable]], df()[[input$predicted_scores]], cutoff =
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
  
  threshold_to_dor <- function() {
    cm <- confusion_matrix(input$threshold_slider)
    return((cm[2,2]/cm[1,2])/(cm[2,1]/cm[1,1]))
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
  
  # base plot so doesnt recalculate when moving cutoff
  base_auc_plot <- reactive({
    set.seed(input$boot.seed)
    model1_roc <- roc(df()[[input$true_variable]], df()[[input$predicted_scores]], direction='<', levels=c(0, 1), plot=F)
    ci_obj_m1 <- fix_ci_df__(ci.se(model1_roc, specificities=seq(0, 1, 0.01), boot.n=input$boot.n))
    
    p <- ggroc(list(model1=model1_roc), legacy.axes=T) + 
      theme_classic() + 
      geom_abline(slope=1, intercept = 0, linetype = "dashed", alpha=0.7, color = "grey") + 
      coord_equal()  +
      theme(legend.position = "none") 
    
    ci_obj_m1$sp <- 1-ci_obj_m1$sp
    
    p <- p + xlab('1 - Specificity') + ylab('Sensitivity')
    p <- p + geom_ribbon(data=ci_obj_m1, aes(x=sp, ymin=se.low, ymax=se.high), fill=2, alpha=0.2, inherit.aes=F)
    return(p)
  })
  
  # plot the auc
  plot_auc <- function() {
    p <- base_auc_plot() +
      geom_point(aes(x=1-threshold_to_specificity(), 
                     y=threshold_to_sensitivity()), 
                 colour="blue")
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
  
  get_auc_ci <- function() {
    roc <- roc(df()[[input$true_variable]], df()[[input$predicted_scores]], direction='<', levels=c(0, 1), plot=F)
    set.seed(input$boot.seed)
    ci <- ci.auc(roc, boot.n=input$boot.n)
    return(paste0('AUC: ', round(ci[2], 5), ' [95% CI ', round(ci[1], 5), '-', round(ci[3], 5), ']'))
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
        'Negative Predictive Value (NPV):',
        threshold_to_npv()
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
      paste(
        'Diagnostic Odds Ratio (DOR):',
        threshold_to_dor()
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
      paste0("Using sample data with `", input$predicted_scores, "` scores.")
    } else {
      paste0("Using uploaded file ", input$file_input$name, " with `", input$predicted_scores, "` scores.")
    }
  }
  
  # info/text
  output$dataset_info <- renderText({
    loaded_file_output()
  })
  output$auc_and_ci <- renderText({
    get_auc_ci()
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
  
  
  js$disableTab('Comparison')
  observeEvent(input$use_predicted_2, {
    if(input$use_predicted_2) {
      js$enableTab("Comparison")
    } else {
      js$disableTab("Comparison")
    }
    # switch to tab2
    updateTabsetPanel(session, "navbar", "Comparison")
  })
  
  observeEvent(input$predicted_scores_swap, {
    tmp <- input$predicted_scores
    updateTextInput(session, "predicted_scores", value=input$predicted_scores_2)
    updateTextInput(session, "predicted_scores_2", value=tmp)
  })
  
  
  output$comparison_auc_plot <- renderPlot({
    if(input$use_predicted_2) {
      compare_aucs(df(), input$true_variable, input$predicted_scores, input$predicted_scores_2, boot.n = input$boot.n, boot.seed = input$boot.seed)
    }
  })
  
  reclass_output <- reactive({
    if(input$use_predicted_2) {
      r <- compare_models(df(), input$true_variable, input$predicted_scores, input$predicted_scores_2, cutoff=as.numeric(unlist(strsplit(input$cutoffs, ','))))
    } else {
      r <- NULL
    }
    return(r)
  })
  
  output$comparison_heatmap_both <- renderPlot({
    plot_reclassification(reclass_output()$tab_both, title='Overall reclassification')
  })
  
  output$comparison_heatmap_present <- renderPlot({
    plot_reclassification(reclass_output()$tab_present, title='Present reclassification')
  })
  
  output$comparison_heatmap_absent <- renderPlot({
    plot_reclassification(reclass_output()$tab_absent, title='Absent reclassification')
  })
  
  output$comparison_table_absent <- renderTable({
    reclass_output()$tab_absent
  }, rownames=T)
  
  output$comparison_table_present <- renderTable({
    reclass_output()$tab_present
  }, rownames=T)
  
  output$comparison_nri_categorical_output <- renderText({
    paste0(reclass_output()$nri_cat$value, paste0(' [', reclass_output()$nri_cat$ci95_low, '-', reclass_output()$nri_cat$ci95_high, ']'), paste0(' (', reclass_output()$nri_cat$pval, ')'))
  })
  
  output$comparison_nri_numerical_output <- renderText({
    paste0(reclass_output()$nri_cont$value, paste0(' [', reclass_output()$nri_cont$ci95_low, '-', reclass_output()$nri_cont$ci95_high, ']'), paste0(' (', reclass_output()$nri_cont$pval, ')'))
  })
  
  output$comparison_idi_output <- renderText({
    paste0(reclass_output()$idi$value, paste0(' [', reclass_output()$idi$ci95_low, '-', reclass_output()$idi$ci95_high, ']'), paste0(' (', reclass_output()$idi$pval, ')'))
  })
}