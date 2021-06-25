library(shiny)
library(pROC)
library(ModelMetrics)
library(PRROC)
library(ggplot2)
library(plyr)
library(tidyverse)
library(readxl)

VERSION <- 'threshold_picker-alpha'

if(!exists(".compare_models_loaded", mode='function')) source(here::here('compare_models.R'))

if(!exists(".base_auc_plot", mode='function')) source(here::here('single_model.R'))

if(!exists(".loaded_cutpointr_wrapper", mode='function')) source(here::here('cutpointr_wrapper.R'))

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
  
  # base plot so doesnt recalculate when moving cutoff
  base_auc_plot <- reactive({
    p <- .base_auc_plot(df(), input)
    return(p)
  })
  # plot the auc
  plot_auc <- function() {
    p <- base_auc_plot() +
      geom_point(aes(x=1-.threshold_to_specificity(df(), input), 
                     y=.threshold_to_sensitivity(df(), input)), 
                 colour="blue")
    return(p)
  }
  
  get_basic_info <- function() {
    return(paste(
      paste('Threshold:', input$threshold_slider),
      paste(
        'Threshold life cost:',
        .cost_overall(input$threshold_slider, df(), input)
      ),
      paste(
        'Threshold financial cost:',
        .cost_cash_overall(input$threshold_slider, df(), input)
      ),
      sep = '\n'
    ))
  }
  
  get_set_of_metrics <- function() {
    return(paste(
      paste('Threshold:', input$threshold_slider),
      paste('Accuracy:', .threshold_to_accuracy(df(), input)),
      paste(
        'Precision (PPV):',
        if (.get_number_predicted_positive(df(), input) == 0)
          "NA (no positive prediction)"
        else
          .threshold_to_precision(df(), input)
      ),
      paste(
        'Negative Predictive Value (NPV):',
        .threshold_to_npv(df(), input)
      ),
      paste(
        'Sensitivity (TPR, Recall, Probability of detection, Power):',
        .threshold_to_sensitivity(df(), input)
      ),
      paste(
        'Specificity (TNR, Selectivity):',
        .threshold_to_specificity(df(), input),
        "*"
      ),
      paste(
        'Diagnostic Odds Ratio (DOR):',
        .threshold_to_dor(df(), input)
      ),
      paste('F-1 score:', .threshold_to_f1score(df(), input)),
      sep = '\n'
    ))
  }
  
  get_costs_calc <- function() {
    cm <- .confusion_matrix(input$threshold_slider, df(), input)
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
    cm <- .confusion_matrix(input$threshold_slider, df(), input)
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
    .get_auc_ci(df(), input)
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
    d <- .confusion_matrix(input$threshold_slider, df(), input)
    colnames(d) <- c('Label Negative', 'Label Positive')
    row.names(d) <- c('Predicted Negative', 'Predicted Positive')
    d
  }, rownames = T)
  
  # plots
  output$auc_plot <- renderPlot({
    plot_auc()
  })
  output$cost_plot <- renderPlot({
    .plot_cost(df(), input)
  })
  output$precision_recall <- renderPlot({
    .plot_precision_recall(df(), input)
  })
  output$nne_recall <- renderPlot({
    .plot_nne_recall(df(), input)
  })
  output$f1_score <- renderPlot({
    .plot_f1_score(df(), input)
  })
  output$hist_pred <- renderPlot({
    .plot_hist_pred(df(), input)
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
    tmp2 <- input$initial_model_label
    updateTextInput(session, "predicted_scores", value=input$predicted_scores_2)
    updateTextInput(session, "initial_model_label", value=input$updated_model_label)
    updateTextInput(session, "predicted_scores_2", value=tmp)
    updateTextInput(session, "updated_model_label", value=tmp2)
  })
  
  compare_aucs_reactive <- reactive({
    return(compare_aucs(df(), input$true_variable, input$predicted_scores, input$predicted_scores_2, 
                        boot.n = input$boot.n, boot.seed = input$boot.seed,
                        label_initial=input$initial_model_label, label_updated=input$updated_model_label))
  })
  
  output$comparison_auc_plot <- renderPlot({
    if(input$use_predicted_2) {
      compare_aucs_reactive()$plot
    }
  })
  
  output$comparison_aucs_text <- renderPrint({
    if(input$use_predicted_2) {
      compare_aucs_reactive()$pval
    }
  })
  
  reclass_output <- reactive({
    if(input$use_predicted_2) {
      r <- compare_models(df(), input$true_variable, 
                          input$predicted_scores, input$predicted_scores_2, cutoff=as.numeric(unlist(strsplit(input$cutoffs, ','))))
    } else {
      r <- NULL
    }
    return(r)
  })
  
  output$comparison_heatmap_both <- renderPlot({
    plot_reclassification(reclass_output()$tab_both, title='Overall reclassification', 
                          diagonal='#e1e6ed', low="#e1e6ed", high="#e1e6ed",
                          label_initial=input$initial_model_label, label_updated=input$updated_model_label)
  })
  
  output$comparison_heatmap_present <- renderPlot({
    plot_reclassification(reclass_output()$tab_present, title='Cases reclassification',
                          invert_colors=F,
                          label_initial=input$initial_model_label, label_updated=input$updated_model_label)
  })
  
  output$comparison_heatmap_absent <- renderPlot({
    plot_reclassification(reclass_output()$tab_absent, title='Controls reclassification', 
                          invert_colors=T,
                          label_initial=input$initial_model_label, label_updated=input$updated_model_label)
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
  
  # based download name
  base_download_name <- reactive({
    dfn <- "sample"
    if(!is.null(input$dataset$datapath)) {
      dfn <- input$dataset$name
    }
    
    return(paste(format(Sys.time(), "%Y%m%d"), dfn, VERSION, sep="_"))
  })
  
  output$download_table_metrics <- downloadHandler(
    filename = function() {
      paste0(base_download_name(), "_complete_table_metrics.csv")
    },
    content = function(file) {
      write.csv(.table_all_metrics(df(), input), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  handling_events <- FALSE
  
  # observe events then change method or threshold as needed
  observeEvent(input$threshold_slider, {
    if(handling_events) return()
    updateSelectInput(session, "cutpoint_method",
                      label = "Method:",
                      choices = names(methods_available),
                      selected = names(methods_available)[1]
    )}, priority=2)
  
  observeEvent(input$threshold_slider, {
    handling_events <<- F
    }, priority=1)
  
  observeEvent({list(input$cutpoint_method, input$cutpoint_metric)}, {
    if(input$cutpoint_method != names(methods_available)[1]) {
      handling_events <<- T
      
      cmethod <- methods_available[[input$cutpoint_method]]
      cmetric <- metrics_available[[input$cutpoint_metric]]
      
      c <- cutpointr_best_point(data=df(),
                                x=input$predicted_scores,
                                class=input$true_variable,
                                direction='<=',
                                pos_class=1,
                                method=methods_available[[input$cutpoint_method]],
                                metric=metrics_available[[input$cutpoint_metric]])

      
      updateSliderInput(session, "threshold_slider", value = c,
                        min = 0, max = 1, step = 0.01)
    }
  }, priority=0)
}