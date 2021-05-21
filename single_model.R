
library(ggplot2)
library(ModelMetrics)
library(PRROC)
library(pROC)

.get_number_predicted_positive <- function(df, input) {
  return(sum(df[[input$predicted_scores]] > input$threshold_slider))
}

.threshold_to_precision <- function(df, input) {
  if (.get_number_predicted_positive(df, input) == 0) {
    return(1)
  }
  return(ppv(df[[input$true_variable]], df[[input$predicted_scores]], cutoff =
               input$threshold_slider))
}

.threshold_to_npv <- function(df, input) {
  return(npv(df[[input$true_variable]], df[[input$predicted_scores]], cutoff =
               input$threshold_slider))
}

.threshold_to_nne <- function(df, input) {
  return(1 / ppv(df[[input$true_variable]], df[[input$predicted_scores]], cutoff =
                   input$threshold_slider))
}

.threshold_to_sensitivity <- function(df, input) {
  return(tpr(df[[input$true_variable]], df[[input$predicted_scores]], cutoff =
               input$threshold_slider))
}

.threshold_to_specificity <- function(df, input) {
  return(tnr(df[[input$true_variable]], df[[input$predicted_scores]], cutoff =
               input$threshold_slider))
}

.threshold_to_dor <- function(df, input) {
  cm <- .confusion_matrix(input$threshold_slider, df, input)
  return((cm[2,2]/cm[1,2])/(cm[2,1]/cm[1,1]))
}

.threshold_to_f1score <- function(df, input) {
  return(f1Score(df[[input$true_variable]], df[[input$predicted_scores]], cutoff =
                   input$threshold_slider))
}

.threshold_to_accuracy <- function(df, input) {
  return(sum(df[[input$true_variable]] == as.integer(df[[input$predicted_scores]] > input$threshold_slider)) / nrow(df))
}

# return the different points of significant threshold
.important_points <- function(df, input) {
  return(sort(c(0, 1, unique(
    round(df[[input$predicted_scores]], digits = 2)
  ))))
}

# predict the datapoitns for a given threshold
.predict_for_threshold <- function(threshold, df, input) {
  return(as.integer(df[[input$predicted_scores]] > threshold))
}

# returns the confusion matrix
.confusion_matrix <- function(threshold, df, input) {
  return(confusionMatrix(df[[input$true_variable]], ".predict_for_threshold"(threshold, df, input)))
}

# calculates the lifetime cost
.cost_overall <- function(threshold, df, input) {
  cm <- .confusion_matrix(threshold, df, input)
  
  return(cm[2, 2] * input$tp_cost +
           cm[2, 1] * input$fp_cost +
           cm[1, 1] * input$tn_cost +
           cm[1, 2] * input$fn_cost)
}

# calculates the overal financial cost
.cost_cash_overall <- function(threshold, df, input) {
  cm <- .confusion_matrix(threshold, df, input)
  
  return(
    cm[2, 2] * input$tp_cost_cash +
      cm[2, 1] * input$fp_cost_cash +
      cm[1, 1] * input$tn_cost_cash +
      cm[1, 2] * input$fn_cost_cash
  )
}

# return the plot with the costs
.plot_cost <- function(df, input) {
  x = .important_points(df, input)
  y = sapply(x, function(x){.cost_overall(x, df, input)}) # obtain lifetime vector
  y_cash = sapply(x, function(x){.cost_cash_overall(x, df, input)}) # obtain cash vector
  
  # budget lines
  budget <- input$budget * nrow(df)
  budget_cash <- input$budget_cash * nrow(df)
  
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
.plot_precision_recall <- function(df, input) {
  c0 <-
    df[[input$predicted_scores]][df[[input$true_variable]] == 0]
  c1 <-
    df[[input$predicted_scores]][df[[input$true_variable]] == 1]
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
    x = .threshold_to_sensitivity(df, input),
    y = .threshold_to_precision(df, input),
    pch = 17,
    col = 'red'
  )
  if (.get_number_predicted_positive(df, input) == 0) {
    text(0.5,
         0.5,
         'Plot may not be reliable (due to number of predicted positive is 0)')
  }
  return(p)
}

# return the precision - recall plot
.plot_nne_recall <- function(df, input) {
  c0 <-
    df[[input$predicted_scores]][df[[input$true_variable]] == 0]
  c1 <-
    df[[input$predicted_scores]][df[[input$true_variable]] == 1]
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
    x = .threshold_to_sensitivity(df, input),
    y = 1 / .threshold_to_precision(df, input),
    pch = 17,
    col = 'red'
  )
  if (.get_number_predicted_positive(df, input) == 0) {
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
.plot_hist_pred <- function(df, input) {
  e <- cbind(df)
  
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

.plot_f1_score <- function(df, input) {
  x = .important_points(df, input)
  y = sapply(x, function(x) {
    f1Score(df[[input$true_variable]], df[[input$predicted_scores]], cutoff =
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

# base plot so doesnt recalculate when moving cutoff
.base_auc_plot <- function(df, input) {
  set.seed(input$boot.seed)
  model1_roc <- roc(df[[input$true_variable]], df[[input$predicted_scores]], direction='<', levels=c(0, 1), plot=F)
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
}

.get_auc_ci <- function(df, input) {
  roc <- roc(df[[input$true_variable]], df[[input$predicted_scores]], direction='<', levels=c(0, 1), plot=F)
  set.seed(input$boot.seed)
  ci <- ci.auc(roc, boot.n=input$boot.n)
  return(paste0('AUC: ', round(ci[2], 5), ' [95% CI ', round(ci[1], 5), '-', round(ci[3], 5), ']'))
}

