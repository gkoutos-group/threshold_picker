library(shiny)
library(shinyjs)

threshold_default <- 0.5


jscode_comparison_enable_disable <- "
shinyjs.disableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.disableInput = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.bind('click.tab', function(e) {
    e.preventDefault();
    return false;
  });
  tab.addClass('disabled');
}

shinyjs.enableTab = function(name) {
  var tab = $('.nav li a[data-value=' + name + ']');
  tab.unbind('click.tab');
  tab.removeClass('disabled');
}
"

css <- "
.nav li a.disabled {
  background-color: #aaa !important;
  color: #333 !important;
  cursor: not-allowed !important;
  border-color: #aaa !important;
}"

ui <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode_comparison_enable_disable, functions = c('disableTab','enableTab')),
  inlineCSS(css),
  
  titlePanel("Threshold picker"),
  
  span(
    textOutput("dataset_info"),
    style = "color:blue",
    align = 'right'
  ),
  tabsetPanel(
    type = "tabs",
    tabPanel("Plots",
             fluidRow(
               column(width = 8,
                      fluidRow(
                        splitLayout(
                          cellWidths = c("50%", "50%"),
                          plotOutput(
                            "auc_plot",
                            click = "plot_click",
                            dblclick = "plot_dblclick",
                            hover = "plot_hover",
                            brush = "plot_brush"
                          ),
                          plotOutput(
                            "hist_pred",
                            click = "plot_click",
                            dblclick = "plot_dblclick",
                            hover = "plot_hover",
                            brush = "plot_brush"
                          )
                        ),
                        splitLayout(
                          cellWidths = c("50%", "50%"),
                          plotOutput(
                            "precision_recall",
                            click = "plot_click",
                            dblclick = "plot_dblclick",
                            hover = "plot_hover",
                            brush = "plot_brush"
                          ),
                          plotOutput(
                            "nne_recall",
                            click = "plot_click",
                            dblclick = "plot_dblclick",
                            hover = "plot_hover",
                            brush = "plot_brush"
                          )
                        ),
                        splitLayout(
                          cellWidths = c("50%", "50%"),
                          plotOutput(
                            "f1_score",
                            click = "plot_click",
                            dblclick = "plot_dblclick",
                            hover = "plot_hover",
                            brush = "plot_brush"
                          ),
                          plotOutput(
                            "cost_plot",
                            click = "plot_click",
                            dblclick = "plot_dblclick",
                            hover = "plot_hover",
                            brush = "plot_brush"
                          )
                        )
                      )),
               column(
                 width = 4,
                 helpText("Basic info"),
                 verbatimTextOutput("info"),
                 
                 fluidRow(
                   column(
                     3,
                     helpText("Breakdown of lifetime costs for threshold"),
                     verbatimTextOutput("costs")
                   ),
                   column(
                     3,
                     helpText("Breakdown of resources estimation for threshold"),
                     verbatimTextOutput("costs_cash")
                   ),
                   column(
                     6,
                     helpText("Confusion matrix for threshold"),
                     tableOutput('confusion_matrix')
                   )
                 ),
                 
                 helpText('Performance Metrics'),
                 verbatimTextOutput('metrics'),
                 HTML(
                   "* ModelMetrics 1.2.2 has an issue with TNR calculation, this could slightly reduce the calculated value."
                 )
               )
             )),
    tabPanel("Comparison",
             
             fluidRow(
               column(width = 8,
                      fluidRow(
                        splitLayout(
                          cellWidths = c("50%", "50%"),
                          plotOutput("comparison_auc_plot"),
                          plotOutput("comparison_heatmap_both")
                        ),
                        splitLayout(
                          cellWidths = c("50%", "50%"),
                          plotOutput("comparison_heatmap_absent"),
                          plotOutput("comparison_heatmap_present")
                        ),
                        helpText("Present (positive) labels (updated model on columns)"),
                        tableOutput("comparison_table_present"),
                        
                        helpText("Absent (negative) labels (updated model on columns)"),
                        tableOutput("comparison_table_absent")
                      )),
               column(
                 width = 4,
                 helpText("Comparison outputs"),
                 
                 helpText("NRI Categorical [95% CI] (p-value)"),
                 verbatimTextOutput("comparison_nri_categorical_output"),
                 
                 helpText("NRI Numerical [95% CI] (p-value)"),
                 verbatimTextOutput("comparison_nri_numerical_output"),
                 
                 helpText("IDI [95% CI] (p-value)"),
                 verbatimTextOutput("comparison_idi_output")
               )
             )),
    tabPanel("Input file & settings",
             fluidRow(
               column(
                 3,
                 helpText("Input file"),
                 fileInput(
                   "file_input",
                   "Choose CSV File",
                   multiple = FALSE,
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv")
                 ),
                 
                 textInput("true_variable", label = "True class column name:", value =
                             "class"),
                 span(textOutput("true_variable_info"),
                      style = "color:red"),
                 
                 textInput("true_variable_label", label = "Positive class value:", value =
                             "1"),
                 
                 textInput("predicted_scores", label = "Predicted score column name (model 1):", value =
                             "prediction"),
                 span(textOutput("predicted_scores_info"),
                      style = "color:red"),
                 
                 checkboxInput("use_predicted_2", "Compare to another model?", value = FALSE),
                 
                 textInput("predicted_scores_2", 
                           label="Predicted score column name (model 2):",
                           value="prediction2"),
                 span(textOutput("predicted_scores_2_info"),
                      style = "color:red")
               ),
               column(
                 4,
                 helpText("Loss curve settings"),
                 fluidRow(splitLayout(
                   cellWidths = c("50%", "50%"),
                   numericInput("tp_cost",
                                label = "TP life",
                                value = 0),
                   numericInput("tp_cost_cash",
                                label = "TP resources",
                                value = 0)
                 )),
                 
                 fluidRow(splitLayout(
                   cellWidths = c("50%", "50%"),
                   numericInput("fp_cost",
                                label = "FP life",
                                value = 0),
                   numericInput("fp_cost_cash",
                                label = "FP resources",
                                value = 0)
                 )),
                 fluidRow(splitLayout(
                   cellWidths = c("50%", "50%"),
                   numericInput("tn_cost",
                                label = "TN life",
                                value = 0),
                   numericInput("tn_cost_cash",
                                label = "TN resources",
                                value = 0)
                 )),
                 
                 fluidRow(splitLayout(
                   cellWidths = c("50%", "50%"),
                   numericInput("fn_cost",
                                label = "FN life",
                                value = 0),
                   numericInput("fn_cost_cash",
                                label = "FN resources",
                                value = 0)
                 )),
                 br(),
                 fluidRow(splitLayout(
                   cellWidths = c("50%", "50%"),
                   numericInput("budget",
                                label = "Budget lifetime (p. person)",
                                value = 0),
                   numericInput("budget_cash",
                                label = "Budget resources (p. person)",
                                value = 0)
                 ))
               ),
               column(
                 4,
                 helpText("Plot settings"),
                 splitLayout(
                   cellWidths = c("50%", "50%"),
                   numericInput(
                     "hist_bins",
                     label = "Number of Histogram bins",
                     value = 30,
                     min = 1,
                     max = 1000
                   )
                 ),
                 helpText("Comparison settings"),
                 numericInput("boot.n",
                              label = "Number of bootstraps",
                              min=1,
                              value=10,
                              max=500),
                 textInput("cutoffs",
                           label="Cutt-offs for reclassification metrics",
                           value='0, 0.1, 0.3, 1')
               )
             ))
  ),
  
  absolutePanel(
    top = '10%',
    left = '30%',
    width = 480,
    draggable = T,
    wellPanel(
      sliderInput(
        "threshold_slider",
        label = "Select a threshold value:",
        min = 0,
        max = 1,
        value = threshold_default
      ),
      
      helpText('This window can be moved!')
    ),
    style = "opacity: 0.92"
  ),
  
  hr(),
  HTML(
    '<b>Disclaimer: This is a prototype tool to support research. Validate your findings. </b><br/>This code is public on <a href="https://github.com/gkoutos-group/threshold_picker/">https://github.com/gkoutos-group/threshold_picker/</a>. For details contact <a href="mailto:V.RothCardoso@bham.ac.uk">V.RothCardoso@bham.ac.uk</a>.'
  )
)
