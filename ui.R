library(shiny)

threshold_default <- 0.5

ui <- fluidPage(
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
                 
                 textInput("true_variable_label", label = "Positive class value:", value =
                             "1"),
                 
                 textInput("predicted_scores", label = "Predicted score column name:", value =
                             "prediction")
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
                 )
               )
             ))
  ),
  
  absolutePanel(
    top = '10%',
    left = '30%',
    width = 480,
    draggable = T,
    wellPanel(
      helpText("Select a threshold value"),
      
      sliderInput(
        "threshold_slider",
        label = "Range of interest:",
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
    '<b>Disclaimer: This is a prototype tool to support research. Validate your findings. </b><br/>This code is public on <a href="https://github.com/gkoutos-group/threshold_picker/">https://github.com/gkoutos-group/threshold_picker/</a>, for details contact <a href="mailto:V.RothCardoso@bham.ac.uk">V.RothCardoso@bham.ac.uk</a>.'
  )
)
