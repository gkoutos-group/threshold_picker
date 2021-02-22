library(shiny)

threshold_default <- 0.5

ui <- fluidPage(
  titlePanel("Threshold picker"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file_input", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      textInput("true_variable", label="True class column name:", value="class"),
      
      textInput("predicted_scores", label="Predicted score column name:", value="prediction"),
      
      helpText("Select a threshold value"),
      
      sliderInput("threshold_slider", 
                  label = "Range of interest:",
                  min = 0, max = 1, value = threshold_default),
      
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("tp_cost", 
                                 label = "TP life",
                                 value = 0),
                    numericInput("tp_cost_cash", 
                                 label = "TP financial",
                                 value = 0))),
                    
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("fp_cost", 
                                 label = "FP life",
                                 value = 0),
                    numericInput("fp_cost_cash", 
                                 label = "FP financial",
                                 value = 0))),
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("tn_cost", 
                                 label = "TN life",
                                 value = 0),
                    numericInput("tn_cost_cash", 
                                 label = "TN financial",
                                 value = 0))),
      
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("fn_cost", 
                                 label = "FN life",
                                 value = 0),
                    numericInput("fn_cost_cash", 
                                 label = "FN financial",
                                 value = 0))),
      
      br(),
      
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"),
                    numericInput("budget", 
                                 label = "Budget lifetime (p. person)",
                                 value = 0),
                    numericInput("budget_cash", 
                                 label = "Budget financial (p. person)",
                                 value = 0))),
      
      #numericInput("threshold_input", label = h3("Threshold value"),
      #             value = threshold_obtained)
    ),
    
    mainPanel(
      span(textOutput("dataset_info"), style="color:blue", align='right'),
      tabsetPanel(type = "tabs",
                  tabPanel("Plots", fluidRow(
                    splitLayout(cellWidths = c("50%", "50%"),
                                plotOutput("auc_plot",
                                           click = "plot_click",
                                           dblclick = "plot_dblclick",
                                           hover = "plot_hover",
                                           brush = "plot_brush"
                                ),
                                plotOutput("cost_plot",
                                           click = "plot_click",
                                           dblclick = "plot_dblclick",
                                           hover = "plot_hover",
                                           brush = "plot_brush")
                    ),
                    splitLayout(cellWidths = c("50%", "50%"),
                                plotOutput("precision_recall",
                                           click = "plot_click",
                                           dblclick = "plot_dblclick",
                                           hover = "plot_hover",
                                           brush = "plot_brush"
                                ),
                                plotOutput("nne_recall",
                                           click = "plot_click",
                                           dblclick = "plot_dblclick",
                                           hover = "plot_hover",
                                           brush = "plot_brush")
                    ),
                    splitLayout(cellWidths = c("50%", "50%"),
                                plotOutput("f1_score",
                                           click = "plot_click",
                                           dblclick = "plot_dblclick",
                                           hover = "plot_hover",
                                           brush = "plot_brush"
                                ),
                                plotOutput("hist_pred",
                                           click = "plot_click",
                                           dblclick = "plot_dblclick",
                                           hover = "plot_hover",
                                           brush = "plot_brush")
                    ))),
                  tabPanel("Summary", helpText("Basic info"),
                           verbatimTextOutput("info"),
                           
                           fluidRow(column(3, 
                                           helpText("Breakdown of lifetime costs for threshold"),
                                           verbatimTextOutput("costs")),
                                    column(3, 
                                           helpText("Breakdown of financial costs for threshold"),
                                           verbatimTextOutput("costs_cash")),
                                    column(6, 
                                           helpText("Confusion matrix for threshold"),
                                           tableOutput('confusion_matrix'))),
                           
                           helpText('Performance Metrics'),
                           verbatimTextOutput('metrics'),
                           HTML("* ModelMetrics 1.2.2 has an issue with TNR calculation, this could slightly reduce the calculated value."))
      ))
  ),
  
  hr(),
  HTML('<b>Disclaimer: This is a prototype tool to support research. Validate your findings. </b><br/>This code is currently private and may be accessed under request to <a href="mailto:V.RothCardoso@bham.ac.uk">V.RothCardoso@bham.ac.uk</a>.')
  # 2021/02/01 current repository: https://github.com/gkoutos-group/threshold_picker
)
