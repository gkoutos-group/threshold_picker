library(shiny)
library(shinyjs)

threshold_default <- 0.5

if(!exists(".loaded_cutpointr_wrapper", mode='function')) source(here::here('cutpointr_wrapper.R'))

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
  withMathJax(),
  useShinyjs(),
  extendShinyjs(text = jscode_comparison_enable_disable, functions = c('disableTab', 'enableTab')),
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
                 verbatimTextOutput("auc_and_ci"),
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
                 ),
                 downloadButton("download_table_metrics", "Download complete performance table")
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
                        
                        helpText("Absent (negative) labels (updated model on columns)"),
                        tableOutput("comparison_table_absent"),
                        
                        helpText("Present (positive) labels (updated model on columns)"),
                        tableOutput("comparison_table_present")
                      )),
               column(
                 width = 4,
                 helpText("Comparison outputs"),
                 verbatimTextOutput('comparison_aucs_text'),
                 
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
                   "Choose CSV/XLS(X) File",
                   multiple = FALSE,
                   accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv",
                              ".xls",
                              ".xlsx")
                 ),
                 
                 textInput("sheet", label = "If XLSX file, add sheet name:", value=""),
                 
                 textInput("true_variable", label = "True class column name:", value =
                             "class"),
                 span(textOutput("true_variable_info"),
                      style = "color:red"),
                 
                 textInput("true_variable_label", label = "Positive class value:", value =
                             "1"),
                 
                 textInput("predicted_scores", label = "Predicted score column name (current model):", value =
                             "prediction"),
                 span(textOutput("predicted_scores_info"),
                      style = "color:red"),
                 textInput("initial_model_label", 
                           label="Initial model label on plots:",
                           value="Initial model"),
                 
                 fluidRow(column(10,
                                 checkboxInput("use_predicted_2", "Compare to another model?", value = FALSE)),
                          column(2,
                                 actionButton("predicted_scores_swap", "Swap"))),
                 
                 textInput("predicted_scores_2", 
                           label="Predicted score column name (updated model):",
                           value="prediction2"),
                 span(textOutput("predicted_scores_2_info"),
                      style = "color:red"),
                 textInput("updated_model_label", 
                           label="Updated model label on plots:",
                           value="Updated model"),
                 
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
                 helpText("Analysis settings"),
                 numericInput("boot.n",
                              label = "Number of bootstraps",
                              min=1,
                              value=10,
                              max=500),
                 numericInput("boot.seed",
                              label = "Bootstrapping seed",
                              value=123),
                 helpText("Comparison settings"),
                 textInput("cutoffs",
                           label="Cutt-offs for reclassification metrics",
                           value='0, 0.1, 0.3, 0.5, 0.7, 0.9, 1')
               )
             )),
    tabPanel("Help",
             
             HTML('<h3>Basic metrics</h3><br/>'),
             helpText('\\(Accuracy = \\frac{TP + TN} {TP + TN + FP + FN}\\)'),
             helpText('\\(PPV = \\frac{TP} {TP + FP}\\)'),
             helpText('\\(NPV = \\frac{TN} {TN + FN}\\)'),
             helpText('\\(TPR = \\frac{TP} {TP + FN}\\)'),
             helpText('\\(TNR = \\frac{TN} {TN + FP}\\)'),
             helpText('\\(F_{1} score = 2 \\cdot \\frac{PPV \\cdot TPR} {PPV + TPR}\\)'),
             HTML('The range of most metrics is 0 to 1. The higher the value for most metrics the best they are.<br/>'),
             HTML('In the case of AUC, a value around 0.5 means that the model is not able to separate the samples. A value of 1 indicates perfect model.<br/>'),
             HTML('Information about these metrics can be found at <a href="https://en.wikipedia.org/wiki/Confusion_matrix">Wikipedia</a>.<br/>'),
             
             HTML('<h3>Diagnostic Odds Ratio (DOR)</h3><br/>'),
             helpText('\\(DOR = \\frac{\\left( \\cfrac{TP}{FN} \\right)} {\\left( \\cfrac{FP}{TN} \\right)}\\)'),
             HTML('<i>"DOR depends significantly on the sensitivity and specificity of a test. A test with high specificity and sensitivity with low rate of false positives and false negatives has high DOR. With the same sensitivity of the test, DOR increases with the increase of the test specificity. For example, a test with sensitivity > 90% and specificity of 99% has a DOR greater than 500."</i> - <a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4975285/">Ana-Maria Simundic. Measures of Diagnostic Accuracy: Basic Definitions</a>.<br/>'),
             
             HTML('<h3>AUC confidence interval</h3><br/>'),
             HTML('The confidence interval is calculated with stratified boostrap replicates. <a href="https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-12-77">Robin et al. pROC: an open-source package for R and S+ to analyze and compare ROC curves</a><br/>'),
             
             HTML('<h3>Comparison of models</h3><br/>'),
             HTML('The use of these functions will depend on the calibration of the model, as the distribution of predictions are compared.<br/>'),
             HTML('AUC statistical test is calculated using DeLong\'s method. <a href="https://pubmed.ncbi.nlm.nih.gov/3203132/">Delong et al. Comparing the areas under two or more correlated receiver operating characteristic curves: a nonparametric approach</a> method.<br/>'),
             HTML('More details about Net Reclassification Improvement (NRI) can be found here: <a href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3918180/">Kerr et al. Net Reclassification Indices for Evaluating Risk-Prediction Instruments: A Critical Review</a><br/>'),
             HTML('More details about Integrated Discrimination Improvement (IDI) can be found here: <a href="https://onlinelibrary.wiley.com/doi/abs/10.1002/sim.2929">Pencina et al. Evaluating the added predictive ability of a new marker: From area under the ROC curve to reclassification and beyond</a><br/>'),
             
             HTML('<h4>Heatmaps</h4><br/>'),
             HTML("Grey indicate a region where the predictions of the models are similar.<br/>"),
             HTML("Blue are regions where an increased number means the updated model performance is worse and green indicate regions where an increased number is better.<br/>"),
             HTML("For controls (heatmap on the left), the lower the output prediction score the better, as the number of participants in the green region will increase (left of the diagonal means lower score in the updated than the initial model, these are controls, the lower the better).<br/>"),
             HTML("For cases (heatmap on the right), the higher the output prediction score the better, as the number of participants in the green region will increase (right of the diagonal means higher score in the updated model than the initial model, these are cases, the higher the score the better).<br/>"),
             
             HTML('<h4>Cut-offs</h4><br/>'),
             HTML('This information is copied from <i>cutpointr</i> documentation:<br/><br/>'),
             HTML('Different methods available:<br/>'),
             HTML('<ul>'),
             HTML('<li><i>maximize_metric</i>: Maximize the metric function</li>'),
             HTML('<li><i>minimize_metric</i>: Minimize the metric function</li>'),
             #HTML('<li><i>maximize_loess_metric</i>: Maximize the metric function after LOESS smoothing</li>'),
             #HTML('<li><i>minimize_loess_metric</i>: Minimize the metric function after LOESS smoothing</li>'),
             #HTML('<li><i>maximize_spline_metric</i>: Maximize the metric function after spline smoothing</li>'),
             #HTML('<li><i>minimize_spline_metric</i>: Minimize the metric function after spline smoothing</li>'),
             #HTML('<li><i>maximize_gam_metric</i>: Maximize the metric function after smoothing via Generalized Additive Models</li>'),
             #HTML('<li><i>minimize_gam_metric</i>: Minimize the metric function after smoothing via Generalized Additive Models</li>'),
             #HTML('<li><i>maximize_boot_metric</i>: Bootstrap the optimal cutpoint when maximizing a metric</li>'),
             #HTML('<li><i>minimize_boot_metric</i>: Bootstrap the optimal cutpoint when minimizing a metric</li>'),
             #HTML('<li><i>oc_manual</i>: Specify the cutoff value manually</li>'),
             HTML('<li><i>oc_mean</i>: Use the sample mean as the "optimal" cutpoint</li>'),
             HTML('<li><i>oc_median</i>: Use the sample median as the "optimal" cutpoint</li>'),
             HTML('<li><i>oc_youden_kernel</i>: Maximize the Youden-Index after kernel smoothing the distributions of the two classes</li>'),
             HTML('<li><i>oc_youden_normal</i>: Maximize the Youden-Index parametrically assuming normally distributed data in both classes</li>'),
             HTML('</ul>'),
             
             HTML('Different metrics available:<br/>'),
             HTML('<ul>'),
             HTML('<li><i>accuracy</i>: Fraction correctly classified</li>'),
             HTML('<li><i>abs_d_sens_spec</i>: The absolute difference of sensitivity and specificity</li>'),
             HTML('<li><i>abs_d_ppv_npv</i>: The absolute difference between positive predictive value (PPV) and negative predictive value (NPV)</li>'),
             HTML('<li><i>roc01</i>: Distance to the point (0,1) on ROC space</li>'),
             HTML('<li><i>cohens_kappa</i>: Cohen\'s Kappa</li>'),
             HTML('<li><i>sum_sens_spec</i>: sensitivity + specificity</li>'),
             HTML('<li><i>sum_ppv_npv</i>: The sum of positive predictive value (PPV) and negative predictive value (NPV)</li>'),
             HTML('<li><i>prod_sens_spec</i>: sensitivity * specificity</li>'),
             HTML('<li><i>prod_ppv_npv</i>: The product of positive predictive value (PPV) and negative predictive value (NPV)</li>'),
             HTML('<li><i>youden</i>: Youden- or J-Index = sensitivity + specificity - 1</li>'),
             HTML('<li><i>odds_ratio</i>: (Diagnostic) odds ratio</li>'),
             HTML('<li><i>risk_ratio</i>: risk ratio (relative risk)</li>'),
             HTML('<li><i>p_chisquared</i>: The p-value of a chi-squared test on the confusion matrix</li>'),
             #HTML('<li><i>cost_misclassification</i>: The sum of the misclassification cost of false positives and false negatives. Additional arguments</li>'),
             #HTML('<li><i>total_utility</i>: The total utility of true / false positives / negatives. Additional arguments</li>'),
             HTML('<li><i>F1_score</i>: The F1-score (2 * TP) / (2 * TP + FP + FN)</li>'),
             #HTML('<li><i>metric_constrain</i>: Maximize a selected metric given a minimal value of another selected metric</li>'),
             #HTML('<li><i>sens_constrain</i>: Maximize sensitivity given a minimal value of specificity</li>'),
             #HTML('<li><i>spec_constrain</i>: Maximize specificity given a minimal value of sensitivity</li>'),
             #HTML('<li><i>acc_constrain</i>: Maximize accuracy given a minimal value of sensitivity</li>'),
             HTML('</ul>'),
             
             HTML('<h3>R Packages used</h3><br/>'),
             HTML('<i>shiny, shinyjs, mathjaxr</i> (Web interface).<br/>'),
             HTML('<i>readxl, plyr, tidyverse</i> (data loading and processing).<br/>'),
             HTML('<i>ModelMetrics</i> (some metrics).<br/>'),
             HTML('<i>pROC</i> (AUC and CI).<br/>'),
             HTML('<i>PRROC</i> (Precision-Recall curves).<br/>'),
             HTML('<i>Hmisc</i> (NRI/IDI).<br/>'),
             HTML('<i>ggplot2, scales</i> (visualisation).<br/>'),
             HTML('<i>cutpointr</i> (optimal threshold points).<br/>')
             )
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
      helpText('Select method/metric:'),
      selectInput("cutpoint_method",
                  label = "Method:",
                  choices = names(methods_available)),
      selectInput("cutpoint_metric",
                  label = "Metric:",
                  choices = names(metrics_available)),
      helpText('This window can be moved!')
    ),
    style = "opacity: 0.92"
  ),
  
  hr(),
  HTML(
    '<b>Disclaimer: This is a prototype tool to support research. Validate your findings. </b><br/>This code is public on <a href="https://github.com/gkoutos-group/threshold_picker/">https://github.com/gkoutos-group/threshold_picker/</a>. For details contact <a href="mailto:V.RothCardoso@bham.ac.uk">V.RothCardoso@bham.ac.uk</a>.'
  )
)
