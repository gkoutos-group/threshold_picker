

#library(cutpointr) # not needed, will break npv and other functions


.loaded_cutpointr_wrapper <- function() {}


methods_available = list('-- Manual -- '=NULL, #default if there was a movement
                         'Maximize metric'=cutpointr::maximize_metric,
                         'Minimize Metric'=cutpointr::minimize_metric,
                         #'maximize_loess_metric'=cutpointr::maximize_loess_metric,
                         #'minimize_loess_metric'=cutpointr::minimize_loess_metric,
                         #'maximize_spline_metric'=cutpointr::maximize_spline_metric,
                         #'minimize_spline_metric'=cutpointr::minimize_spline_metric,
                         #'maximize_gam_metric'=cutpointr::maximize_gam_metric,
                         #'minimize_gam_metric'=cutpointr::minimize_gam_metric,
                         #'maximize_boot_metric'=cutpointr::maximize_boot_metric,
                         #'minimize_boot_metric'=cutpointr::minimize_boot_metric,
                         #'oc_manual'=cutpointr::oc_manual,
                         'Maximize Youden-Index after kernel smoothing'=cutpointr::oc_youden_kernel,
                         'Maximize Youden-Index assuming normal distribution'=cutpointr::oc_youden_normal,
                         'Sample mean as cutpoint'=cutpointr::oc_mean,
                         'Sample median as cutpoint'=cutpointr::oc_median
                         )


metrics_available = list('Distance to point (0,1) in Sensitivity x Specificity curve'=cutpointr::roc01,
                         'Cohens Kappa'=cutpointr::cohens_kappa,
                         'Diagnostic Odds Ratio'=cutpointr::odds_ratio,
                         'Relative risk'=cutpointr::risk_ratio,
                         'P-value of Chi-Squared test'=cutpointr::p_chisquared,
                         #'cost_misclassification'=cost_misclassification,
                         #'total_utility'=cutpointr::total_utility,
                         'F1-score'=cutpointr::F1_score,
                         'Accuracy'=cutpointr::accuracy,
                         'Absolute (PPV - NPV)'=cutpointr::abs_d_ppv_npv,
                         'PPV + NPV'=cutpointr::sum_ppv_npv,
                         'PPV * NPV'=cutpointr::prod_ppv_npv,
                         'Absolute (Sensitivity - Specificity)'=cutpointr::abs_d_sens_spec,
                         'Sensitivity + Specificity'=cutpointr::sum_sens_spec,
                         'Sensitivity * Specificity'=cutpointr::prod_sens_spec,
                         'Sensitivity + Specificity - 1'=cutpointr::youden
                         #'metric_constrain'=cutpointr::metric_constrain,
                         #'sens_constrain'=cutpointr::sens_constrain,
                         #'spec_constrain'=cutpointr::spec_constrain,
                         #'acc_constrain'=cutpointr::acc_constrain
                         )


cutpointr_best_point <- function(data, x, class, direction="<=", pos_class=1, method=cutpointr::maximize_metric, metric=cutpointr::accuracy) {
  c <- cutpointr::cutpointr(data=data,
                            x=!!x,
                            class=!!class,
                            direction=direction,
                            pos_class=pos_class,
                            method=method,
                            metric=metric)
  return(c$optimal_cutpoint)
  
}
