

#library(cutpointr) # not needed, will break npv and other functions


.loaded_cutpointr_wrapper <- function() {}


methods_available = list('-- Manual -- '=NULL, #default if there was a movement
                         'maximize_metric'=cutpointr::maximize_metric,
                         'minimize_metric'=cutpointr::minimize_metric,
                         #'maximize_loess_metric'=cutpointr::maximize_loess_metric,
                         #'minimize_loess_metric'=cutpointr::minimize_loess_metric,
                         #'maximize_spline_metric'=cutpointr::maximize_spline_metric,
                         #'minimize_spline_metric'=cutpointr::minimize_spline_metric,
                         #'maximize_gam_metric'=cutpointr::maximize_gam_metric,
                         #'minimize_gam_metric'=cutpointr::minimize_gam_metric,
                         #'maximize_boot_metric'=cutpointr::maximize_boot_metric,
                         #'minimize_boot_metric'=cutpointr::minimize_boot_metric,
                         #'oc_manual'=cutpointr::oc_manual,
                         'oc_mean'=cutpointr::oc_mean,
                         'oc_median'=cutpointr::oc_median,
                         'oc_youden_kernel'=cutpointr::oc_youden_kernel,
                         'oc_youden_normal'=cutpointr::oc_youden_normal
                         )


metrics_available = list('accuracy'=cutpointr::accuracy,
                         'abs_d_sens_spec'=cutpointr::abs_d_sens_spec,
                         'abs_d_ppv_npv'=cutpointr::abs_d_ppv_npv,
                         'roc01'=cutpointr::roc01,
                         'cohens_kappa'=cutpointr::cohens_kappa,
                         'sum_sens_spec'=cutpointr::sum_sens_spec,
                         'sum_ppv_npv'=cutpointr::sum_ppv_npv,
                         'prod_sens_spec'=cutpointr::prod_sens_spec,
                         'prod_ppv_npv'=cutpointr::prod_ppv_npv,
                         'youden'=cutpointr::youden,
                         'odds_ratio'=cutpointr::odds_ratio,
                         'risk_ratio'=cutpointr::risk_ratio,
                         'p_chisquared'=cutpointr::p_chisquared,
                         #'cost_misclassification'=cost_misclassification,
                         #'total_utility'=cutpointr::total_utility,
                         'F1_score'=cutpointr::F1_score
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
