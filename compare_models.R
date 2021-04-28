library(pROC)
library(scales)
library(ggplot2)
library(here)

source(here::here('reclassification.R'))

fix_ci_df__ <- function(ci_obj) {
  return(data.frame(sp = as.numeric(row.names(ci_obj)),
                    se.low = ci_obj[, 1],
                    se.median = ci_obj[, 2],
                    se.high = ci_obj[, 3]))
}


compare_models <- function(df, true_label, model1risk, model2risk, cutoff=c(0, 0.1, 0.3, 1)) {
  rdf <- data.frame(real=df[[true_label]], model1=df[[model1risk]], model2=df[[model2risk]])
  rdf$real <- as.numeric(as.character(rdf$real))
  ret <- reclassification_returned(data=rdf, cOutcome=1, predrisk1 = rdf$model1, predrisk2 = rdf$model2, cutoff)
  return(ret)
}


compare_aucs <- function(df, true_label, model1risk, model2risk, boot.n=100, boot.seed=123, spec_roc=seq(0, 1, 0.01)) {
  set.seed(boot.seed)
  # first model
  model1_roc <- roc(df[[true_label]], df[[model1risk]], direction='<', levels=c(0, 1), plot=F)
  ci_obj_m1 <- fix_ci_df__(ci.se(model1_roc, specificities=spec_roc, boot.n=boot.n))
  # second model
  model2_roc <- roc(df[[true_label]], df[[model2risk]], direction='<', levels=c(0, 1), plot=F)
  ci_obj_m2 <- fix_ci_df__(ci.se(model2_roc, specificities=spec_roc, boot.n=boot.n))
  
  p <- ggroc(list(model1=model1_roc, model2=model2_roc), legacy.axes=T) + 
    theme_classic() + 
    geom_abline(slope=1, intercept = 1, linetype = "dashed", alpha=0.7, color = "grey") + 
    coord_equal() + 
    scale_color_manual(labels = c("Previous model", "Updated model"), values = c(2, 4)) +
    labs(color="Model")
  
  #add the cis
  p <- p + geom_ribbon(data=ci_obj_m1, aes(x=sp, ymin=se.low, ymax=se.high), fill=2, alpha=0.2, inherit.aes=F)
  p <- p + geom_ribbon(data=ci_obj_m2, aes(x=sp, ymin=se.low, ymax=se.high), fill=4, alpha=0.2, inherit.aes=F)
  p <- p + xlab('1 - Specificity') + ylab('Sensitivity')
  
  return(p)
}

plot_reclassification <- function(tab, low="#fee8c8", high="#aeeb34", title='Reclassification') {
  ## reshape data (tidy/tall form)
  dat <- as.data.frame(tab)
  dat$Var1 <- rownames(tab)
  
  dat2 <- dat %>% as_tibble() %>%
    gather(Var2, value, -Var1)
  
  dat2$Var1 <- as.factor(dat2$Var1)
  dat2$Var1 <- forcats::fct_rev(dat2$Var1)
  
  dat2$Var2 <- as.factor(dat2$Var2)
  dat2$Var2 <- forcats::fct_relevel(dat2$Var2, " % reclassified", after = Inf)
  
  dat2$color <- ifelse((as.character(dat2$Var1) == as.character(dat2$Var2)) | dat2$Var2 == ' % reclassified', NA, dat2$value)
  
  ## plot data
  p <- ggplot(dat2, aes(Var2, Var1)) +
      geom_tile(aes(fill = color)) + 
      scale_fill_gradient("", low = low, high = high, na.value = "white") +
      geom_text(aes(label = round(value, 1))) +
      xlab('Updated model') +
      ylab('Initial model') + 
      scale_x_discrete(position = "top")  +
      ggtitle(title) + theme_minimal()
  
  return(p)
}

if(F) {
  df <- read.csv('C:\\Users\\vroth\\Desktop\\UoB material\\20210310_ecg_model\\20210311_nn_clinical_valid_selection_reclassification.csv')
  a <- compare_aucs(df, 'real', 'clinical', 'dnn', boot.n=5)
  r <- compare_models(df, 'real', 'clinical', 'dnn', cutoff=c(0, 0.1, 0.2, 0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))
  plot_reclassification(r$tab_both, title='Overall reclassification')
  plot(a)
  
  model1_roc <- roc(df$real, df$dnn, direction='<', levels=c(0, 1), plot=F)
  ci.auc(model1_roc)
}



