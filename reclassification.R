# this is a modified version of reclassification from PredictABEL so that it returns the tables and values without rounding

library(Hmisc)

"reclassification_returned" <-
  function(data,cOutcome,predrisk1,predrisk2, cutoff, verbose=F) {
    
    c1 <- cut(predrisk1,breaks = cutoff ,include.lowest=TRUE,right= FALSE)
    c2 <- cut(predrisk2,breaks = cutoff ,include.lowest=TRUE,right= FALSE)
    tabReclas <- table("Initial Model"=c1, "Updated Model"=c2)

    ta<- table(c1, c2, data[,cOutcome])
    
    TabAbs <- ta[,,1]
    tab1 <- cbind(TabAbs, " % reclassified"= round((rowSums(TabAbs)-diag(TabAbs))/rowSums(TabAbs),2)*100)
    names(dimnames(tab1)) <- c("Initial Model", "Updated Model")
    
    TabPre <- ta[,,2]
    tab2 <- cbind(TabPre, " % reclassified"= round((rowSums(TabPre)-diag(TabPre))/rowSums(TabPre),2)*100)
    names(dimnames(tab2)) <- c("Initial Model", "Updated Model")

    Tab <- tabReclas
    tab <- cbind(Tab, " % reclassified"= round((rowSums(Tab)-diag(Tab))/rowSums(Tab),2)*100)
    names(dimnames(tab)) <- c("Initial Model", "Updated Model")
    
    c11 <-factor(c1, levels = levels(c1), labels = c(1:length(levels(c1))))
    c22 <-factor(c2, levels = levels(c2), labels = c(1:length(levels(c2))))
    
    x<-improveProb(x1=as.numeric(c11)*(1/(length(levels(c11)))),
                   x2=as.numeric(c22)*(1/(length(levels(c22)))), y=data[,cOutcome])
    
    
    y<-improveProb(x1=predrisk1, x2=predrisk2, y=data[,cOutcome])
    
    
    nri_cat = list(value=x$nri, 
                   se=x$se.nri,
                   ci95_low=x$nri-1.96*x$se.nri,
                   ci95_high=x$nri+1.96*x$se.nri,
                   pval=2*pnorm(-abs(x$z.nri)))
    
    nri_cont = list(value=y$nri,
                    se=y$se.nri,
                    ci95_low=y$nri-1.96*y$se.nri,
                    ci95_high=y$nri+1.96*y$se.nri,
                    pval=2*pnorm(-abs(y$z.nri)))
    
    idi = list(value=y$idi,
               se=y$se.idi,
               ci95_low=y$idi-1.96*y$se.idi,
               ci95_high=y$idi+1.96*y$se.idi,
               pval=2*pnorm(-abs(y$z.idi)))
    
    if(verbose) {
      cat(" _________________________________________\n")
      cat(" \n     Reclassification table    \n")
      cat(" _________________________________________\n")
      
      cat ("\n Outcome: absent \n  \n" )
      print(tab1)
      
      cat ("\n \n Outcome: present \n  \n" )
      print(tab2)
      
      cat ("\n \n Combined Data \n  \n" )
      print(tab)
      
      cat(" _________________________________________\n")
      cat("\n NRI(Categorical) [95% CI]:", round(x$nri,4),"[",round(x$nri-1.96*x$se.nri,4),"-",
          round(x$nri+1.96*x$se.nri,4), "]", "; p-value:", round(2*pnorm(-abs(x$z.nri)),5), "\n" )
      cat(" NRI(Continuous) [95% CI]:", round(y$nri,4),"[",round(y$nri-1.96*y$se.nri,4),"-",
          round(y$nri+1.96*y$se.nri,4), "]", "; p-value:", round(2*pnorm(-abs(y$z.nri)),5), "\n" )
      cat(" IDI [95% CI]:", round(y$idi,4),"[",round(y$idi-1.96*y$se.idi,4),"-",
          round(y$idi+1.96*y$se.idi,4), "]","; p-value:", round(2*pnorm(-abs(y$z.idi)),5), "\n")
    }
    
    return(list(tab_absent=tab1, tab_present=tab2, tab_both=tab, nri_cat=nri_cat, nri_cont=nri_cont, idi=idi))
  }
