esKM2 <-
function(es, kmtime, kmcens, covar = "" , n.risk=TRUE, levels.only=TRUE, title = "", xtitle = "Time", ytitle = "Survival Probability", outputdir, filename, save=FALSE) {
  
  #########################################################################
  ## Function 7
  ##  Uses SurvPlot function that gives out info about number of patient censored at each time.
  ##     es - can be an expressionset or a dataframe
  ##     kmtime - survival time (assumed to be in years, if not convert it to be in years)
  ##     kmcens - censor indictor (0 alive, 1 for dead)
  ##     covariate - to use for stratification, can be a vector, or label for expressionset or dataframe variables
  ##     title, ytitle - self explanatory
  ##     outputdir, filename  (use if save is set to TRUE, default is FALSE)
  #########################################################################
  
  
  
  
  if(typeof(es)[1]=='S4') {df = es2df(es[1,!is.na(es[[kmtime]])])} else {df = es}
  if (typeof(covar)!='integer') {covar = df[[covar]]}
  capture.output(print(covar))
  kmtime = as.numeric(df[[kmtime]])
  kmcens = df[[kmcens]]
  
  
  kmtime = kmtime/365  
  t.Surv = Surv(kmtime, kmcens)
  
  ##### Change axis to be by years (can choose odd or even years)
  axis_odd_years = c(1:trunc(max(kmtime)))[c(1:trunc(max(kmtime))) %% 2 == 1]
  axis_odd_years_breaks = c(1:trunc(max(kmtime)))[c(1:trunc(max(kmtime))) %% 2 == 1]
  axis_even_years = c(1:trunc(max(kmtime)))[c(1:trunc(max(kmtime))) %% 2 != 1]
  axis_even_years_breaks = c(1:trunc(max(kmtime)))[c(1:trunc(max(kmtime))) %% 2 != 1]
  axis_breaks = axis_even_years_breaks
  axis_labels = axis_even_years
  ### see if a stratification variable has been given:
  
  if (length(covar) == 0) 
    t.survfit = survfit(t.Surv ~ 1, data=df, ) 
  else
    t.survfit = survfit(t.Surv ~ covar, data=df)
  survplot(t.survfit, n.risk=n.risk, levels.only=levels.only, conf="none", xlab=xtitle, ylab=ytitle)
  
}
