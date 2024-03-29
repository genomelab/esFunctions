#' esKM2
#' 
#' Uses SurvPlot function that gives out info about number of patient censored
#' at each time.
#' 
#' @export
#' @param es expression set
#' @param kmtime survival time (assumed to be in years, if not convert it to be
#' in years)
#' @param kmcens censor indictor (0 alive, 1 for dead)
#' @param covar covariate
#' @param n.risk is a logical command. If it is set to \code{TRUE}, then will
#' add the number of risk from \code{risk.summary} and the numbers are placed
#' in the bottom.
#' @param levels.only Set to \code{TRUE} to remove the variable names
#' @param title title of the analysis
#' @param xtitle title of the x-axis
#' @param ytitle title of the y-axis
#' @param outputdir outputdir
#' @param filename filename (use if save is set to \code{TRUE}, default is
#' \code{FALSE})
#' @param save use if save is set to \code{TRUE}, default is \code{FALSE}
#' @note Requires package 'SurvPlot'
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #esKM2(eset, kmtime, kmcens, covar = "covariates_of_interest", n.risk = TRUE, levels.only = TRUE, title = "esKM 2", xtitle = "Time", ytitle = "Survival Probability", outputdir, filename, save = FALSE)
#' 
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
