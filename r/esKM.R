#' esKM
#' 
#' Wrapper for qplot_survival function
#' 
#' @export
#' @param es expression set
#' @param kmtime survival time (assumed to be in years, if not convert it to be
#' in years)
#' @param kmcens censor indictor (0 alive, 1 for dead)
#' @param covar covariate
#' @param title title of the analysis
#' @param xtitle title of the x-axis
#' @param ytitle title of the y-axis
#' @param kmcolors color palette
#' @param outputdir outputdir
#' @param filename filename (use if save is set to \code{TRUE}, default is
#' \code{FALSE}
#' @param simplestat If \code{TRUE}, it will write the statistics to display
#' the text on the plot
#' @param save use if save is set to \code{TRUE}, default is \code{FALSE}
#' @note Requires package 'ggplot' and 'serve'. Requires function 'surv',
#' 'theme_set', and 'ggplot'.
#' @author Shahab Asgharzadeh
#' @references "An Introduction to Bioconductor's ExpressionSet Class" \cr Seth
#' Falcon, Martin Morgan, and Robert Gentleman \cr 6 October, 2006; revised 9
#' February, 2007 \cr
#' @examples
#' 
#' #esKM(eset, kmtime, kmcens, covar = "covariate_of_interest", title = "KM Plot", xtitle = "Time", ytitle = "Survival", kmcolors = kmPalette, outputdir, filename, simplestat = FALSE, save = FALSE)
#' 
esKM <-
function(es, kmtime, kmcens, covar="", title = "", xtitle="Time", ytitle = "Survival", kmcolors = kmPalette, outputdir, filename, simplestat = FALSE, save=FALSE) {
  
  #########################################################################
  ## Function 7
  ##  Wrapper for qplot_survival function
  ##     es - can be an expressionset or a dataframe
  ##     kmtime - srvival time (assumed to be in years, if not convert it to be in years)
  ##     kmcens - censor indictor (0 alive, 1 for dead)
  ##     covariate - to use for stratification, can be a vector, or label for expressionset or dataframe variables
  ##     title, ytitle - self explanatory
  ##     outputdir, filename  (use if save is set to TRUE, default is FALSE)
  #########################################################################
  kmPalette<-NULL
  
  if(typeof(es)[1]=='S4') {df = es2df(es[1,!is.na(es[[kmtime]])])} else {df = es}
  if (typeof(covar)!='integer') {covar = df[[covar]]}
  capture.output(print(covar))
  kmtime = as.numeric(df[[kmtime]])
  kmcens = as.numeric(df[[kmcens]])
  
  #  if (title=='') title = paste0('Dataset=', deparse(substitute(es)), ' Covar: ',deparse(substitute(covar)), 
  #                     ' (', paste(levels(droplevels(factor(es[[deparse(substitute(covar))]]))), collapse=", "), ') ',
  #                     'risk categories: ', paste(levels(droplevels(factor(es[['risk']]))), collapse=", "))  
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
  
  if (length(covar) == 0) {
    #### Setup survival function, logrank test, and ploting framework
    #create survival curve for all samples stratified by 'covar' variable above
    t.survfit = survfit(t.Surv ~ 1, data=df)
    #Logrank test
    #setup survival frame
    t.survframe = createSurvivalFrame(t.survfit)
    
    ##### Actual Plot (based on ggplot KM code modified by me in Extras.R)
    km = 
      qplot_survival(t.survframe, kmcolors = kmcolors) + 
      scale_x_continuous(breaks=axis_breaks, labels=axis_labels) +
      labs(x = xtitle) +
      labs(y = ytitle) +
      theme(legend.position="right") +  
      theme(legend.title = element_text(colour="black", size=16, face="bold")) +
      theme(legend.text = element_text(colour="black", size=16, face="bold")) +
      labs(title = title)
    if(save){
      ggsave(paste(outputdir, filename, "_km.png", sep=""), plot = km)
      #ggsave(paste(outputdir, file_beginning, "_km.ps", sep=""), plot = km)  
    }
    return(km)
  }
  ## Startification variable has been given, stats are calculated and reported.
  else { 
    
    #### Setup survival function, logrank test, and ploting framework
    #create survival curve for all samples stratified by 'covar' variable above
    
    ### Change labeling of covar:
    ### Add the group numbers to the name
    cov_levels = table(droplevels(as.factor(covar)))
    cov_levels_legend_heading = names(cov_levels)
    cov_levels_legend_numbers = cov_levels
    
    number_covar_levels = rep(1:length(cov_levels_legend_heading))
    covar = factor(covar)
    for(x in number_covar_levels) { 
         levels(covar)[levels(covar)==cov_levels_legend_heading[x]] <- 
           paste0(cov_levels_legend_heading[x], "  n=" , cov_levels_legend_numbers[x])
    }
    

    Grp = covar   ### REnamed it to Grp as on the legend it comes up as "Grp:"
                  ### Need to fix it at some point.
    
    t.survfit = survfit(t.Surv ~ Grp, data=df)
    #plot(t.survfit)
    #Logrank test
    logrank = survdiff(t.Surv ~ Grp, data=df)
    
    #setup survival frame
    t.survframe = createSurvivalFrame(t.survfit)
    
    ##### Setup Labeling for the plot (besides the legend)
    #Obtain chisq, pvalue, and degree of freedome from survdiff 
    df = (sum(1 * (logrank$exp > 0))) - 1
    chisq = round(logrank$chisq,2)
    pvalue =  format(signif(1 - pchisq(chisq, df), 3))
    chisq_text = paste("logrank =",chisq)
    pvalue_text = paste("P-value =", pvalue)
    df_text = paste("df =", df)
    # Obtain the number of samples in each covar and paste them to a text
    cov_levels = table(droplevels(as.factor(covar)))
    cov_levels_heading = paste(names(cov_levels), collapse = '\t')
    cov_levels_numbers = paste(cov_levels, collapse = '\t')


    #write all the stats and numbers into a variable to display on the plot
    stat = paste(chisq_text, df_text, cov_levels_heading, cov_levels_numbers, pvalue_text, sep="\n")
    if (simplestat==TRUE) stat = pvalue_text
    ##### Actual Plot (based on ggplot KM code modified by me in Extras.R)
    km = 
      qplot_survival(t.survframe, kmcolors = kmcolors) + 
      #scale_x_continuous(breaks=axis_breaks, labels=axis_labels) +
      labs(x = xtitle) +
      labs(y = ytitle) + 
      labs(title = title) +
      theme(plot.title = element_text(colour="black", size=18, face="bold", vjust=0.4)) +
      theme(legend.position="right") +  
      theme(legend.title = element_text(colour="black", size=16, face="bold")) +
      theme(legend.text = element_text(colour="black", size=16, face="bold")) 
      
    
    ####  Create a label of text containing stat information
    p1 = annotate("text", 
                  x = 0, 
                  y = 0.08, label = stat, 
                  vjust = 0.5, size=6, hjust = -0.05, face="bold", colour = "black");
    p2 = annotate("text", 
                  x = 0, 
                  y = 0, label = stat, 
                  vjust = 0.0, size=6, hjust = 0, face="bold", colour = "black");
    #print out the stats:
    cat(stat)
    ###### Save the plots, uses the variable name to save (with and without labels)
    ###### saves as png and ps.
    if(save){
      ggsave(paste(outputdir, filename, "_km.png", sep=""), plot = km+p1)
      #ggsave(paste(outputdir, file_beginning, "_km.ps", sep=""), plot = km+p1)
      ggsave(paste(outputdir, filename, "_km_nolabel.png", sep=""),plot = km+p2)
      #ggsave(paste(outputdir, file_beginning, "_km_nolabel.ps", sep=""), plot = km)
    }
    if(simplestat==TRUE)  km = km + p1 else km=km + p2
    return(km)
  }
}
