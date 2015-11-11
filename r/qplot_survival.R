qplot_survival <-
function(f.frame, f.CI="default",f.linesize=1, f.shapesize=2.5, f.shape=3, kmcolors = kmPalette){
  kmPalette<-NULL # setting this variable as NULL to pass R CMD check , it was defined in anothe frame and passed into this frame without
  # being declared a global variable.  
  surv<-NULL # setting this variable as NULL to pass R CMD check , it was defined in anothe frame and passed into this frame without
  # being declared a global variable.  
  upper<- NULL # setting this variable as NULL to pass R CMD check , it was defined in anothe frame and passed into this frame without
  # being declared a global variable.  
  lower<-NULL # setting this variable as NULL to pass R CMD check , it was defined in anothe frame and passed into this frame without
  # being declared a global variable.  
  n.censor<-NULL # setting this variable as NULL to pass R CMD check , it was defined in anothe frame and passed into this frame without
  # being declared a global variable.  
  
  
  # use different plotting commands dependig whether or not strata's are given 
  if("strata" %in% names(f.frame) == FALSE){
    # confidence intervals are drawn if not specified otherwise
    if(f.CI=="default" | f.CI==TRUE ){
      # create plot with 4 layers (first 3 layers only events, last layer only censored)
      # hint: censoring data for multiple censoring events at timepoint are overplotted
      # (unlike in plot.survfit in survival package)
      ggplot(data=f.frame) + geom_step(aes(x=time, y=surv), direction="hv", size=f.linesize) + 
        geom_step(aes(x=time, y=upper), directions="hv", linetype=2, size=f.linesize) + 
        geom_step(aes(x=time,y=lower), direction="hv", linetype=2, size=f.linesize) + 
        geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape, size=f.shapesize) +
        scale_y_continuous(limits=c(0, 1)) +
        coord_cartesian(xlim = c(0, max(f.frame$time)*1.025), ylim = c(0, max(f.frame$surv)*1.025)) +
        theme(axis.text.x = element_text(size=18)) + theme(axis.text.y = element_text(size=18)) + 
        theme(axis.title.x = element_text(size = 18, vjust = -1)) +
        theme(axis.title.y = element_text(size = 18, angle = 90, vjust = 0.25))  +
        scale_color_manual(values=kmcolors) +
        labs(x = "Time") + 
        labs(y = "Survival")
      
    }
    else {
      # create plot without confidence intervalls
      ggplot(data=f.frame) + geom_step(aes(x=time, y=surv), direction="hv" , size=f.linesize) + 
        geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape, size=f.shapesize) +
        scale_y_continuous(limits=c(0, 1)) +
        coord_cartesian(xlim = c(0, max(f.frame$time)*1.025), ylim = c(0, max(f.frame$surv)*1.025)) +
        theme(axis.text.x = element_text(size=18)) + theme(axis.text.y = element_text(size=18)) + 
        theme(axis.title.x = element_text(size = 18, vjust = -1)) +
        theme(axis.title.y = element_text(size = 18, angle = 90, vjust = 0.25))  +
        scale_color_manual(values=kmcolors) +
        labs(x = "Time") + 
        labs(y = "Survival")
    }
  }
  else {
    if(f.CI=="default" | f.CI==FALSE){
      # without CI
      ggplot(data=f.frame, aes(group=strata, colour=strata, linetype=strata)) + 
        geom_step(aes(x=time, y=surv), direction="hv", size=f.linesize) + 
        geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape, size=f.shapesize) + 
        scale_y_continuous(limits=c(0, 1)) +
        scale_linetype_manual(values=c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid")) +
        coord_cartesian(xlim = c(0, max(f.frame$time)*1.025), ylim = c(0, max(f.frame$surv)*1.025)) +
        theme(axis.text.x = element_text(size=18)) + theme(axis.text.y = element_text(size=18)) + 
        theme(axis.title.x = element_text(size = 18, vjust = -1)) +
        theme(axis.title.y = element_text(size = 18, angle = 90, vjust = 0.25)) +
        scale_color_manual(values=kmcolors) +
        labs(x = "Time") + 
        labs(y = "Survival") 
    }
    else {
      ggplot(data=f.frame, aes(colour=strata, group=strata, linetype=strata)) + 
        geom_step(aes(x=time, y=surv), direction="hv", size=f.linesize) + 
        geom_step(aes(x=time, y=upper), directions="hv", linetype=2, alpha=0.5, size=f.linesize) + 
        geom_step(aes(x=time,y=lower), direction="hv", linetype=2, alpha=0.5, size=f.linesize) + 
        geom_point(data=subset(f.frame, n.censor==1), aes(x=time, y=surv), shape=f.shape, size=f.shapesize) +
        scale_y_continuous(limits=c(0, 1)) +
        coord_cartesian(xlim = c(0, max(f.frame$time)*1.025), ylim = c(0, max(f.frame$surv)*1.025)) +
        theme(axis.text.x = element_text(size=18)) + theme(axis.text.y = element_text(size=18)) + 
        theme(axis.title.x = element_text(size = 18, vjust = -1)) +
        theme(axis.title.y = element_text(size = 18, angle = 90, vjust = 0.25))  +
        scale_color_manual(values=kmcolors) +
        labs(x = "Time") + 
        labs(y = "Survival")
    }
  }
}
