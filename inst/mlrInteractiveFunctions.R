assumptionStat<-function(lm, assumption){

  #Durbin-Wilson for independence
  dw<-lmtest::dwtest(lm)
  dwStat<-unname(dw$statistic)
  dwP<-unname(dw$p.value)
  auto<-c("Durbin-Wilson Statistic"=dwStat, "p-value"=dwP)

  #Breusch-Pagan for homo
  bp <- lmtest::bptest(lm)
  bpStat <- unname(bp$statistic)
  bpP <- unname(bp$p.value)
  ind <- c("Breusch-Pagan Statistic"=bpStat, "p-value"=bpP)

  #Shapiro-Wilks for norm
  sw <- stats::shapiro.test(lm$residuals)
  swStat <- unname(sw$statistic)
  swP <- unname(sw$p.value)
  norm <- c("Shapiro-Wilks Statistic"=swStat, "p-value"=swP)

  #Mulit
  vif <- regclass::VIF(lm)
  vif <- as.data.frame(vif, nrow=2)

  #Cooks
  cook <- c("No Statistic"=NA, "No p-value"=NA)

  if(assumption=="Independence"){

    return(auto)
  }
  if(assumption=="Homoscedasticity"){

    return(ind)
  }
  if(assumption=="Normality"){

    return(norm)
  }
  if(assumption=="Multicollinearity"){
    return(vif)
  }
  if(assumption=="Outliers"){

    return(cook)
  }
}



assumptionVisual<-function(lm, assumption){

  #Residuals plot
  resPlot<-ggplot2::ggplot(ggplot2::fortify(lm), ggplot2::aes(x=.fitted, y=.resid))+
    ggplot2::geom_point()+
    ggplot2::geom_hline(yintercept = 0, color="red", linetype="dashed")+
    ggplot2::ylab("Residuals")+
    ggplot2::xlab("Fitted Values")+
    ggplot2::labs(title="Residuals vs. Fitted Values")+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "none",
                   text=ggplot2::element_text(family = "Times"))

  #Density plot
  densPlot<-ggplot2::ggplot(ggplot2::fortify(lm), ggplot2::aes(x=.resid))+
    ggplot2::geom_density(fill="red4")+
    ggplot2::ylab("")+
    ggplot2::xlab("")+
    ggplot2::labs(title="Density Plot of Residuals of the Model",
                  caption=paste("Mean", mean(lm$residuals), "Standard deviation:", sd(lm$residuals)))+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "none",
                   text=ggplot2::element_text(family = "Times"))

  #QQ plot
  qqPlot<-ggplot2::ggplot(ggplot2::fortify(lm), ggplot2::aes(sample=.resid))+
    ggplot2::stat_qq()+
    ggplot2::stat_qq_line()+
    ggplot2::ylab("Sample Quantiles")+
    ggplot2::xlab("Theoretical Quantiles")+
    ggplot2::labs(title="QQ Plot of Model Residuals")+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "none",
                   text=ggplot2::element_text(family = "Times"))
  normGrids<-gridExtra::grid.arrange(densPlot, qqPlot, ncol=2)

  #Cooks plot
  cookPlot<-lindia::gg_cooksd(lm, threshold = "convention")+
    ggplot2::labs(title="Cook's Distance Plot")+
    ggplot2::theme_classic()+
    ggplot2::theme(legend.position = "none",
                   text=ggplot2::element_text(family = "Times"))

  #Empty plot for independenc
  indPlot<-ggplot2::ggplot(data = iris, ggplot2::aes(x = Sepal.Width)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position="none", text=ggplot2::element_text(family = "Times"))+
    ggplot2::labs(x="", y="")+
    ggplot2::annotate(geom="text", x=40, y=40 , label="IF DATA IS TIME SERIES, SEE DURBIN-WILSON, OTHERWISE CONSIDER SATISFIED")



  # Empty plot for multi
  multPlot<-ggplot2::ggplot(data = iris, ggplot2::aes(x = Sepal.Width)) +
    ggplot2::theme_void() +
    ggplot2::theme(legend.position="none", text=ggplot2::element_text(family = "Times"))+
    ggplot2::labs(x="", y="")+
    ggplot2::annotate(geom="text", x=40, y=40 , label="SEE VIF SCORES")


  if(assumption=="Independence"){
    print(indPlot)
  }
  if(assumption=="Homoscedasticity"){
  print(resPlot)
  }
  if(assumption=="Normality"){
  print(normGrids)
  }
  if(assumption=="Multicollinearity"){
  print(multPlot)
  }
  if(assumption=="Outliers"){
  print(cookPlot)
  }
}

