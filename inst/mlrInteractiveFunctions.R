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

ciComps<-function(data, iter, alpha=.05){
  #Defining the degrees of freedom and z stat to be used in conf. ints
  dfree=nrow(data)-ncol(data)
  t<-stats::qt(1-alpha/2, dfree)
  z<-stats::qnorm(1-alpha)

  #Counter for singular matrices
  fails<-0

  #Empty data frame where beta estimates will be stored
  betas<-data.frame(matrix(NA, nrow=iter, ncol=ncol(data)))
  names(betas)<-names(data)
  names(betas)[1]<-"Intercept"

  #Empty data frame where resampled data will be stored
  newDF<-data.frame(matrix(NA, nrow=nrow(data), ncol=ncol(data)))
  names(newDF)<-names(data)

  #Loop through the number of iterations
  for (i in 1:iter){

    #Sample the rows. Ind is a vector of indices that is the same
    #length as the number of rows of the original data frame
    ind<-sample(1:nrow(data), nrow(data), replace=TRUE)

    #Construct new data frame s.t. each row is defined by the ind vecor
    for(j in 1:nrow(data)){
      newDF[j,]<-data[ind[j],]
    }


    #Linear algebra, constructing design matrix
    Y<-as.matrix(newDF[,1])
    X<-newDF[-1]
    X<-as.matrix(cbind(1,X))

    #Taking the inverse. try()
    iXTX<-try(solve(t(X)%*%X), silent=TRUE)

    # If try caught an error then the singular matrix counter increases by 1
    if(inherits(iXTX, "try-error")){fails<-fails+1}

    #Otherwise calculate beta estimates for the new data frame and add to
    #the betas matrix
    else{
      betaHat<-iXTX %*% t(X) %*% Y
      betas[i,]<-betaHat
    }

    #End of iterations loop
  }

  #Remove na rows from betas data frame
  betas<-stats::na.omit(betas)

  #Summary statistics
  summ<-sapply(betas, function(x){
    ave<-mean(x)
    s<-stats::sd(x)
    min<-min(x)
    max<-max(x)
    n<-length(x)
    return(list("Count"=n ,"Mean"=ave, "Std Dev"=s, "Min"=min, "Max"=max))
  })

  #Bootstrap Confidence intervals
  cis<-mapply(betas, alpha, FUN=function(x,a){
    b1<-stats::quantile(x, probs=a/2)
    b2<-stats::quantile(x, probs=1-a/2)
    E<-mean(x)
    L<-min(b1,b2)
    U<-max(b1,b2)
    ret<-c( "Estimate"=E, "Lower"=L, "Upper"=U)
    ret
  })




  #The list to be returned
  ret<- cis



  #Printing the return to the console here because otherwise it shows up in the
  #middle of histograms
  #print(ret)
  ret

}

oCI<-function(data, alpha=0.05){

  dfree=nrow(data)-ncol(data)
  t<-stats::qt(1-alpha/2, dfree)
  z<-stats::qnorm(1-alpha)

  Yo<-as.matrix(data[,1])
  Xo<-data[-1]
  Xo<-as.matrix(cbind(1,Xo))
  iXTXo<-try(solve(t(Xo)%*%Xo), silent=TRUE)


  # If try caught an error then the singular matrix counter increases by 1
  if(inherits(iXTXo, "try-error")){
    print("Original Design Matrix Singular")
    betaHato<-NULL}

  #Otherwise calculate beta estimates
  else{
    betaHato<-iXTXo %*% t(Xo) %*% Yo
  }

  names(betaHato)<-names(data)
  names(betaHato)[1]<-"Intercept"

  #Calculate s to be used in confidence interval
  SSEo<-t(Yo) %*% Yo - t(betaHato) %*% t(Xo) %*% Yo
  so<-sqrt(SSEo/dfree)

  #Calculate estimates and confidence intervals
  cio<-data.frame(matrix(NA, ncol=nrow(betaHato), nrow=3))
  names(cio)<-names(betaHato)

  for(k in 1:length(betaHato)){
    EE<-betaHato[k]
    bb1<- EE + t*so*sqrt(iXTXo[k,k])
    bb2<- EE - t*so*sqrt(iXTXo[k,k])
    Lo<- min(bb1, bb2)
    Uo<- max(bb1, bb2)
    ret<-c( "Estimate"=EE, "Lower"=Lo, "Upper"=Uo)
    cio[,k]<-ret
  }

  rownames(cio)<-c("Estimate", "Lower", "Upper")
  cio
}

ciPlots<-function(data, iter, alpha=0.05){
  cib<-as.data.frame(t(ciComps(data, iter, alpha)))
  cib<-cbind(rownames(cib), cib)
  rownames(cib)<-NULL
  names(cib)[1]<-"Beta"
  cib$TYPE<-rep(as.factor("b"), nrow(cib))


  cio<-as.data.frame(t(oCI(data, alpha)))
  cio<-cbind(rownames(cio), cio)
  rownames(cio)<-NULL
  names(cio)[1]<-"Beta"
  cio$TYPE<-rep(as.factor("o"), nrow(cio))


  df<-dplyr::full_join(cib, cio)
  df<-as.data.frame(df)

  names(df)[1]<-"Betas"


  plot<-ggplot2::ggplot(df, aes(x=Betas, y=Estimate, color=TYPE ))+
    ggplot2::geom_point()

  print(plot)

}
