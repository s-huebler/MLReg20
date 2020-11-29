#' Bootstrap analysis of a multiple linear regression model
#'
#' @param data , a data frame with first column the dependent variable and all other columns predictors
#' @param iter , number of bootstrap iterations
#' @param alpha , confidence level
#'
#' @return , command line statistics and plots
#' @export
#'
#' @examples
#'require(datasets)
#'
#' iris<-as.data.frame(datasets::iris)
#' iris<-within(iris, {
#' Setosa<-ifelse(Species=="setosa", 1, 0)
#' Versicolor<-ifelse(Species=="versicolor", 1, 0)
#' rm(Species)
#' })
#'
#' mlrBoot(iris, 1000)
#'
mlrBoot<-function(data, iter, alpha=.05){
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
    s<-sd(x)
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

  #Original MLR
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

  names(betaHato)<-names(betas)

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


  #The list to be returned
  ret<-list("Bootstrap Summary Statistics"=summ,
            "Singular Matrices"=fails,
            "Bootstrap Confidence Intervals"=cis,
            "Original MLR Confidence Intervals"=cio
  )

  #Printing the return to the console here because otherwise it shows up in the
  #middle of histograms
  print(ret)

  #Histograms
  mapply(betas, 0:(ncol(betas)-1),
         FUN=function(vec, index){graphics::hist(vec,
                                                 main=paste("Histogram of Beta",index),
                                                 xlab=paste("Beta", index, "Values"))},USE.NAMES = TRUE)

  #Return what was printed before as an invisible list
  invisible(ret)

}
