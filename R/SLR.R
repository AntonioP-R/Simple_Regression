SimpleLinearRegression<-function(Xvar,Yvar,Regression_Plot = FALSE,Residuals_Plot = FALSE){
  if(length(Xvar) == length(Yvar)){
    if(is.numeric(Xvar)&is.numeric(Yvar)){
      Xmean<-mean(Xvar)
      Ymean<-mean(Yvar)
      Xdeviance<-0
      Ydeviance<-0
      covXY<-0
      for(i in 1:length(Xvar)){
        Xdeviance <- ((Xvar[i]-Xmean)^2)+Xdeviance
        Ydeviance <- ((Yvar[i]-Ymean)^2) +Ydeviance
        covXY<-((Xvar[i]-Xmean)*(Yvar[i]-Ymean)) + covXY
      }
      Xvariance<-Xdeviance/length(Xvar)
      Yvariance<-Ydeviance/length(Yvar)
      Xstd<-sqrt(Xvariance)
      Ystd<-sqrt(Yvariance)
      covXY<-covXY/length(Xvar)
      beta<-covXY/Xvariance
      alfa<-Ymean-(beta*Xmean)
      phi<-covXY/(Xstd*Ystd)
      RSquared<-phi^2
      ty <- c(1:length(Yvar))
      residuals <- c(1:length(Yvar))
      MSE<-0
      for(i in 1:length(Yvar)){
        ty[i] <- alfa + beta*Xvar[i]
        residuals[i] <- Yvar[i] - ty[i]
        MSE <- (residuals[i]^2) + MSE
      }
      MSE <- MSE/(length(Yvar)-2)
      RMSE <- sqrt(MSE)
      stErrorAlfa <- RMSE*sqrt((1/length(Yvar))+((Xmean^2)/Xdeviance))
      StErrorBeta <- (RMSE/sqrt(Xdeviance))
      stresiduals <- c(1:length(Yvar))
      for(i in 1:length(Yvar)){
        stresiduals[i] <- residuals[i]/RMSE
      }
      stresiduals<-sort(stresiduals)
      DensResiduals <- c(1:length(Yvar))
      for(i in 1:length(Yvar)){
        DensResiduals[i] <- (1/sqrt(2*pi))*(exp(-(stresiduals[i])^2/2))
      }
      Values <- c(signif(beta,4),signif(alfa,4),round(RSquared,3),round(MSE,3),round(RMSE,3))
      LRmodel<-data.frame(Values)
      LRmodel$St.Error <- c(round(StErrorBeta,3),round(stErrorAlfa,3)," "," "," ")
      LRmodel<-LRmodel
      row.names(LRmodel)<- c("Slope:","Intercept:","R-Squared:","MSE:","RMSE:")
      if(isTRUE(Regression_Plot)&isTRUE(Residuals_Plot)){
        #Make a regression plot
        plot(Xvar,Yvar,type = "p",pch = 16,col = "blue",
             main = "REGRESSION PLOT",xlab = "Independent Variable",
             ylab = "Dependent Variable",cex.axis = 0.7, las = 1,
             cex.main = 1.1,font.lab = 2)
        grid(col = "black")
        abline(alfa,beta,lwd = 1.4,col = "red")
        #Make a residuals plot
        plot(Xvar,residuals,type = "p",pch = 16,col = "blue",
             main = "RESIDUALS PLOT",xlab = "Independent Variable",
             ylab = "Dependent Variable",cex.axis = 0.7, las = 1,
             cex.main = 1.1,font.lab = 2)
        abline(0,0,lwd = 1.5,col ="red")
        grid(col = "black")
        #Make a histogram with the stresiduals
        hist(stresiduals,probability = TRUE,col = "#1e90ff",ylab = "Density",
             xlab = "Standardized Residuals",las =1,cex.axis = 0.7,
             main = "Standardized Residuals",cex.main = 1.3,font.lab = 2,cex.lab = 1)
        par(new = TRUE)
        plot(stresiduals,DensResiduals,type = "l",pch = 16,col = "red",lwd =1.8,xlab =" ",ylab =" ",
             xaxt = "n",yaxt = "n")
        return(LRmodel)
      }else if(isFALSE(Regression_Plot)&isTRUE(Residuals_Plot)){
        #Make a residuals plot
        plot(Xvar,residuals,type = "p",pch = 16,col = "blue",
             main = "RESIDUALS PLOT",xlab = "Independent Variable",
             ylab = "Dependent Variable",cex.axis = 0.7, las = 1,
             cex.main = 1.1,font.lab = 2)
        abline(0,0,lwd = 1.5,col ="red")
        grid(col = "black")
        #Make a histogram with the stresiduals
        hist(stresiduals,probability = TRUE,col = "green",ylab = "Density",
             xlab = "Standardised Residuals",las =1,cex.axis = 0.7, main = "Standardised Residuals",
             cex.main = 1.3,font.lab = 2,cex.lab = 1)
        par(new = TRUE)
        plot(stresiduals,DensResiduals,type = "p",pch = 16,col = "black",lwd = 1.6,xlab =" ",ylab =" ",
             xaxt = "n",yaxt = "n")
        return(LRmodel)
      }else if(isTRUE(Regression_Plot)&isFALSE(Residuals_Plot)){
        #Make a regression plot
        plot(Xvar,Yvar,type = "p",pch = 16,col = "blue",
             main = "REGRESSION PLOT",xlab = "Independent Variable",
             ylab = "Dependent Variable",cex.axis = 0.7, las = 1,
             cex.main = 1.1,font.lab = 2)
        grid(col = "black")
        abline(alfa,beta,lwd = 1.4,col = "red")
        return(LRmodel)
      }else{
        return(LRmodel)
      }
    }else{
      print("Error! Input data are not 'numeric' type.")
    }
  }else{
    print("Error! Input data don't have the same size, check their size.")
  }
}


