##### Load packages #####
library(moments) # package for calculating moments
library(rugarch) # package for GARCH modelling
library(forecast)  # tools for analysing time series forecasts
library(data.table) # tools for managing the data frame
library(lubridate) # better handling of dates
library(tidyverse) #collection of packages for handling data-science workflows
library(dplyr) #options to change the data frame
library(ggplot2) # plot environment

#Data preparation
rm(list=ls())
setwd("C:/Users/illne/Desktop/Uni/6.Semester/Seminar Ökonometrie/risk-measure-forecasts/Codes")
getwd()
data <- read.csv("XDWF.DE.csv",header=TRUE, sep = ",")
class(data) #check class of the data
Index <- as.data.table(data) #create new data table
Index[33, 2:7]=NA # replacing undefined values for further calculations
Index[ , Date:=ymd(Date)] #converting date format
Index$Adj.Close <- as.numeric(Index$Adj.Close) #converting Adj.Close to numeric values


#Compute log-returns y_t
Index[ , lret:= c(NA, diff(log(Adj.Close)))]

Index <- na.omit(Index) # remove missing values


# calculation of squared returns
Index[ , lret2:= lret^2]

################################################################
################################################################

#Model fit GARCH(1,1)-N

#specifying model
spec<-ugarchspec(variance.model =list(model = "sGARCH", garchOrder = c(1,1)) , # GARCH(1,1)
                 mean.model = list(include.mean = FALSE,armaOrder = c(0,0)),   # no mean model
                 distribution.model = "norm") #normal distribution

#specifying rolling window forecasts with ugarchroll-function. (#Note that you can include VaR calculation in this function by setting calculate.VaR = True and defining alpha-levels)
estim<-ugarchroll(spec, Index$lret, n.ahead = 1, forecast.length = 344, refit.every = 1, 
                  refit.window = c("moving"),  
                  calculate.VaR = TRUE, 
                  VaR.alpha = c(0.01, 0.05))   
VaR <- estim@forecast$VaR #containing VaR forecasts
as.data.frame(VaR)

#containing predcitions of y_t
preds <- as.data.frame(estim)
#containing estimated degrees of freedom nu
#as.numeric(preds$Shape)

#manual VaR calculations:
#Way one:
alpha=0.05 #define alpha-level
f <- function(l, preds) qnorm(p=l, mean=0, sd=1) #define quantile function
q_sgarch.t <- preds$Mu + preds$Sigma  * f(alpha,preds) #VaR calculation using location scale relationship



#Way two:
VAR = preds$Mu + preds$Sigma*qdist("norm",p=alpha, mu = 0, sigma = 1)
VaR


#ES caluclation:
#defining loop for passing scalars to the integrate function
f <- function(x) {
  result <- numeric(length(x))
  for (i in seq_along(x)) {
    result[i] <- qdist("norm", p = x[i], mu = 0, sigma = 1)
  }
  result
}

ES <- preds$Mu + preds$Sigma * integrate(f, 0, 0.05)$value / 0.05 # ES calculation 





#Assign our risk measures volatility, VaR and ES forecasts to the Index data frame 
Index$VaRforecast<-c(rep(NA,1189),VAR)
Index$ESforecast<-c(rep(NA,1189),ES)
Index$volaforecast<-c(rep(NA,1189),preds$Sigma)
RollingWindow<-tail(Index,500) 
as.numeric(RollingWindow$VaRforecast)

#plot the rolling window
ts.N<-ggplot(data=RollingWindow,aes(x=Date, y=lret))+
  geom_line(color="black",alpha=0.7)+
  geom_line(aes(y=volaforecast),color="darkorange")+
  geom_line(aes(y=VaRforecast), color="green")+
  geom_line(aes(y=ESforecast),color="blue")+
  geom_point(data=subset(RollingWindow, lret < VaRforecast), aes(y = lret), color="darkred")
ggsave(filename = "C:/Users/illne/Desktop/Uni/6.Semester/Seminar Ökonometrie/Paper/Rolling Window GARCH(1,1)-N.png", plot = ts.N, dpi = 600, width = 8, height = 5)
#Calculation of violations
RollingWindow$violations=(RollingWindow$lret<RollingWindow$VaRforecast)
sum(na.omit(RollingWindow$violations==TRUE))
