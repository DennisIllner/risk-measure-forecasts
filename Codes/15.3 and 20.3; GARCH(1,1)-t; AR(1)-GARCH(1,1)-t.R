##### Load packages #####
library(moments) # package for calculating moments
library(rugarch) # package for GARCH modelling
library(forecast)  # tools for analysing time series forecasts
library(data.table) # tools for managing the data frame
library(lubridate) # better handling of dates
library(tidyverse) #collection of packages for handling data-science workflows
library(dplyr) #options to change the data frame
library(ggplot2) # plot environment

rm(list=ls())
setwd("C:/Users/illne/Desktop/Uni/6.Semester/Seminar Ökonometrie/Empirical Estimation")
getwd()
data <- read.csv("XDWF.DE.csv",header=TRUE, sep = ",")
class(data) #check class of the data
Index <- as.data.table(data) #create new data table
Index[33, 2:7]=NA # replacing undefined values for further calculations
Index[ , Date:=ymd(Date)] #converting date format
Index$Adj.Close <- as.numeric(Index$Adj.Close)  #converting Adj.Close to numeric values

#Compute log-returns y_t
Index[ , lret:= c(NA, diff(log(Adj.Close)))]

Index <- na.omit(Index) # remove missing values



#Model Fit GARCH(1,1)-t and one-step ahead forecasts to 15.3
spec<-ugarchspec(variance.model =list(model = "sGARCH", garchOrder = c(1,1)) , #GARCH(1,1) 
                 mean.model = list(include.mean = FALSE,armaOrder = c(0,0)), # no mean model
                 distribution.model = "std") #Student-t
Garchfit15<-ugarchfit(spec,data=Index[1:1498, "lret"]) #model fit
Garchfit15
alpha=0.05 #define alpha-level
Garchforecast15 <- ugarchforecast(Garchfit15, n.ahead = 1, n.roll = 0, out.sample = 0) # one-step-ahead forecast
sigma_hat <- sigma(Garchforecast15) #contain volatility
sigma_hat
f <- function(l, fit) fGarch::qstd(p=l, mean=0, sd=1, nu=Garchfit15@fit$coef["shape"]) #define quantile function
GarchVar15 <- fitted(Garchforecast15) + sigma_hat  * f(alpha,fit) #VaR forecast

GarchVar15                                                    
GarchES15 <- fitted(Garchforecast15)+ sigma_hat  * integrate(f, 0, alpha, fit=Garchfit15)$value/alpha #ES forecast
GarchES15


#Model Fit GARCH(1,1)-t and one-step ahead forecasts to 20.3
spec1<-ugarchspec(variance.model =list(model = "sGARCH", garchOrder = c(1,1)) , 
                 mean.model = list(include.mean = FALSE,armaOrder = c(0,0)),
                 distribution.model = "std")
Garchfit20<-ugarchfit(spec1,data=Index[1:1501, "lret"])
Garchfit20
alpha=0.05
Garchforecast20 <- ugarchforecast(Garchfit20, n.ahead = 1, n.roll = 0, out.sample = 0)
sigma_hat1 <- sigma(Garchforecast20)
sigma_hat1
f <- function(l, fit) fGarch::qstd(p=l, mean=0, sd=1, nu=Garchfit20@fit$coef["shape"]) 
GarchVar20 <- fitted(Garchforecast20) + sigma_hat1  * f(alpha,fit) 

GarchVar20                                                   
GarchES20 <- fitted(Garchforecast20)+ sigma_hat1  * integrate(f, 0, alpha, fit=Garchfit20)$value/alpha
GarchES20



#Model Fit AR(1)-GARCH(1,1)-t and one-step ahead forecasts to 15.3
spec2<-ugarchspec(variance.model =list(model = "sGARCH", garchOrder = c(1,1)) , 
                 mean.model = list(include.mean = FALSE,armaOrder = c(1,0)),
                 distribution.model = "std")
ArGarchfit15<-ugarchfit(spec2,data=Index[1:1498, "lret"])
ArGarchfit15

########short residual analysis to show AR(1) could be a possible model#############
plot(ArGarchfit15, which = 10)
resAR1<-ArGarchfit15@fit$residuals
Pacf(resAR1, lag.max = 30, )
# We do not detect any significant spikes different from zero in the correlation functions which justifies the proposal of an AR(1)
####################################################################################
alpha=0.05
ArGarchforecast15 <- ugarchforecast(ArGarchfit15, n.ahead = 1, n.roll = 0, out.sample = 0)
sigma_hat2 <- sigma(ArGarchforecast15)
sigma_hat2
f <- function(l, fit) fGarch::qstd(p=l, mean=0, sd=1, nu=ArGarchfit15@fit$coef["shape"]) 
ArGarchVar15 <- fitted(ArGarchforecast15) + sigma_hat2  * f(alpha,fit) 

ArGarchVar15                                                    
ArGarchES15 <- fitted(ArGarchforecast15)+ sigma_hat2  * integrate(f, 0, alpha, fit=ArGarchfit15)$value/alpha
ArGarchES15

#Model Fit AR(1)-GARCH(1,1)-t and one-step ahead forecasts to 20.3
spec3<-ugarchspec(variance.model =list(model = "sGARCH", garchOrder = c(1,1)) , 
                  mean.model = list(include.mean = FALSE,armaOrder = c(1,0)),
                  distribution.model = "std")
ArGarchfit20<-ugarchfit(spec3,data=Index[1:1501, "lret"])
ArGarchfit20
alpha=0.05
ArGarchforecast20 <- ugarchforecast(ArGarchfit20, n.ahead = 1, n.roll = 0, out.sample = 0)
sigma_hat3 <- sigma(ArGarchforecast20)
sigma_hat3
f <- function(l, fit) fGarch::qstd(p=l, mean=0, sd=1, nu=ArGarchfit20@fit$coef["shape"]) 
ArGarchVar20 <- fitted(Garchforecast20) + sigma_hat3  * f(alpha,fit) 

ArGarchVar20                                                   
ArGarchES20 <- fitted(ArGarchforecast20)+ sigma_hat3  * integrate(f, 0, alpha, fit=ArGarchfit20)$value/alpha
ArGarchES20



