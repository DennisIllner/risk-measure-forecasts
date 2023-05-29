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
setwd("C:/Users/illne/Desktop/Uni/6.Semester/Seminar Ökonometrie/Empirical Estimation")
getwd()
data <- read.csv("XDWF.DE.csv",header=TRUE, sep = ",")
class(data) #check class of the data
Index <- as.data.table(data) #create new data table
Index[33, 2:7]=NA # replacing undefined values for further calculations
Index[ , Date:=ymd(Date)] #converting date format
Index$Adj.Close <- as.numeric(Index$Adj.Close) #converting Adj.Close to numeric values

#Plot of Adj.Close Prices
ggplot(data = Index, mapping = aes(x = Date, y = Adj.Close)) +
  geom_line()+
  ggtitle("Adj.Close Price of the Xtrackers MSCI World Financials UCITS ETF")

#Compute log-returns y_t
Index[ , lret:= c(NA, diff(log(Adj.Close)))]

Index <- na.omit(Index)

#Plot log-returns y_t
ggplot(data=Index, aes(x=Date,y=lret))+
  geom_line()+
  ggtitle("Returns of the Xtrackers MSCI World Financials UCITS ETF")+
  labs(y="Daily Return", x="time")

#Calculation of the unconditional mean E[y_t]
uncond.mean.lret<-mean(Index$lret)
uncond.mean.lret
#The uncond. mean of the returns is approximately 0.
#Therefore we can calculate the variance-proxy with the squared Returns
#Note Var(Y_t)=E[Y_t-E[Y_t]]^2 with E[Y_t]=0 

Index <- na.omit(Index) #remove missing values
# calculation of squared returns
Index[ , lret2:= lret^2]

#Plot of squared returns
ggplot(data=Index, aes(x=Date,y=lret2))+
  geom_line()+
  ggtitle("uncond. Variance of the Returns")+
  ylab("Daily Variance")
xlab("Time")
#From the returns and squared returns we can see that volatility is time varying



#Checking dependencie structures of returns and squared returns
Acf(Index$lret, lag.max = 40, main="ACF Returns", xlab="", ylab="")
Acf(Index$lret2, lag.max = 40, main="ACF Variance", xlab="", ylab="")
Pacf(Index$lret, lag.max = 40, main="PACF Returns", xlab="",ylab="")
Pacf(Index$lret2, lag.max = 40, main="PACF Variance", xlab="", ylab="")
#In the squared Returns we observe a clearly time dependence with a large number of significant autocorrelations different from zero.

#density plot of the returns y_t
ret<-Index$lret
ggplot()+
  geom_density(aes(ret))

#Calculate Kurtosis 
kurtosis<-kurtosis(ret)
kurtosis 

