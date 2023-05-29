library(rugarch) # package for GARCH modelling
library(forecast) #t ools for analysing time series forecasts
library(data.table) # tools for managing the data frame
library(lubridate) # better handling of dates
library(ggplot2)  # plot environment

#Specifying model parameters to generate a time series following aGARCH(1,1) process
nu <- 7 #degress of freedom of the standardized innovation t_nu
fix.p <- list(  mu = 0, # constant c
                omega = .05, # omega 
                alpha1 = 0.1, # alpha-GARCH(1,1) 
                beta1 = 0.85, # beta-(GARCH(1,1)
                shape = nu) # dof for standardized t-distributed innovations

#Specifying GARCH(1,1)
specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder =c(1,1)), # GARCH(1,1)
                            mean.model = list(include.mean = FALSE,armaOrder = c(0,0)), # no mean model
                            fixed.pars = fix.p, distribution.model = "std") # "std" for Student-t distributed residuals


# Generating Y_t
n <- 500 # sample
return <- ugarchpath(specification, n.sim = n, m.sim = 1, rseed = 1) # n.sim = path length and m.sim = number of paths


#Containing  artificial generated series of Y_t
Y <- fitted(return) # simulated process of Y_t following the GARCH(1,1)
sig <- sigma(return) # sigma_t (volatility)
eps <- return@path$residSim # epsilon_t 



# Fitting an GARCH(1,1) model on artifical generated returns Y_t
spec <- ugarchspec(variance.model =list(model = "sGARCH", garchOrder = c(1,1)) , # GARCH(1,1) 
                   mean.model = list(include.mean = FALSE,armaOrder = c(0,0)), #no mean model
                   distribution.model = "std") # "std" for Student-t distributed residuals
fitting <- ugarchfit(spec, data = Y)  #model fit 
fitting


#GARCH(1,1) forecast, VaR-and ES forecast
alpha=0.05 #alpha-level
forecast <- ugarchforecast(fitting, n.ahead = 1, n.roll = 0, out.sample = 0) #one-step ahead forecast
sigma_hat <- sigma(forecast) #containing volatility forecast
f <- function(l, fit) fGarch::qstd(p=l, mean=0, sd=1, nu=fitting@fit$coef["shape"]) # define quantile-function
q_sgarch.t <- fitted(forecast) + sigma_hat  * f(alpha,fit) #VaR calculation using location-scale relationship

q_sgarch.t 

es_sgarch.t <- fitted(forecast)+ sigma_hat  * integrate(f, 0, alpha, fit=fitting)$value/alpha #ES calculation using location-scale relationship
es_sgarch.t

#Graphical illustration
library(ggplot2)

#risk-measure one step ahead forecasts
dt_limit <- function(x=x1) {
  y <- fGarch::dstd(x, nu=nu) # ensure to use Student-t disitribution
  y[x>fGarch::qstd(nu=nu,p=alpha)] <- NA
 return(y)}

stat_function(fun = dnorm) # additional inclusion of the normal distribution for comparing purposes
riskmeasures<-ggplot() + 
  xlim(-5,5)+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red")+
  stat_function(fun = fGarch::dstd, args = list(nu = nu), color = "green")+
  stat_function(fun = dt_limit, geom = "area", fill = "green", alpha = 0.2)+
  geom_point(aes(x = es_sgarch.t, y = 0, color = "ES"), size = 1)+
  geom_point(aes(x = q_sgarch.t, y = 0, color = "VaR"), size = 1)+
  labs(y = "density", x = "return", color = "") +
  theme(legend.position = "none")
ggsave(filename = "C:/Users/illne/Desktop/Uni/6.Semester/Seminar Ökonometrie/Paper/Density Simulation.png", plot = riskmeasures, dpi = 600, width = 8, height = 5)
#Graphical illustration of risk-measure time series
f <- function(l) fGarch::qstd(p=l, mean=0, sd=1, nu=7) #define quantile function
alpha=.05

#Creating data-frame containing relevant time-series
riskmeasures<-data.frame(
  "timeindex" = 1:nrow(Y),
  "return"=Y, 
  "vola"= sig, 
  "VaR"=  0 + sig  * f(l=alpha) ,
  "ES" = 0 + sig  * integrate(f, 0, alpha)$value/alpha
) 
riskmeasures$violations=(riskmeasures$return<riskmeasures$VaR) # risk measure violations

riskmeasures.ts<-ggplot(data = riskmeasures, aes(x = timeindex, y = return)) +
  geom_line(color="black", alpha = 0.7) +
  geom_line(aes(y=vola), color = "darkorange") +
  geom_line(aes(y=VaR), color = "green") +
  geom_line(aes(y=ES), color = "blue") +
  geom_point(data=subset(riskmeasures, return < VaR), aes(y = return), color="darkred")
ggsave(filename = "C:/Users/illne/Desktop/Uni/6.Semester/Seminar Ökonometrie/Paper/Time Series Simulation.png", plot = riskmeasures.ts, dpi = 600, width = 8, height = 5)
sum(riskmeasures$violations==TRUE) #number of violations
