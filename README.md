# risk-measure-forecasts
Financial risk measure forecasting based on GARCH-type models

We are interested in risk measure forecasting, especially the Value-at-Risk (VaR) and Expected shortfall (ES) based on GARCH-type models.
First, we performed one-step-ahead risk measure forecasts based on a GARCH(1,1)-model with an artificially generated data following a GARCH(1,1) process. 
You can find the replication material in our provided codes under "Simulation exercise.R"

We also made an empirical analysis. Using data of a globally invested bank ETF Xtrackers MSCI World
Financials UCITS we estimated Garch-type models under different distributional assumptions of the model innovations.
We distinguished between the Student's t-distribution and the normal distribution.
Our estimated models are the:
GARCH(1,1)-t, GARCH(1,1)-N, AR(1)-GARCH(1,1)-t and AR(1)-GARCH(1,1)-N.
For each model we performed VaR und ES forecasts for different dates. 
The results are available in the following codes: "15.3 and 20.3; GARCH(1,1)-t; AR(1)-GARCH(1,1)-t.R" and 15.3 and 20.3; GARCH(1,1)-N; AR(1)-GARCH(1,1)-N.R.
We also provide some descriptive statistics for our data set in "General descriptive analysis.R"

After model estimation we evaluated our models with a backtesting procedure. We performed one-step-ahead VaR forecasts based on a rolling window and calculated VaR violations
with different alpha-levels.
The replication material is available under the following codes:
-GARCH(1,1)-N 1% Rolling Window.R  
-GARCH(1,1)-N 5% Rolling Window.R
-GARCH(1,1)-t 1% Rolling Window.R
-GARCH(1,1)-t 5% Rolling Window.R
-AR(1)-GARCH(1,1)-N 1% Rolling Window.R
-AR(1)-GARCH(1,1)-N 5% Rolling Window.R
-AR(1)-GARCH(1,1)-t1% Rolling Window.R
-AR(1)-GARCH(1,1)-t5% Rolling Window.R

You can get the data set from the file named "XDWF.DE.csv".
