######################################################################################################
# This script fits an ARIMA model to a wood price time series in order to make a forecast.
# Uncertainty about wood price forecasts are acknowledged by creating n.boot bootstraps of the 
# residuals of the best ARIMA model fit, adding each of the resampled residuals to the best fit 
# model predictions, fitting an ARIMA to each and making n.boot forecasts respectively.
#
# Questions? naomi.radke@ife.uni-freiburg.de
#
######################################################################################################

# Set working directory
setwd("~/timber price")

# Load required packages
library(forecast) # for fitting arima and forecasting
library(tseries)  # for turning data into time series data
library(ggplot2)  # for plotting

# Load price data as time series
WP <- read.csv("Data/Preis_Index_Buch_1971_2016.csv", sep = ";")# read the data from csv
WP_ts <- ts(WP$price_index, start = 1971)                  # turn it into time series
plot.ts(WP_ts)                                             # check for stationarity 

#### MANUAL AND AUTOMATED ARIMA ######################################

# STEP 1: Test for stationarity/ Identify d

# If non-stationary, use diff to de-trend the data
WP_ts_diff1 <- diff(WP_ts, differences = 1)
plot(WP_ts_diff1)


# Conduct formal test for stationarity

  # e.g. with the Augmented Dickey-Fuller unit root test (p-val should be below 0.05/5%)
  print(adf.test(WP_ts_diff1))


# STEP 2: Identifying p and q
  
  # making a correlogram and partial correlogram
  acf(WP_ts_diff1)    # shows that q is 1
  pacf(WP_ts_diff1)   # shows that p is 0
  
  WP_ts_arima <- Arima(WP_ts, order = c(1,1,0), include.mean = FALSE) # This could be our arima model
  
  
  # Let's see what the automated arima function gives us 
  auto <- auto.arima(WP_ts, stepwise = FALSE, approximation = FALSE, seasonal = FALSE)  # let the auto.arima function do that for us
                                                                      # stepwise+approximation set to FALSE to 
                                                                      # consider all possible models in the search
  summary(auto) # shows the fitted arima model
  
# STEP 3: Check the residuals
  checkresiduals(auto)  # check for normality / ACF = whether autorcorrelations are within threshold
  
  
# STEP 4: Forecast  

  # for the manual arima
  WP_forecast <- forecast(WP_ts_arima, h = 60)
  summary(WP_forecast)
  
  # for the automated arima
  for_auto_arima <- forecast(auto, h = 60)
 
  # plot the best fit arima -> will add uncertain forecast scenario lines to the graph lateron
   g <- autoplot(for_auto_arima)
  


####### CREATING UNCERTAIN SCENARIOS #########################
  
  #---- test: making future trajectories (from adding bootstrapped residuals to forecast) using simulate()
   
  n.yrs = 30 # number of forecasted years
  n.sim = 10 # number of sampled future trajectories
   
  sim <- ts(matrix(0, nrow = n.yrs, ncol = n.sim), start = end(WP_ts)[1]+1) # empty ts matrix to be filled with simulations
  for(i in 1:n.sim){
    sim[,i] <- simulate(auto, nsim = n.yrs)
  }
    
  autoplot(WP_ts)+
    autolayer(sim)+
    guides(legend = "none")
  
 
   
   #-----------
   
  # STEP 1: Create a vector of residuals (fitted vs. data)
  resids <- auto$residuals
  
  # STEP 2: Make n.boot bootstraps of the residuals
  n.boot = 100 # define number of samples
  t = 60      # number of years for forecast
  set.seed(2019)
  boot.resids <- data.frame(replicate(n.boot, sample(resids, length(resids), replace = TRUE)))
  
  boot.for <- data.frame(matrix(0, ncol = t, nrow = n.boot)) # initialize a vector for the forecasts
  
  # STEP 3: For each bootstrap add residuals to best fit arima model, fit a new model and make a forecast
  for(i in 1:n.boot){
    
    # add the residuals to the best fit arima model
    boot.WP <- auto$fitted + boot.resids[,i]
    
    # fit an arima model to the new data
    auto_boot <- auto.arima(boot.WP, stepwise = FALSE, approximation = FALSE, seasonal = FALSE)
    
    # forecast new model
    for_auto_boot <- forecast(auto_boot, h=t)
    
    # save forecasted WP scenarios in a data frame
    boot.for[i,] <- for_auto_boot$mean 
    
    g <- g + autolayer(for_auto_boot$mean)
  }
  

# plot the best fit arima model, its forecast (black line) and n.boot forecast realizations 
  g+
    scale_color_manual(labels = c("bootstrap"), values = c(2))+
    ggtitle("Forecasts from ARIMA(1,0,0) and resampled residuals")
  