######################################################################################################
# This script fits an ARIMA model to a carbon tax time series in order to simulate scenarios.
# Uncertainty about wood price forecasts are acknowledged by creating n.boot bootstraps of the 
# residuals of the best ARIMA model fit, adding each of the resampled residuals to the best fit 
# model predictions, fitting an ARIMA to each and making n.boot forecasts respectively.
#
# Questions? naomi.radke@ife.uni-freiburg.de
#
######################################################################################################

# Set working directory
setwd("~/timber_price_scenarios")

# Load required packages
library(forecast) # for fitting arima and forecasting
library(tseries)  # for turning data into time series data
library(ggplot2)  # for plotting
library(ggfortify)
library(tsibble) # for saving ts data as csv

# Load carbon tax data as time series
C_tax <- read.csv("Data/carbon_tax.csv", sep = ";") #record
C_tax$Date <- as.Date(C_tax$Date, "%d.%m.%Y") # change date column to date format
C_tax$Year <- format(C_tax$Date, "%Y") # add a year column in date format
C_tax_yearly <- aggregate(C_tax ~ Year, C_tax, mean)
C_tax_yearly$Year <- as.Date(C_tax_yearly$Year, "%Y") # change year from character to date format

# Load expert scenarios data
C_tax_scen <- read.csv("Data/carbon_scenarios.csv", sep = ";") # scenarios
scen <- ts(as.matrix(C_tax_scen[,-1]), start = 2019) # create a multivariate time series object

# turn yearly carbon tax into time series
C_tax_ts <- ts(C_tax_yearly$C_tax, start = 2005) 

# plot the time series to check for stationarity
plot.ts(C_tax_ts) 

### FIT AN ARIMA MODEL ###############################################################################

# fit an arima model with automatic function
CT_arima <- auto.arima(C_tax_ts, stepwise = FALSE, approximation = FALSE, seasonal = FALSE)  # stepwise+approximation set 
                                                                                      # to FALSE to consider all
                                                                                      # possible models in the search
summary(CT_arima) # shows the fitted arima model -> (0,1,0) = random walk


### CREATING UNCERTAIN SCENARIOS #####################################################################

# Some settings
n.yrs = 30 # number of forecasted years
n.sim = 10 # number of sampled future trajectories

# Simulating n.sim future trajectories (from adding bootstrapped residuals to forecast)
sim <- ts(matrix(0, nrow = n.yrs, ncol = n.sim), start = end(C_tax_ts)[1]+1) # empty ts matrix to be filled with simulations

for(i in 1:n.sim){
  sim[,i] <- simulate(CT_arima, nsim = n.yrs)
}

# Plot the recoreded time series, plausible future trajectories and expert scenarios  
ts.plot(sim[,-c(2,7)], C_tax_ts, scen, 
        gpars = list(col = c(rep("blue", 8), "black", rep("green3", 10)), 
                     ylab = "Carbon tax (Euro/ton C)", 
                     main = "Carbon tax development and plausible future paths"))

# write csv of simulated scenarios for use in Sobol sensitivity 
sim_df <- as.data.frame(sim) # turn ts into data frame
row.names(sim_df) <- c(time(sim)) # add years as row numbers
write.csv(sim_df, "Output/carbon_tax_scen.csv") # write csv and save to output folder
