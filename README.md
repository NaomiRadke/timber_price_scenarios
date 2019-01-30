TIMBER PRICE SCENARIOS FOR UNCERTAIN TIME SERIES USING ARIMA

Main script:
prices_arima.R
The script loads the timber price data that is stored in the "Data" folder and uses the automated ARIMA function of the "forecast" package to fit an ARIMA model to the timber price time series.
It then creates n.boot bootstraps of the residuals, adds each residuals scenario to the best fit model and again fits an ARIMA to each of these resampled time series. These new n.boot models are used to make n.boot forecasts which are then plotted.
The goal is to select about 10 forecast scenarios by eye that will serve as uncertain scenarios for a Sobol analysis.
