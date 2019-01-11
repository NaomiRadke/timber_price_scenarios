######################################################################################################################
#
# This script creates n.scenarios carbon tax scenarios by taking the carbon tax for Germany of the years 2005-2017 
# (source: source: https://de.investing.com/commodities/carbon-emissions-historical-data, 2018), fitting a distribution to 
# the carbon tax, and then making random draws for each harvest period and adding it to the carbon tax of 2017.
#
######################################################################################################################

# Load packages
library(fitdistrplus) # for fitting distribution
library(lhs) # for Latin Hypercube Sampling
library(ggplot2) # for plotting

# Load data
C_tax <- read.csv("carbon_tax.csv", sep = ";")

# make a historgram of the price index
hist(C_tax$C_tax)


# fit a lognormal distribution
distr <- fitdist(C_tax$C_tax, "lnorm")
summary(distr)
m <- distr$estimate["meanlog"]
sd <-distr$estimate["sdlog"] # extract the standard deviation of the lognormal distribution 

# calculate the tax with maximum probability
grid <- seq(0,30, 0.1)
max_probability <- grid[which.max(dlnorm(grid,m, sd))] # price with maximum probability

# draw random samples from the lognormal distribution (with replacement)

test <- rlnorm(100, meanlog = m, sdlog = sd)

# make a plot of original histogram with fitted lognormal curve
hist(C_tax$C_tax, prob = TRUE, ylab = "density", xlab = "carbon tax (Euro/ton C)")
curve(dlnorm(x, m, sd), add = TRUE, col = "red")

ggplot(C_tax, aes(x = C_tax))+
  geom_histogram(aes(y=..density..),
                 binwidth = 5,
                 colour = "black", fill = "blue", alpha = .2)+
stat_function(fun = dlnorm, args = list(meanlog = m, sdlog = sd),size = 1, color = "red", alpha = .2)+
  labs(x = "Euro/ ton carbon", title = "Density of carbon tax scenarios")+
  theme(text = element_text(size =20))
