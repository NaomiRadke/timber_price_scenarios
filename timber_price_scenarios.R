######################################################################################################################
#
# This script creates n.scenarios timber price scenarios by taking a timber price index for Germany of the years 1976-2016 
# (source: Destatis, Preisindizes für die Land- und Forstwirtschaft, 2018), fitting a distribution to the wood prices with 
# zero mean, and then making random draws for each harvest period and adding it to the wood price of 2016.
#
######################################################################################################################

# Load packages
library(fitdistrplus) # for fitting distribution
library(lhs) # for Latin Hypercube Sampling
library(dplyr) # for melting list of dataframes into a single dataframe

# Load data
price <- read.csv("price_index_destatis_1976-16.csv", sep = ";")

# make a historgram of the price index
hist(price$price_index_all_wood)

# fit a lognormal distribution
distr <- fitdist(price$price_index_all_wood, "lnorm")
summary(distr)
sdlog <-distr$estimate["sdlog"] # extract the standard deviation of the lognormal distribution  

# calculate the price with maximum probability

grid <- seq(0,150,1)
dlnorm(grid,4.44, 0.177)

max_probability <- grid[which.max(dlnorm(grid,4.44, 0.177))] # price with maximum probability

# center the lognormal distribution around the wood price of 2016
p_2016 <- # wood price of 2016
dist <- p_2016 - max_probability #distance by which to move the lognorm distribution


#####################################

# Just to demonstrate: X equals Y
X <- rlnorm(100, 4.44, 0.177)
Y <- exp(rnorm(100, 4.44, 0.177))

####################################

# ----- Alternative: uniform sampling from x = potency x= [3.49;3.7]

# 
set.seed(1234)
n.scenarios <- 100 # number of price scenarios we want to create
n.years <- 10 # number of simulation years
bound_lower <-  rep(3.49, n.years)
bound_upper <- rep(3.7, n.years)


price_scenarios <- randomLHS(n.scenarios, n.years) # creates samples between [0,1]
price_scenarios_new <- price_scenarios # create new matrix to be filled

for (p in 1:n.years) {
  price_scenarios_new[,p] <- bound_lower[p] + (bound_upper[p]-bound_lower[p])*(price_scenarios[,p])
}


# Plot different revenue per dbh scenarios

dbh <- seq(1, 60, 1)
# Make a list of dataframes for every scenario
rev_scenarios <- lapply(c(1:n.scenarios), function(i){
  rev <- 0.0001*dbh^price_scenarios_new[i,1]
  scenario <- data.frame("dbh" = dbh, "revenue" = rev)  
})

# Make a long dataframe out of the list for use in ggplot
rev_scenarios <- bind_rows(rev_scenarios, .id = "scenario")


# Plot all scenarios

ggplot(data = rev_scenarios, aes(x= dbh, y= revenue, group = scenario, colour = scenario))+
  geom_line()+
  labs(y="revenue (Euro/stem)", x = "diameter at breast height (cm)", title = "Diameter-dependent Revenue Function")+
  theme(legend.position = "none", text = element_text(size =20))
