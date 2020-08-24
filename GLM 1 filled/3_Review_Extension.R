##########################
## Review and Extension ##
##      Kye Ripley      ##
##       9/6/19         ##
##########################

# Packages needed:
install.packages("DescTools")
library(DescTools)

# Data used:

############################
## Descriptive Statistics ##
############################

# In our last session, we talked about ways to import data
# and reviewed how to calculate some of the most commonly 
# used desriptive statistics:
# mean, median, etc. 
# Today, we'll expand on that a bit by looking at how to 
# export data and calculate an F statistic.

########
# Data #
########

# For today, we'll use the iris dataset that is available
# inside of R. We'll just be focusing on the Sepal.Length
# column.

dat <- iris
View(dat)

########
# Mean #
########

mean_sl <- mean(dat$Sepal.Length)

##########
# Median #
##########

median_sl <- median(dat$Sepal.Length)

########
# Mode #
########

mode_sl <- Mode(dat$Sepal.Length)

######################
# Standard Deviation #
######################

sd_sl <- sd(dat$Sepal.Length)

############
# Variance #
############

var_sl <- var(dat$Sepal.Length)

###############
# Mean 95% CI #
###############

mean_ci_sl <-qnorm(p = c(.025, .975), mean = mean_sl, sd = sd_sl)

######################
## Model Comparison ##
######################

# We're going to test the hypothesis that a single parameter model using 
# the sample mean is superior to a model using the mode as a constant.

###########
# Model C #
###########

# First, we need to subtract our constant from the observed values.

dat$error_C <- dat$Sepal.Length - 5

# We then need to square the error.

dat$sq_error_C <- dat$error_C^2

# We then sum the errors to get our sum of squared errors

SSE_C <- sum(dat$sq_error_C)
SSE_C

###########
# Model A #
###########

# We'll then follow the same process, except using our new parameter:
# the mean.

dat$error_A <- dat$Sepal.Length - mean_sl

# We then need to square the error.

dat$sq_error_A <- dat$error_A^2

# We then sum the errors to get our sum of squared errors

SSE_A <- sum(dat$sq_error_A)
SSE_A

#######
# PRE #
#######

# Finally, we need to calculate our PRE.

PRE <- 1 - (SSE_A/SSE_C)
PRE

#################
# Calculating F #
#################

# Remember:
# F = [PRE/(PA - PC)] / [(1-PRE)/(n-PA)]
# Where PA and PC are the number of parameters in the
# models respectively and n is the number of observations.

PA <- 1
PC <- 0
n <- length(dat$Sepal.Length)
F_stat <- (PRE/(PA - PC))/((1 - PRE)/(n - PA))

####################
## Exporting Data ##
####################

library(tidyverse)

# The packages we used last week for importing data also have 
# functions for exporting your data, such as:

?write_csv

# So, if we wanted to export our new version of the iris data,
# we could do the following:

write_csv(x = dat, path = "/Users/Kyle/Desktop/iris.csv")   ##Enter own path
