##############################
## Simple Linear Regression ##
##        Kyle Ripley       ##
##          9/13/19         ##
##############################

# Packages used:
library(tidyverse)
library(readr) 
library(httr)

# Data used:
dat <- read_csv("https://raw.githubusercontent.com/RipleyKyleR/class_files/master/arndt%20lab%20dataset.csv")


###########
# Model C #
###########

# Predictions #

mean_wallsupport <- ___(___$___)
mean_wallsupport

# SSE #

dat$Error_C <- ___$___-___
dat$SqError_C <- ___$___^___
SSE_C <- ___(___$___)
SSE_C

###########
# Model A #
###########

# Predictions #

mean_trumpsupport <- ___(___$___)
mean_trumpsupport

# Slope #

dat$X_mean <- ___$___-___
slope_num <- ___(___$___*___$___)
slope_denom <- ___(___$___^___)
slope <- ___/___
slope

# Intercept #

intercept <- ___-___*___
intercept

# SSE #

dat$Pred_A <- ___ + ___*___$___
dat$Error_A <- ___$___-___$___
dat$SqError_A <- ___$___^___
SSE_A <- ___(___$___)
SSE_A

#######
# PRE #
#######

PRE <- ___-(___/___)
PRE

#######
#  F  #
#######

F_num <- ___/(___-___)
F_denom <- (___-___)/(___-___)
Fstat <- ___/___
Fstat

#######
#  t  #
#######

tstat <- ___(___)
tstat

################################
#  Linear Regression with lm() #
################################

?lm
ModelA <- ___(___ ~ ___, ___ = ___)
summary(___)

############################
#  Mean Centered Predictor #
############################

# Predictions #

dat$meancent_trumpsupport <- ___$___-___
mean_mean_cent <- ___(___$___)
mean_mean_cent

# Slope #

dat$X_mean_cent <- ___$___-___
slope_num2 <- ___(___$___*___$___)
slope_denom2 <- ___(___$___^___)
slope2 <- ___/___
slope2

# Intercept #

intercept_cent <- ___-___*___
intercept_cent

# 95% CI for Slope #

MSE_A <- ___/(___-___)
MSE_A

var_trumpsupport <- ___(___$___)
var_trumpsupport

slope_CI <- ___((3.84*___)/((___-___)*___))
slope_CI

?confint
___(___ = ___, ___ = ___)