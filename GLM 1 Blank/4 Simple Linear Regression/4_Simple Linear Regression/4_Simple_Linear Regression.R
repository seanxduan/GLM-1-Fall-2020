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

mean_wallsupport <- mean(dat$wall_support)
mean_wallsupport

# SSE #

dat$Error_C <- dat$wall_support-mean_wallsupport
dat$SqError_C <- dat$Error_C^2
SSE_C <- sum(dat$SqError_C)
SSE_C

###########
# Model A #
###########

# Predictions #

mean_trumpsupport <- mean(dat$trump_support)
mean_trumpsupport

# Slope #

dat$X_mean <- dat$trump_support-mean_trumpsupport
slope_num <- sum(dat$X_mean*dat$Error_C)
slope_denom <- sum(dat$X_mean^2)
slope <- slope_num/slope_denom
slope

# Intercept #

intercept <- mean_wallsupport-mean_trumpsupport*slope
intercept

# SSE #

dat$Pred_A <- intercept + slope*dat$trump_support
dat$Error_A <- dat$wall_support-dat$Pred_A
dat$SqError_A <- dat$Error_A^2
SSE_A <- sum(dat$SqError_A)
SSE_A

#######
# PRE #
#######

PRE <- 1-(SSE_A/SSE_C)
PRE

#######
#  F  #
#######

F_num <- PRE/(2-1)
F_denom <- (1-PRE)/(729-2)
Fstat <- F_num/F_denom
Fstat

#######
#  t  #
#######

tstat <- sqrt(Fstat)
tstat

################################
#  Linear Regression with lm() #
################################

?lm
ModelA <-lm(wall_support ~ trump_support, data = dat)
summary(ModelA)

############################
#  Mean Centered Predictor #
############################

# Predictions #

dat$meancent_trumpsupport <- dat$trump_support-mean_trumpsupport
mean_mean_cent <-mean(dat$meancent_trumpsupport)
mean_mean_cent

# Slope #

dat$X_mean_cent <- dat$meancent_trumpsupport-mean_trumpsupport
slope_num2 <- sum(dat$X_mean_cent*dat$Error_C)
slope_denom2 <- sum(dat$X_mean^2)
slope2 <- slope_num2/slope_denom2
slope2

# Intercept #

intercept_cent <- mean_wallsupport-slope*mean_mean_cent
intercept_cent

# 95% CI for Slope #

MSE_A <- SSE_A/(729-2)
MSE_A

var_trumpsupport <- var(dat$trump_support)
var_trumpsupport

slope_CI <- sqrt((3.84*MSE_A)/((729-1)*var_trumpsupport))
slope_CI

?confint
confint(object = ModelA, level = 0.95)
