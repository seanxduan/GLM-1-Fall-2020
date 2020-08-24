##########################
## Review and Extension ##
##      Kye Ripley      ##
##       8/30/19        ##
##########################

# Packages needed:
install.packages("DescTools")
library(DescTools)

# Data used:

############################
## Descriptive Statistics ##
############################

# In our last session, we talked about ways to calculate
# some of the most commonly used desriptive statistics:
# mean, median, etc. Today, we'll expand on that a bit
# and look at some simpler ways to calculate these.

########
# Data #
########

# For today, we'll use the iris dataset that is available
# inside of R. We'll just be focusing on the Sepal.Length
# column.

dat <- iris
___(dat)

########
# Mean #
########

mean_sl <- ___(___$___)

##########
# Median #
##########

median_sl <- ___(___$___)

########
# Mode #
########

mode_sl <- ___(___$___)

######################
# Standard Deviation #
######################

sd_sl <- ___(___$___)

############
# Variance #
############

var_sl <- ___(___$___)

###############
# Mean 95% CI #
###############

mean_ci_sl <-___(___ = ___(___, ___), ___ = ___, ___ = ___)

######################
## Model Comparison ##
######################

# We're going to test the hypothesis that a single parameter model using 
# the sample mean is superior to a model using the mode as a constant.

###########
# Model C #
###########

# First, we need to subtract our constant from the observed values.

dat$error_C <- ___$___ - ___

# We then need to square the error.

dat$sq_error_C <- ___$___^___

# We then sum the errors to get our sum of squared errors

SSE_C <- ___(___$___)
SSE_C

###########
# Model A #
###########

# We'll then follow the same process, except using our new parameter:
# the mean.

dat$error_A <- ___$___ - ___

# We then need to square the error.

dat$sq_error_A <- ___$___^___

# We then sum the errors to get our sum of squared errors

SSE_A <- ___(___$___)
SSE_A

#######
# PRE #
#######

# Finally, we need to calculate our PRE.

PRE <- ___ - (___/___)
PRE

##############
## Graphing ##
##############

############
# Box Plot #
############

boxplot(x = ___$___, data = ___,
        main = "Iris Data",
        ylab = "Sepal Length")

#############
# Histogram #
#############

hist(x = ___$___, 
     main = "Iris Data", 
     xlab = "Sepal Length",
     ylab = "Frequency")

################
# Scatter Plot #
################

___(x = ___$___, y = ___$___,
     main = "Iris Data", 
     xlab = "Sepal Length",
     ylab = "Sepal Width")

___(x = ___$___, y = ___$___,
               main = "Iris Data", 
               xlab = "Sepal Length",
               ylab = "Sepal Width")
