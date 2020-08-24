##################################
###  Multilevel Linear Models  ###
###         11/15/19           ###
##################################

# Packages needed:
library(tidyverse)
library(ggeffects)
library(ggplot2)
library(car)
library(lsr)
library(compute.es)
library(multcomp)
library(pastecs)
library(jtools)
library(effects)
library(nlme)

# Data used:
cos_surg <- read_tsv("https://raw.githubusercontent.com/RipleyKyleR/class_files/master/Cosmetic%20Surgery.dat")
honeymoon <- read_tsv("https://raw.githubusercontent.com/RipleyKyleR/class_files/master/Honeymoon%20Period.dat")

#####################################
# If ANOVA was the only tool we had #
#####################################

# If all we knew how to do was ANOVA, we may want to see
# if there are differences in post quality of life depending
# on the type of surgery.

surg_aov <- ___(___ ~ ___, data = ___)
summary(___)

# Are there?

###############################
# If we knew a bit about GLMs #
###############################

# We could also attempt to answer this question
# with our old friend `lm()`.

surg_lm <- ___(___ ~ ___, data = ___)
summary(___)

# As expected, we get the same answer.

#################################
# What if we knew about ANCOVA? #
#################################

# We might think that it would be useful to 
# include the pre-test Quality of Life as a 
# covariate for our simpler model.

surg_anc <- ___(___ ~ ___ + ___, data = ___)
summary(___)
Anova(___, ___ = "___")

###############
# Or as a GLM #
###############

surg_anc2 <- ___(___ ~ ___ + ___, data = ___)
summary(___)
Anova(___, ___ = "___")

####################################################
## How can we tell if we need a Multilevel Model? ##
####################################################

#########################
# Start with a baseline #
#########################

# First, you can start with a baseline model that only
# contains the intercept. (Again, this is represented by a 1).

x_baseline_surg <-___(___ ~ ___, 
                     data = ___)
summary(___)

# We can get our correct answer with `lm()`, but we'll run
# into some issues with model comparisons later, so let's
# use a different function.

baseline_surg <- ___(___ ~ ___,
                     data = ___,
                     ___ = "___")

summary(___)

#######################
# Let intercepts vary #
#######################

# Here, we're going to let the intercepts vary
# by clinic (as can be seen in the`random =` term).

m1_surg <-___(___ ~ ___, data = ___,
              ___ = ~___|___, ___ = "___")
summary(___)

#####################
# Model Comparisons #
#####################

___(___, ___)

# Based on these comparisons, we can see that intercepts
# vary significantly based off of the clinics. That's a 
# good indication that we may benefit from a multilevel
# model.

#################
# Fixed Effects #
#################

# We had originally hypothesized that surgery and baseline
# quality of life affect quality of life after surgery. So
# let's go ahead and add those main effects back into our
# model.

m2_surg <-___(___ ~ ___, 
              data = ___, 
              random = ~___|___, ___ = "___")
summary(___)

m3_surg <-___(___ ~ ___ + ___, 
              data = ___, 
              ___ = ~___|___, ___ = "___")
summary(___)

##########################
# More Model Comparisons #
##########################

anova(___, ___, ___)

# So it looks like having our random intercepts and
# the fixed effects for both predictors is our best model
# so far.

# Let's try adding random slopes into the model to see
# if that is also necessary.

#################
# Random Slopes #
#################

# So far our slopes have not been allowed to vary, but 
# we'll change that now.

m4_surg <- ___(___ ~ ___ + ___, 
               data = ___, 
               random = ~ ___|___,
               method = "___") 
summary(___)

# In this code, we've replaced `random = ~1|Clinic` with
# `random = ~Surgery|Clinic`. This means that we want the 
# slope of Surgery to be able to vary across Clinics.
# With this code, we get random intercepts for Clinic and
# random slopes for surgery.

##########################
# More Model Comparisons #
##########################

anova(___, ___)

# Our model with random intercepts and slopes seems to be superior.

###################
# More Predictors #
###################

# We'll now finish adding the rest of our predictors.

m5_surg <- ___(___, .~. + ___) 
summary(___)

m6_surg <- ___(___, .~. + ___:___)
summary(___)


###############################
# Even More Model Comparisons #
###############################

anova(___, ___, ___)

####################
# Further Analyses #
####################

# It may be interesting to see if quality of life
# after surgery is different depeding on the reason
# for the surgery. Fortunately, `lme()` has a subset 
# argument that makes this easy to do.

# But first, we need to specify our subsets of reason.

phys_sub <- ___$___ == 1
cosm_sub <- ___$___ == 0

# Now we can run our model for physical reasons.

phys_m <- ___(___ ~ ___ + ___, 
              data = ___, 
              ___ = ~ ___|___, 
              ___ = ___, ___ = "___")
summary(___)

# And our model for cosmetic reasons.

cosm_m <- ___(___ ~ ___ + ___, 
              data = ___, 
              ___ = ~ ___|___, 
              ___ = ___, ___ = "___")
summary(___)

###################
## Growth Models ##
###################

# Make that data tall!

hm_tall <- ___ %>% 
  ___(___ = ___, ___ = ___, ___:___, ___ = ___)
hm_tall <- ___[___(___$___),]

hm_tall$___ <- ___(___,
                      ___(___ == "___", ___,
                             ___(___ == "___", ___,
                                    ___(___ == "___", ___, 
                                           ___(___ == "___", ___, ___)))))

##################
# Baseline Model #
##################

baseline_hm <- ___(___ ~ ___, 
                   data = ___, ___ = "___", 
                   ___ = ___)
summary(___)

###################
# Vary Intercepts #
###################

m1_hm <- ___(___ ~ ___, 
             data = ___, 
             ___ = ~ ___|___, ___ = "___",
             ___ = ___, 
             ___ = ___(___ = "___"))
summary(___)

# Andy notes in his book that the default optimizer 
# doesn't always succeed in calculating for this data,
# so he chose another one that does work.

############
# Compare! #
############

___(___, ___)

#################
# Fixed Effects #
#################

m2_hm <- ___(___, .~. + ___)
summary(___)

#################
# Random Slopes #
#################

m3_hm <- ___(___, ___ = ~ ___|___)

# this allows the slopes of time_num to vary by person

#########################
# Covariance Structures #
#########################

# You may wish to specify a particular type of covariance
# structure for your model. This is pretty easy to do with `lme()`.

# The three most common options are:

# `corAR1()` : first-order autoregressive covariance structure (equally spaced time points)'
# `corCAR1()` : same but for continous time covariate (not equally spaced time points)
# `corARMA()` : Allows the correlation structure to involve a moving average of error variance
# See the Field book for more information.
# Also,
?corClasses

m4_hm <- ___(___, 
                ___ = ___(___, ___ = ~ ___|___))
summary(___)
intervals(___)

# Remember how your book is called "Data Analysis: A Model Comparison Approach"?

###########################
# More Model Comparisons! #
###########################

___(___, ___, ___, ___, ___)

#################
## Polynomials ##
#################

# You may wish to add polynomial terms to your models
# at some point. They don't make a ton of sense in this
# model, but it's what we've got, so let's roll with it.

#############
# Quadratic #
#############

m5_hm <- ___(___, .~. + ___(___^___))
summary(___)

#########
# Cubic #
#########

m6_hm <-___(___, .~. + ___(___^___))
summary(___)

###############
# Alternative #
###############

m7_hm <- ___(___, .~ ___(___, ___))
summary(___)

###########
# Compare #
###########

___(___, ___, ___, ___)