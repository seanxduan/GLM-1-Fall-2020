#########################################
## Multiple Regression and Diagnostics ##
##              Kyle Ripley            ##
##                 9/6/19              ##
#########################################

# Packages needed:
install.packages("car")
install.packages("GGally")
library(car)
library(GGally)
library(olsrr)
library(tidyverse)
library(ggplot2)
library(haven)

# Data used:
dat <- read_sav("https://github.com/RipleyKyleR/public_data_files/blob/master/dataset%20for%20victoria.sav?raw=true") 
dat <- dat %>% 
  ___(___(dat)) %>% 
  ___(id, scorumin, pos, anxious, depcesd)

# Data Key:

# scorumin: Co-rumination with specific friend (sr)
# pos: Positive friendship quality
# anxious: RCMAS anxiety
# depcesd: CESD depression

##############################
# Initial Data Visualization #
##############################

# As always, it's a good idea to get a feel for the structure
# of your data before ever conducting any analyses. An easy way
# to do this is through data visualization.

# We can make a correlation matrix to examine the correlations
# between all of our variables.

___(___ = ___, ___ = ___, ___ = "___", ___ = "___", ___ = ___,
    ___ = ___, ___ = "___")

# We could also make multiple scatter plots to examine the 
# different relationships in the data that we're interested in.
# We'll explore some more of ggplot2's options here.

___(data = ___, mapping = aes(x = ___, y = ___, ___ = ___)) +
  ___(___ = ___, ___ = ___, ___ = ___) +
  ___() +
  ___(___ = "#0091ff", ___ = "#f0650e") +
  ___(___ = "___", ___ = "springgreen3")

___(data = dat, mapping = aes(x = ___, y = ___, ___ = ___)) +
  ___(shape = ___, size = ___, show.legend = ___) +
  ___() +
  ___(low = "darkslategray1", high = "darkviolet") +
  ___(method = "lm", color = "deeppink")

___(data = dat, mapping = aes(x = ___, y = ___, ___ = ___)) +
  ___(shape = ___, size = ___, show.legend = ___) +
  ___() +
  ___(low = "springgreen", high = "midnightblue") +
  ___(method = "lm", color = "purple")

___(data = dat, mapping = aes(x = ___, y = ___, color = ___)) +
  ___(shape = ___, size = ___, show.legend = ___) +
  ___() +
  ___(low = "paleturquoise1", high = "palevioletred4") +
  ___(method = "lm", color = "black")

##############
# The Models #
##############

## Running the models ##

# We'll first start out with our simplest model:
# co-rumination predicted by positive friendship quality

___ <- ___(___ ~ ___, ___ = ___)
___(___)

# Next, we'll add anxiety as a predictor. Notice that we 
# simply add it into the lm() model.

___ <- ___(___ ~ ___ + ___, ___ = ___)
___(___)

# Finally, we'll add depression as our last predictor.

___ <- ___(___ ~ ___ + ___ + ___, ___ = ___)
___(___)

## Comparting the models ##

# We'll start by comparing our simplest model to our
# 2 predictor model.

___(___,___)

# We can then get the ANOVA tables for each of the
# individual models.

___(___)
___(___)

# We can then compare our 2-predictor model with the 
# full model.

___(___,___)

###############
# Diagnostics #
###############

# Because `ModelA` was our preferred model,
# we'll move ahead with that one.

# These functions should mostly be familiar
# from last week's lesson.

## Leverage ##
lev <- ___(___(___))
___(lev)

___(___)

## Studentized Deleted Residuals ##

___(___)

## Cook's Distance ##

___(___)

## Normality Assumption ##

___(ModelA$___)
___(ModelA$___)
___(ModelA$___)

## Homogeneity of Variance ##

___(___)

## Redundancy, tolerance ##

# We'll now take a look at some new functions
# that deal with your Variance Inflation Factors.

___(___)
___(___)

___(___)
___(___)

## Model without Obs 338 ##

dat2 <- ___[-___, ]
ModelA_clean <- ___(___ ~ ___ + ___, data = ___)
___(ModelA_clean)

# For a reminder...

summary(ModelA)


