################
###  ANCOVA  ###
### 10/30/19 ###
################

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

# Data used:
ex10.1 <- read_tsv("https://raw.githubusercontent.com/RipleyKyleR/class_files/master/ex10.1.txt")
ex10.8 <- read_tsv("https://raw.githubusercontent.com/RipleyKyleR/class_files/master/ex10.8.txt")

##############################
## Getting to know the data ##
##############################

# As always, it's a good idea to get a feel for your data before diving straight
# in to analyzing it.

############
# Visually #
############

___(___ = ex10.1, ___ = ___(x = ___(___, ___ = ___(-1, 1), ___ = ___("A", "B")),
                                    y = ___,
                                    color = ___(___, ___ = ___(-1, 1), ___ = ___("A", "B")))) +
  ___(___ = ___(___ = 0.2), ___ = "___", ___ = "___") +
  ___(___ = "___", ___ = ___(___ = 0.2), ___ = "___", ___ = ___) +
  ___(___ = ~ ___(___, ___ = ___(-1, 1), ___ = ___("Old", "New"))) + 
  ___(___ = "Teacher by Curriculum", ___ = "Score") + 
  ___(___ = "___")

___(___ = ex10.1, ___ = ___(x = ___(___, ___ = ___(-1, 1), ___ = ___("Old", "New")),
                                    y = ___,
                                    color = ___(___, ___ = ___(-1, 1), ___ = ___("Old", "New")))) +
  ___(___ = ___(___ = 0.2), ___ = "___", ___ = "___") +
  ___(___ = "___", ___ = ___(___ = 0.2), ___ = "___", ___ = 0.75) +
  ___(___ = ~ ___(___, ___ = ___(-1, 1), ___ = ___("A", "B"))) + 
  ___(___ = "Curriculum by Teacher", ___ = "Score") + 
  ___(___ = "___")

###############
# Numerically #
###############

___ (ex10.1$___, ex10.1$___, ___)
___ (ex10.1$___, ex10.1$___, ___)
___ (ex10.1$___, ex10.1$___, ___)
___ (ex10.1$___, ex10.1$___, ___)

################
## Our Models ##
################

##########################################
# Balanced 2-way ANOVA with NO covariate #
##########################################

m1 <- ___(___ ~ ___ * ___, ___ = ex10.1)
___(m1)
___(m1)

#######################################
# Balanced 2-way ANOVA WITH covariate #
#######################################

m2 <- ___(___ ~  ___ * ___ + ___, ___ = ex10.1)
___(m2)
___(m2)

##PRE
m1m2 <- ___(___, ___)
(___$___[___] - ___$___[___])/___$___[___]
(___-___)/___

#############################################
# Balanced 2-way ANOVA on Difference Scores #
#############################################

ex10.1$diff_score <- ___(ex10.1$___ - ex10.1$___)
m3 <- ___(___ ~ ___ * ___, data = ex10.1)
___(m3)
___(m3)

# While it may seem like we're doing almost the same thing by
# using a difference score instead of a covariate, we can
# see that the difference score analysis was less powerful
# by comparing the error sums of squares of both models.

___(___)$"___ ___"[___(___(___)$"___ ___")]
___(___)$"___ ___"[___(___(___)$"___ ___")]

########################################
## Different Types of Sums of Squares ##
########################################

###########################
# Quick SS Types Overview #
###########################

# The different types of sums of squares all have
# a proper place for their use, but you need to be
# aware of when you should use each type and how 
# they're calculated.

# Type I: 
# Type I considers the predictors sequentially.
# Not particularly useful for unbalanced studies.
# Useful for parsimonious (simple) quadratic models.

# Type III: 
# Every effect (predictor) is adjusted for all other effects.
# Allows a comparison of main effects in the presence of an
# interaction.

# Type II: 
# An effect (F) is adjusted for an effect (G) if, and only if,
# (G) does not contain (F).
# Example:
# In an A + B + A*B model, A would be adjusted for B,
# B would be adjusted for A, and A*B would be adjusted 
# for A and B (because neither contains the term A*B).
# If the model contains only main effects, then type II and III
# SS will be the same.

# Important note:
# Remember that lm() defaults to using dummy coding, and
# we need to use contrast codes for type III SS to be correct
# when an interaction is present.

# Data used:

# For this discussion, we'll use the data provided in 
# chapter 11 of the Field text. Notice that this data is
# not balanced like the previous examples. This data is similar 
# to the data we used in the previous lesson on one-way ANOVA,
# but it has more observations and is unbalanced.

id <-(1:30)
libido <-c(3,2,5,2,2,2,7,2,4,7,5,3,4,4,7,5,4,9,2,6,3,4,4,4,6,4,6,2,8,5)
dose <-c(rep(1, times = 9),rep(2, times = 8), rep(3, times = 13))
dose <-factor(dose, levels = c(1:3), labels = 
                c("Placebo", "Low Dose", "High Dose"))
# We're also going to add in a covariate: partner's libido.
p_libido <- c(4,1,5,1,2,2,7,4,5,5,3,1,2,2,6,4,2,1,3,5,4,3,3,2,0,1,3,0,1,0)
viagra <- data.frame(id, dose, libido, p_libido)

##############################
## Getting to know the data ##
##############################

# As always, it's a good idea to get a feel for your data before diving straight
# in to analyzing it.

############
# Visually #
############

___ %>% 
  ___(___ = ___, ___ = ___, ___, ___, ___ = ___) %>%
  ___(___ = ___(x = ___, y = ___)) + 
  ___() +
  ___(~ ___) +
  ___() +
  ___(___ = "Dose", ___ = "Libido")

###############
# Numerically #
###############

# This will give you descriptive statistics for libido by each level of dose.

___(viagra$___, viagra$___, ___)
___(viagra$___, viagra$___, ___)

# This will give you descriptive statistics for libido as a whole.

___(viagra$___)
___(viagra$___)

######################
# Main Effects Model #
######################

# We'll first run a model with dose as our first predictor, and
# then we'll run a model with dose as our second predictor.

ssm1 <- ___(___ ~ ___ + ___, data = viagra)
ssm2 <- ___(___ ~ ___ + ___, data = viagra)

# Let's compare the type I SS (the default of the anova() function)

___(ssm1)
___(ssm2)

# Notice that order really does matter here, but only because the
# data is unbalanced. Let's take a quick peak back at our balanced
# data.

ssm3 <- ___(___ ~ ___ + ___, data = ex10.1)
ssm4 <- ___(___ ~ ___ + ___, data = ex10.1)

___(ssm3)
___(ssm4)

# And here order doesn't matter.

# Next, we'll take a look at our type II SS for the
# unbalanced data.

___(ssm1, ___ = ___) # `type = 2` is the default
___(ssm2, ___ = ___) # `type = 2` is the default

# Notice that order doesn't matter here, because all
# effects are adjusted for every other effect.

# And finally a look at type III SS.

___(ssm1, ___ = ___)
___(ssm2, ___ = ___)

# Remember, that as long as the model only contains main
# effects, type II and type III will be the same. Let's add
# an interaction term to the model and see what happens..

ssm5 <- ___(___ ~ ___ + ___ + ___:___, data = viagra)
ssm6 <- ___(___ ~ ___ + ___ + ___:___, data = viagra)

# Our type I SS:

___(ssm5)
___(ssm6)

# We can see that order still matters, but the interaction SS
# stay the same, because it's the last term in the model.

# Our type II SS:

___(ssm5, ___ = ___) # `type = 2` is the default
___(ssm6, ___ = ___) # `type = 2` is the default

# Since order doesn't matter, we get the same SS for either model.

# Our type III SS:

___(ssm5, ___ = ___)
___(ssm6, ___ = ___)

# Since order doesn't matter, we get the same SS for either
# model, but the SS are incorrect - don't forget that type III
# SS requires orthogonal contrasts and lm() defaults to dummy coding.
# We can fix that relatively easily by rerunning the models and adding
# contrast codes - we'll do all of this in one line of code.

___(___(___ ~ ___ + ___ + ___:___, data = viagra, ___ = ___(___ = ___(___ = ___))), ___ = ___)
___(___(___ ~ ___ + ___ + ___:___, data = viagra, ___ = ___(___ = ___(___ = ___))), ___ = ___)

# Notice that the order doesn't matter, but now our SS are correct.