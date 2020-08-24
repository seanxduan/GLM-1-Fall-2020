#############################
###  Logistic Regression  ###
###       11/22/19        ###
#############################

# Packages needed:
install.packages("mlogit")
install.packages("aod")
library(tidyverse)
library(car)
library(mlogit)
library(aod)
library(ggeffects)
library(ggplot2)
library(jtools)
library(effects)

# Data used:
eel <- read_tsv("https://raw.githubusercontent.com/RipleyKyleR/class_files/master/eel.dat")
pickup_lines <- read_tsv("https://raw.githubusercontent.com/RipleyKyleR/class_files/master/Chat-Up%20Lines.dat")

# About the data:

## Eel:
# If you're curious, you can find the full description of this data starting at the bottom of
# of page 325 in the Field text.
# Cured - whether the patient was cured or not
# Intervention - whether the patient received the intervention or not
# Duration - number of days before treatment that the patient had the problem

## Chat:
# Data about the success/failure of various pick-up-lines. 348 men and 672 women in sample.
# Success - The degree of success/failure - 3 possibilities
# Funny - how funny the line is from 0 (not at all) to 10 (funniest thing ever)
# Sex - amount of sexual content in the line from 0 (none) to 10 (very sexually direct)
# Good_Mate - whether the line reflected good moral values from 0 (doesn't reflect good moral values)
# to 10 (does reflect good moral values)
# Gender - gender of person receiving the pick-up-line.

###############################
## Basic Logistic Regression ##
###############################

# For this section, we'll be using the `eel` data. However, we need to
# make a few quick changes to it.

# First, we're going to make the `Cured` variable into a factor.

___$___ <- ___(x = ___$___)
___(___$___)
___(___$___)

# We were able to make it into a factor, but it looks like
# we have `cured` as the baseline - let's change that to
# make our comparisons later a bit easier.

eel$___ <- ___(x = eel$___, ___ = "___")
___(eel$___)
___(eel$___)

# That's better.

# Now, let's also factor intervention.

eel$Intervention <- ___(x = eel$Intervention)
___(eel$Intervention)
___(eel$Intervention)

# Same problem.

eel$Intervention <- ___(x = eel$Intervention, ref = "___")
___(eel$Intervention)
___(eel$Intervention)

#########
# glm() #
#########

# So far, we've been using lm() and lme() to run our models. For basic
# logistic regressions, we're going to use the glm() function. It follows
# the same basic layout as lm(), but with a few additional arguments.

# We'll start off with a simple model just using intervention as a
# predictor.

eel_m1 <- ___(___ ~ ___, ___ = ___, ___ = ___)

# In this code, we need to specify `family =` to be binomial because
# logistic regression uses a binomial distribution. If you wanted to
# go back to normal regression, you could specify `family = gaussian`.

___(eel_m1)

# We have a couple important bits of information in this output.
# First, we can see whether our predictor was significant. We
# can also see if our model was better at predicting the outcome
# than a model with a constant (1, as we've used in previous
# baseline models) for the predictor.

# However, to determine if this reduction is significant, we
# need to conduct a chi-square comparison. Which we'll do shortly,
# but first let's run our second model with both predictors.

eel_m2 <- ___(___ ~ ___ + ___, ___ = ___, ___ = ___)
___(eel_m2)

# What do we notice here?
# Let's compare the two models.

___(eel_m1, eel_m2)

# As we can see, the residual deviances of the two models
# are the same - meaning that adding `Duration` did not have
# a significant impact on our model, this would also be confirmed
# by a chi-square test.

# Speaking of chi-squares, because we've determined our first
# model to be superior, let's calculate our p-value for the 
# chi-square comparison of model 1 to the baseline to see if
# it is a significant iprovement.

___(eel_m1)

chi_m1 <- ___$___ - ___$___
chi_m1

chi_df_m1 <- ___$___ - ___$___
chi_df_m1

chi_prob_m1 <- ___ - ___(___ = ___, ___ = ___)
chi_prob_m1

# This gives us the p-value for our chi-square comparison.
# Did adding Intervention significantly improve our model?

# Next, we need to calculate the odds ratios for our model
# coefficients. This can be easily done if you remember that
# the odds ratio is the exponential of the b-values of your
# coefficients.

___(___ = ___(___ = ___))

# It's also a good idea to provide confidence intervals around
# your odds ratios.

___(___ = ___(___ = ___(___ = ___), ___(___ = ___, ___ = ___)))

# To interpret the odds ratios, we need to remember back to how we
# factored the intervention. Since our reference group is "No Treatment,"
# we would say that the odds of a patient being cured if they experience
# treatment is 3.42 times higher than a patient being cured with no
# treatment.

# It's also a good idea to visualize the results of your analysis.
# This can easily be done with `plot()` as shown below.

___(___(___ = ___))

#####################################
## Multinomial Logistic Regression ##
#####################################

# For this section, we'll be using the `pickup_lines` data. However, 
# we need to make a few quick changes to it, like we did with the
# previous data.

# First, we're going to make the `Success` variable into a factor.
# Because we have multiple levels, we'll just specify the order
# we want them in in `factor()` instead of with `relevel()`.
# We can test that we did it properly by checking `class()` and
# `summary()` before and after assigning levels and labels in 
# `factor()`.

pickup_lines$Success <- ___(x = pickup_lines$Success)
___(pickup_lines$Success)
___(pickup_lines$Success)

pickup_lines$Success <- ___(x = pickup_lines$Success, ___ = c("No response/Walk Off", "Get Phone Number", "Go Home with Person"),
                            ___ = c("No response/Walk Off", "Get Phone Number", "Go Home with Person"))
___(pickup_lines$Success)
___(pickup_lines$Success)

# Now, let's also factor `gender`.

pickup_lines$Gender <- ___(x = pickup_lines$Gender)
___(pickup_lines$Gender)
___(pickup_lines$Gender)

# Because we're interested in seeing if female responses differ
# from male responses, it makes more sense from an interpretation
# perspective to have males as the baseline instead of females.

pickup_lines$Gender <- ___(x = pickup_lines$Gender, ___ = "___")
___(pickup_lines$Gender)
___(pickup_lines$Gender)

# We now need to get our dataset in a format that will allow us to
# conduct our multinomial logistic regression.

ml_lines <- ___(___ = ___, ___ = "___", ___ = "___")

# Notice that the resulting data frame has 3 times as many observations.
# That is because each of the original observations now has an observation
# for each potential outcome of `Success`.

# We can now run our analysis!
# For this, we're going to use the `mlogit()` function. It behaves similarly
# `lm()` and `glm()`, except we also need to specify a reference level for
# the model.

lines_m1 <- ___(___ ~ ___ | ___ + ___ + ___ + ___ + ___:___ + ___:___, 
                   ___ = ___, 
                   ___ = "___")
___(lines_m1)

# This output may be a bit difficult to interpret as is, so let's
# calculate some odds ratios.

___(___ = ___(___ = ___(___ = ___), ___(___ = ___, ___ = ___)))

# For interpretation, remember that males are our reference group.

# And it doesn't hurt to make some plots. But first, we should pull
# the predicted probabilities for our observations out of our model.

___$___ <- ___$___$___

___(___ = ___, ___ = ___(___ = ___, ___ = ___, ___ = ___)) +
  ___(___ = "___", ___ = "___", ___ = "___") +
  ___(~ ___) +
  ___(___ = ___, ___ = "___") +
  ___(___ = "Sexuality of Pick-Up-Line", ___ = "Predicted Probability")

___(___ = ___, ___ = ___(___ = ___, ___ = ___, ___ = ___)) +
  ___(___ = "___", ___ = "___", ___ = "___") +
  ___(~ ___) +
  ___(___ = ___, ___ = "___") +
  ___(___ = "Humor of Pick-Up-Line", ___ = "Predicted Probability")

___(___ = ___, ___ = ___(___ = ___, ___ = ___, ___ = ___)) +
  ___(___ = "___", ___ = "___", ___ = "___") +
  ___(~ ___) +
  ___(___ = ___, ___ = "___") +
  ___(___ = "Wholesomeness of Pick-Up-Line", ___ = "Predicted Probability")


