##########################################
### Moderated and Nonlinear Regression ###
###             10/4/19                ###
##########################################

# Packages needed:
install.packages("sjPlot")
install.packages("sjstats")
install.packages("MBESS")
install.packages("apaTables")
install.packages("devtools")
devtools::install_github("crsh/papaja")
install.packages("pwr")
library(tidyverse)
library(sjPlot)
library(sjstats)
library(MBESS)
library(apaTables)
library(papaja)
library(pwr)


# Data used:

dat <- read_csv(file = "https://raw.githubusercontent.com/RipleyKyleR/class_files/master/ex7.1.txt")

########################
## Writing Our Models ##
########################

#################
# Simple Models #
#################

# You should all be familiar with the following lm() syntax
# by this point. We're simply creating two simple regressions
# and one multiple regression.

m1 <- ___(___ ~ ___, data = ___)
___(m1)
___(m1)
___(m1, ___ = 1)
___(m1, ___ = 2)

m2 <- ___(___ ~ ___, data = ___)
___(m2)
___(m2)

m3 <- ___(___ ~ ___ + ___, data = ___)
___(m3)
___(m3)

___(___, ___ = "___",
    ___ = ___("___", "___"))

___(___, ___ = "___",
    ___ = ___("___", "___"))

######################
# Interaction Models #
######################

# This is where the syntax for lm() gets new, but it's
# still very straightforward. If we want to add an
# interaction to our model, we can simply add the
# two (or more) predictors multiplied by each other.

m4 <- ___(___ ~ ___ + ___ + ___*___, data = ___)
___(m4)
___(m4)

___(___, ___ = "___",
    ___ = ___("___", "___"))

___(___, ___ = "___",
    ___ = ___("___", "___"))

####################
# Nonlinear Models #
####################

# You'll sometimes find that for whatever (hopefully theoretical)
# reason, your data will have quadratic, cubic, or possibly even
# higher powers for predictors. Modeling these in lm() is also
# very straightforward. Just remember that if you're going to
# include a higher power, you must include all lower powers as
# well.

m5 <- ___(___ ~ ___ + ___(___^___), data = ___)
___(m5)
___(m5)

___(___, ___ = "___",
    ___ = ___("___"))

###########
## Power ##
###########

# Here we're going to try something a bit different.

https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html

############################################
## Writing More Complex Model Expressions ##
############################################

# “+” - include this variable (X+Z)
# “-” - delete this variable (X-Z)
# “:” - include the interaction between these variables (X:Z)
# “*" - include these variables and their interactions
# “|” - conditioning: include x given z (X|Z)
# “ˆ” - include these variables and all interactions up to 3-way ((X+Z+W)ˆ3)
# “I” - as is: include a new variable consisting of these multiplied (I(X*Z))
# “1” - intercept: delete the intercept and regress through origin (X-1)

# Yi = β0 + β1Xi + β2Zi + β3Wi + β4XiZi + β5XiWi + β6ZiWi + β7XiZiWi + εi
# This model can be written as any of the following:
lm(___) 
lm(___)
lm(___)