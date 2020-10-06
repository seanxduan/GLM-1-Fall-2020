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
install.packages("olsrr")
library(tidyverse)
library(sjPlot)
library(sjstats)
library(MBESS)
library(apaTables)
library(papaja)
library(pwr)
library(olsrr)

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

m1 <- lm(TIME ~ AGE, data = dat)
summary(m1)
anova_stats(m1)
apa.reg.table(m1, table.number = 1)
apa.aov.table(m1, table.number = 2)

m2 <- lm(TIME ~ MILES, data = dat)
summary(m2)
anova_stats(m2)

m3 <- lm(TIME ~ AGE + MILES, data = dat)
summary(m3)
anova_stats(m3)

plot_model(m3, type = "pred",
    terms = c("MILES", "AGE"))

plot_model(m3, type = "pred",
           terms = c("AGE", "MILES"))

######################
# Interaction Models #
######################

# This is where the syntax for lm() gets new, but it's
# still very straightforward. If we want to add an
# interaction to our model, we can simply add the
# two (or more) predictors multiplied by each other.

m4 <- lm(TIME ~ AGE + MILES + AGE*MILES, data = dat)
summary(m4)
anova_stats(m4)

plot_model(m4, type = "pred",
    terms = c("MILES", "AGE"))

plot_model(m4, type = "pred",
           terms = c("AGE", "MILES"))

####################
# Nonlinear Models #
####################

# You'll sometimes find that for whatever (hopefully theoretical)
# reason, your data will have quadratic, cubic, or possibly even
# higher powers for predictors. Modeling these in lm() is also
# very straightforward. Just remember that if you're going to
# include a higher power, you must include all lower powers as
# well.

m5 <- lm(TIME ~ MILES + I(MILES^2), data = dat)
summary(m5)
anova_stats(m5)

pwr.f2.test(u=2, v=77, f2=.5956/(1-.5956), sig.level = 0.05)

plot_model(m5, type = "pred",
    terms = c("MILES"))

###########
## Power ##
###########

# Here we're going to try something a bit different.

https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html

##############################
## Collinearity Diagnostics ##
##############################

ols_vif_tol(m4)
ols_vif_tol(m3)

ols_vif_tol(m5)


######################
## Tables of Models ##
######################

tab_model(m1)
tab_model(m2)
tab_model(m3)
tab_model(m4)



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
lm(Y~X + Z + W + X:Z + X:W + Z:W + X:Z:W) 
lm(Y~X*Z*W)
lm(Y~ (X+Z+W)^3)
#Yi = β0 + β1Xi + β2Zi + β3Wi + β4XiZi + β5XiWi + β6ZiWi + ei
lm(Y~ (X+Z+W)^2)