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
devtools::install_github("crsh/papaja")     ## Probs starting w this
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

m1 <- lm(TIME ~ AGE, data = dat)
summary(m1)
anova_stats(m1)                         ## Ignore power    !VS said can use effect size to comp
apa.reg.table(m1, table.number = 1)     ## Can add "filename = "C:\Users\Ronald\Desktop"" to get table
apa.aov.table(m1, table.number = 2)

m2 <- lm(TIME ~ MILES, data = dat)
summary(m2)
anova_stats(m2)

m3 <- lm(TIME ~ AGE + MILES, data = dat)
summary(m3)
anova_stats(m3)

plot_model(m3, type = "pred",
    terms = c("MILES", "AGE"))

plot_model(m3, type = "___",       ## Just swapping
    ___ = ___("___", "___"))

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

m5 <- lm(TIME ~ MILES + I(MILES^2), data = dat)      ## Cant just ^2
summary(m5)
anova_stats(m5)

pwr.f2.test(u = 2, v = 77, f2 = .5956 / (1 - .5956), sig.level = .05)   ## Whatever is omitted, gets calcd




plot_model(m5, type = "pred",
    terms = c("MILES"))

###########
## Power ##
###########

# Here we're going to try something a bit different.

https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html

############################################
## Writing More Complex Model Expressions ##
############################################

# "+" - include this variable (X+Z)
# "-" - delete this variable (X-Z)
# ":" - include the interaction between these variables (X:Z)
# "*" - include these variables and their interactions
# "|" - conditioning: include x given z (X|Z)
# "^" - include these variables and all interactions up to 3-way ((X+Z+W)^3)
# "I" - as is: include a new variable consisting of these multiplied (I(X*Z))
# "1" - intercept: delete the intercept and regress through origin (X-1)

# Yi = ??0 + ??1Xi + ??2Zi + ??3Wi + ??4XiZi + ??5XiWi + ??6ZiWi + ??7XiZiWi + ??i
# This model can be written as any of the following:
lm(Y ~ X + Z + W + X:Z + X:W + Z:W + X:Z:W) 
lm(Y ~ X*Z*W)
lm(Y ~ (X + Z + W)^3)


