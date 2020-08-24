#####################
### One-Way ANOVA ###
###    10/18/19   ###
#####################

# Packages needed:
install.packages("lsr")
install.packages("compute.es")
install.packages("multcomp")
install.packages("pastecs")
library(tidyverse)
library(ggeffects)
library(ggplot2)
library(car)
library(lsr)
library(compute.es)
library(multcomp)
library(pastecs)

# Data used:

# This data is taken from the Field R book (chapter 10)

id <-(___)
libido <-c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
dose <-___(___(___, ___), ___(___, ___),  ___(___, ___))
dose <-___(___, ___ = ___(___), ___ = 
             ___("Placebo", "Low Dose", "High Dose")) # OR
dose <-___(___, ___, ___ = ___("Placebo", "Low Dose",
                          "High Dose"))
viagra <- ___(___, ___, ___)

##############################
## Getting to know the data ##
##############################

############
# Visually #
############

___(___ = ___, ___ = ___(___ = ___, ___ = ___)) + 
  ___(___ = ___, ___ = "___", ___ = ___, 
         ___(___ = ___), ___ = "___") + 
  ___(___ = ___, ___ = "___",
         ___ = ___, ___ = ___, ___ = "___") + 
  ___(___ = ___, ___ = "___", ___ = ___,
         ___ = "___") + 
  ___(___ = ___, ___ = "___", ___ = ___, 
         ___ = "___") + 
  ___(___ = "Dose of Viagra", ___ = "Mean Libido")

###############
# Numerically #
###############

# This will give you descriptive statistics for libido by each level of dose.

___(___$___, ___$___, ___)

# This will give you descriptive statistics for libido as a whole.

___(___$___)

################
## Our Models ##
################

####################
# The Omnibus Test #
####################

m1r <- ___(___ ~ ___, ___ = ___)
___(m1r)
___(m1r)

m1a <- ___(___ ~ ___, ___ = ___)
___(m1a)
___(m1a)

# `summary()` and `summary.lm()` default to using dummy coding. This may be fine
# for some of your purposes, but other times, you may wish to use a different
# scheme.

###################
# Contrast Coding #
###################

# Let's come up with a couple sets of contrast codes to test specific
# questions about our data.

# First, we want to test if either of the test groups are significantly
# different than the placebo group.

contrast1 <- ___(___)

# Second, we want to test if the test groups are different from each
# other, but not the placebo group.

contrast2 <- ___(___)

# Now we need to apply this to our dose variable.

___(___$___) <- ___(contrast1, contrast2)

# You could also do this in one line with the following:

___(___$___) <- ___(___(___), ___(___))

# So what did that do for us? 

___

#Our data doesn't look any different.

___$___

# Let's give our contrast codes a test.

m2 <- ___(___ ~ ___, ___ = ___)
___(m2)
___(m2)

#################
# Helmert Codes #
#################

# What if we don't have any a priori hypotheses?
# We can use Helmert codes to test potential differences.

___(___$___) <- ___(___ = ___)
___$___

m3 <- ___(___ ~ ___, ___ = ___)
___(m3)
___(m3)
___(m3, ___ = ___)

##############################
## Displaying ANOVA Results ##
##############################

# You could make a bar graph.

___(___ = ___, ___ = ___(___ = ___, ___ = ___)) + 
  ___(___ = ___, ___ = "___", ___ = ___,
         ___(___ = ___), ___ = "___", fill = "___") + 
  ___(___ = ___, ___ = "___",
         ___ = ___, ___ = ___, ___ = "___") + 
  ___(___ = ___, ___ = "___", ___ = ___,
         ___ = "___") + 
  ___(___ = ___, ___ = "___", ___ = ___,
         ___ = "___") + 
  ___(___ = "Dose of Viagra", ___ = "Mean Libido")

# Or you could use the line graph from earlier.
# A word of caution:
# Using a line graph tends to imply some level of continuity
# in your categorical variables even if there isn't.

ggplot(data = viagra, mapping = aes(x = dose, y = libido)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1, 
               aes(group=1), color = "blue") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
               width = 0.2, size = 0.75, color = "slateblue") + 
  stat_summary(fun.y = mean, geom = "point", size = 4,
               color = "slateblue") + 
  stat_summary(fun.y = mean, geom = "point", size = 3, 
               color = "blue") + 
  labs(x = "Dose of Viagra", y = "Mean Libido")