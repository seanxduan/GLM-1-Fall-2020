#######################
### Factorial ANOVA ###
###     10/19/20    ###
#######################

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

id <-(1:15)
libido <-c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
dose <-c(rep(1, 5), rep(2, 5),  rep(3, 5))
dose <-factor(dose, levels = c(1:3), labels = 
             c("Placebo", "Low Dose", "High Dose")) # OR
dose <-gl(3, 5, labels = c("Placebo", "Low Dose",
                          "High Dose"))
viagra <- data.frame(id, dose, libido)

##############################
## Getting to know the data ##
##############################

############
# Visually #
############

ggplot(data = viagra, mapping = aes(x = dose, y = libido)) + 
  stat_summary(fun.y = mean, geom = "line", size = 1, 
         aes(group = 1), color = "blue") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
         width = 0.2, size = 0.75, color = "slateblue") + 
  stat_summary(fun.y = mean, geom = "point", size = 4,
         color = "slateblue") + 
  stat_summary(fun.y = mean, geom = "point", size = 3, 
         color = "blue") + 
  labs(x = "Dose of Viagra", y = "Mean Libido")

###############
# Numerically #
###############

# This will give you descriptive statistics for libido by each level of dose.

by(viagra$libido, viagra$dose, stat.desc)

# This will give you descriptive statistics for libido as a whole.

stat.desc(viagra$libido)

################
## Our Models ##
################

####################
# The Omnibus Test #
####################

m1r <- lm(libido ~ dose, data = viagra)
summary(m1r)
anova(m1r)

m1a <-aov(libido ~ dose, data = viagra)
summary(m1a)
anova(m1a)
summary.lm(m1a)

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

contrast1 <-c(-2,1,1)

# Second, we want to test if the test groups are different from each
# other, but not the placebo group.

contrast2 <- c(0,-1,1)

# Now we need to apply this to our dose variable.

contrasts(viagra$dose) <-cbind(contrast1, contrast2)

# You could also do this in one line with the following:

contrasts(viagra$dose) <-cbind(c(-2,1,1), c(0,-1,1))

# So what did that do for us? 


#Our data doesn't look any different.

viagra$dose

# Let's give our contrast codes a test.

m2 <- aov(libido ~ dose, data = viagra)
anova(m2)
summary.lm(m2)

#################
# Helmert Codes #
#################

# What if we don't have any a priori hypotheses?
# We can use Helmert codes to test potential differences.

contrasts(viagra$dose) <-contr.Helmert(n = 3)
viagra$dose

m3 <- aov(libido ~ dose, data = viagra)
anova(m3)
summary.lm(m3)
etaSquared(m3, anova = T)

##################
## Effect Sizes ##
##################
library(sjstats)

anova_stats(m3)




##############################
## Displaying ANOVA Results ##
##############################

# You could make a bar graph.

ggplot(data = viagra, mapping = aes(x = dose, y = libido)) + 
  stat_summary(fun.y = mean, geom = "bar", size = 1,
         aes(group = 1), color = "cadetblue4", fill = "darkslategray") + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",
         wdith = .2, size = .75, color = "midnightblue") + 
  stat_summary(fun.y = mean, geom = "point", size = 4,
         color = "midnightblue") + 
  stat_summary(fun.y = mean, geom = "point", size = 3,
               color = "cadetblue4") + 
  labs(x = "Dose of Viagra", y = "Mean Libido")

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

## Extra Bonus Graph Lookin Good Area ##

bar <- ggplot(data=viagra,aes(x=dose,y=libido, fill=dose))
bar + stat_summary(fun = mean, geom = "bar", color="black")+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange") +
  labs(x="Dose of Viagra", y="Libido Level") + theme_apa() +
  scale_fill_manual(values = c("springgreen", "skyblue4", "hotpink")) +
  scale_x_discrete(labels=c("Placebo", "Low Dose", "High Dose")) +
  theme(legend.position = "none")  +
  ggtitle("Effect of Viagra on Libido")
