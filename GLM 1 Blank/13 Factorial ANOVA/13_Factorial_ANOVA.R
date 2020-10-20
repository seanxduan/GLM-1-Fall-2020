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
#http://www.dataanalysisbook.com/datasetsInTextbook.html

ex9.1 <- read.csv("GLM 1 Blank/13 Factorial ANOVA/ex9.1.txt", sep="")
ex9.1$Drug<-as.factor(ex9.1$Drug)
ex9.1$Psychotherapy<-as.factor(ex9.1$Psychotherapy)

################
## Our Models ##
################

####################
# The Omnibus Test #
####################
ex9.1$Lambda1 <- c(rep(5,3),rep(-1,15))
ex9.1$Lambda2 <- c(rep(0,3), rep(4,3), rep(-1,12))
ex9.1$Lambda3 <- c(rep(0,6), rep(3,3), rep(-1,9))
ex9.1$Lambda4 <- c(rep(0,9), rep(2,3), rep(-1,6))
ex9.1$Lambda5 <- c(rep(0,12), rep(1,3), rep(-1,3))
m1<- lm(Mood~Lambda1+Lambda2+Lambda3+Lambda4+Lambda5, data = ex9.1)
summary(m1)
anova(m1)
etaSquared(m1, anova=FALSE)


###################
# Contrast Coding #
###################

# Let's come up with a couple sets of contrast codes to test specific
# questions about our data.

# First, we want to test if either of the test groups are significantly
# different than the placebo group.
levels(ex9.1$Drug)

contrast1 <-c(1,1,-2)

# Second, we want to test if the test groups are different from each
# other, but not the placebo group.

contrast2 <- c(1,-1,0)

# Now we need to apply this to our dose variable.

contrasts(ex9.1$Drug) <-cbind(contrast1, contrast2)

# we can also do this for our Psychotherapy groups
levels(ex9.1$Psychotherapy)
contrast3<-c(-1,1)

contrasts(ex9.1$Psychotherapy)<-cbind(contrast3)
# Let's give our contrast codes a test.

m2 <- aov(Mood ~ Drug + Psychotherapy, data = ex9.1)
anova(m2)
summary.lm(m2)

m3 <- aov(Mood ~ Drug*Psychotherapy, data = ex9.1)
anova(m3)
summary.lm(m3)

#################
# Helmert Codes #
#################

# What if we don't have any a priori hypotheses?
# We can use Helmert codes to test potential differences.

contrasts(ex9.1$Drug) <-contr.Helmert(n = 3)
ex9.1$Drug
contrasts(ex9.1$Psychotherapy) <-contr.Helmert(n = 2)
ex9.1$Psychotherapy

m4 <- aov(Mood ~ Drug + Psychotherapy, data = ex9.1)
anova(m4)
summary.lm(m4)

m5 <- aov(Mood ~ Drug*Psychotherapy, data = ex9.1)
anova(m5)
summary.lm(m5)

##################
## Effect Sizes ##
##################
library(sjstats)

anova_stats(m1)
anova_stats(m2)
anova_stats(m3)
anova_stats(m4)
anova_stats(m5)
