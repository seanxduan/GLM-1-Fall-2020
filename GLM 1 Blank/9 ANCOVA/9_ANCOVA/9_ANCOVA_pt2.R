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
library(effects)

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

ggplot(data = ex10.1, mapping = aes(x = factor(teacher, levels = c(-1, 1), labels = c("A", "B")),
                                    y = score,
                                    color = factor(teacher, levels = c(-1, 1), labels = c("A", "B")))) +
  geom_point(position = position_dodge(width = 0.2), stat = "summary", fun.y = "mean") +
  geom_errorbar(stat = "summary", position = position_dodge(width = 0.2),
                fun.data = "mean_cl_normal", width = 0.75) +
  facet_wrap(facets = ~ factor(curriculum, levels = c(-1, 1), labels = c("Old", "New"))) + 
  labs(x = "Teacher by Curriculum", y = "Score") + 
  theme_apa(legend.pos = "none")

ggplot(data = ex10.1, mapping = aes(x = factor(curriculum, levels = c(-1, 1), labels = c("Old", "New")),
                                    y = score,
                                    color = factor(curriculum, levels = c(-1, 1), labels = c("Old", "New")))) +
  geom_point(position = position_dodge(width = 0.2), stat = "summary", fun.y = "mean") +
  geom_errorbar(stat = "summary", position = position_dodge(width = 0.2),
                fun.data = "mean_cl_normal", width = 0.75) +
  facet_wrap(facets = ~ factor(teacher, levels = c(-1, 1), labels = c("A", "B"))) + 
  labs(x = "Curriculum by Teacher", y = "Score") + 
  theme_apa(legend.pos = "none")

###############
# Numerically #
###############

by (ex10.1$pretest, ex10.1$teacher, stat.desc)
by (ex10.1$pretest, ex10.1$curriculum, stat.desc)
by (ex10.1$score, ex10.1$teacher, stat.desc)
by (ex10.1$score, ex10.1$curriculum, stat.desc)

################
## Our Models ##
################

##########################################
# Balanced 2-way ANOVA with NO covariate #
##########################################

m1 <- lm(score ~ curriculum * teacher, data = ex10.1)
summary(m1)
anova(m1)

#######################################
# Balanced 2-way ANOVA WITH covariate #
#######################################

m2 <- lm(score ~  curriculum * teacher + pretest, data = ex10.1)
summary(m2)
anova(m2)

##PRE
m1m2 <- anova(m1, m2)
(m1m2$RSS[1] - m1m2$RSS[2])/m1m2$RSS[1]
(710-348.07)/710

#############################################
# Balanced 2-way ANOVA on Difference Scores #
#############################################

ex10.1$diff_score <- c(ex10.1$score - ex10.1$pretest)
m3 <- lm(diff_score ~ curriculum * teacher, data = ex10.1)
summary(m3)
anova(m3)

# While it may seem like we're doing almost the same thing by
# using a difference score instead of a covariate, we can
# see that the difference score analysis was less powerful
# by comparing the error sums of squares of both models.
test <- anova(m2)
anova(m2)$"Sum Sq"[length(anova(m2)$"Sum Sq")]
anova(m3)$"Sum Sq"[length(anova(m3)$"Sum Sq")]

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

viagra %>% 
  gather(key = libido_type, value = libido, libido, p_libido, na.rm = TRUE) %>%
  ggplot(mapping = aes(x = dose, y = libido)) + 
  geom_boxplot() +
  facet_wrap(~ libido_type) +
  theme_apa() +
  labs(x = "Dose", y = "Libido")

###############
# Numerically #
###############

# This will give you descriptive statistics for libido by each level of dose.

by(viagra$libido, viagra$dose, stat.desc)
by(viagra$p_libido, viagra$dose, stat.desc)

# This will give you descriptive statistics for libido as a whole.

stat.desc(viagra$libido)
stat.desc(viagra$p_libido)

######################
# Main Effects Model #
######################

# We'll first run a model with dose as our first predictor, and
# then we'll run a model with dose as our second predictor.

ssm1 <- lm(libido ~ dose + p_libido, data = viagra)
ssm2 <- lm(libido ~ p_libido + dose, data = viagra)

# Let's compare the type I SS (the default of the anova() function)

anova(ssm1)
anova(ssm2)

# Notice that order really does matter here, but only because the
# data is unbalanced. Let's take a quick peak back at our balanced
# data.

ssm3 <- lm(score ~ curriculum + teacher, data = ex10.1)
ssm4 <- lm(score ~ teacher + curriculum, data = ex10.1)

anova(ssm3)
anova(ssm4)

# And here order doesn't matter.

# Next, we'll take a look at our type II SS for the
# unbalanced data.

Anova(ssm1, type = 2) # `type = 2` is the default
Anova(ssm2, type = 2) # `type = 2` is the default

# Notice that order doesn't matter here, because all
# effects are adjusted for every other effect.

# And finally a look at type III SS.

Anova(ssm1, type = 3)
Anova(ssm2, type = 3)

# Remember, that as long as the model only contains main
# effects, type II and type III will be the same. Let's add
# an interaction term to the model and see what happens..

ssm5 <- lm(libido ~ dose + p_libido + dose:p_libido, data = viagra)
ssm6 <- lm(libido ~ p_libido + dose + dose:p_libido, data = viagra)

# Our type I SS:

anova(ssm5)
anova(ssm6)

# We can see that order still matters, but the interaction SS
# stay the same, because it's the last term in the model.

# Our type II SS:

Anova(ssm5, type = 2) # `type = 2` is the default
Anova(ssm6, type = 2) # `type = 2` is the default

# Since order doesn't matter, we get the same SS for either model.

# Our type III SS:

Anova(ssm5, type = 3)
Anova(ssm6, type = 3)

# Since order doesn't matter, we get the same SS for either
# model, but the SS are incorrect - don't forget that type III
# SS requires orthogonal contrasts and lm() defaults to dummy coding.
# We can fix that relatively easily by rerunning the models and adding
# contrast codes - we'll do all of this in one line of code.

Anova(lm(libido ~ dose + p_libido + dose:p_libido, data = viagra, contrasts = list(dose = contr.Helmert(n = 3))), type = 3)
Anova(lm(libido ~ p_libido + dose + dose:p_libido, data = viagra, contrasts = list(dose = contr.Helmert(n = 3))), type = 3)

# Notice that the order doesn't matter, but now our SS are correct.

#######################
# Adjustd Group Means #
#######################

# This will give you descriptive statistics for libido by each level of dose.

by(viagra$libido, viagra$dose, stat.desc)
by(viagra$p_libido, viagra$dose, stat.desc)

# This will give you descriptive statistics for libido as a whole.

stat.desc(viagra$libido)
stat.desc(viagra$p_libido)

# But what about group means when we have multiple predictors to account for?
effect("p_libido", ssm2)
effect("dose", ssm2)
