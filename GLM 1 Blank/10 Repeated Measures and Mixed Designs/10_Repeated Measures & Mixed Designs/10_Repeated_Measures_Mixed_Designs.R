#############################################
###  Repeated-Measures and Mixed Designs  ###
###                 11/8/19               ###
#############################################

# Packages needed:
install.packages("effects")
install.packages("nlme")
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
library(nlme)

# Data used:

bushtucker <- read_tsv("https://raw.githubusercontent.com/RipleyKyleR/class_files/master/Bushtucker.dat")
attitude <- read_tsv("https://raw.githubusercontent.com/RipleyKyleR/class_files/master/Attitude.dat")
speeddate <- read_tsv("https://raw.githubusercontent.com/RipleyKyleR/class_files/master/LooksOrPersonality.dat")

#############################
## Repeated-Measures ANOVA ##
#############################

##########################
# Fixing the Data Format #
##########################

# Currently the data is in wide format, but we want it to be
# in tall format for our repeated-measures analyses.

bush_tall <- ___ %>% 
  ___(key = ___, value = ___, ___, ___, ___, ___, ___ = ___)
bush_tall$___ <- ___(bush_tall$___, 
                     ___ = c(fish_eye = "Fish Eye", 
                                      kangaroo_testicle ="Kangaroo Testicle",
                                      stick_insect = "Stick Insect",
                                      witchetty_grub = "Witchetty Grub"))
bush_tall <- ___[___(bush_tall$___),]

############################
# Getting to know the data #
############################

# As always, it's a good idea to get a feel for your data before diving straight
# in to analyzing it.

############
# Visually #
############


___(___ = ___, ___ = ___(x = ___, y = ___)) + 
  ___() + 
  ___() +
  ___(___ = "Type of Animal Eaten", 
         ___ = "Mean Time to Retch (Seconds)")

##############################
# Constructing our Contrasts #
##############################

# Remember: Our Animal order is - Fish Eye, Kangaroo Testicle, Stick Insect, and Witchetty Grub

# C1: We think that eating an insect will have a different time-to-retch than eating a vaguely human-looking body part.

bug_v_human <- ___(___, ___, ___, ___)

# C2: We need a contrast code to separate the first group.

testicle_v_eye <- ___(___, ___, ___, ___)

# C3: We need a contrast code to separate the second group.

stick_v_grub <- ___(___, ___, ___, ___)

# Put 'em all together and apply 'em to the variable.

___(bush_tall$___) <- cbind(bug_v_human, 
                                     testicle_v_eye, 
                                     stick_v_grub)

# Did it work?

bush_tall$___

#####################
# Running our Model #
#####################

# What's wrong with this model? (Hint: it's not that we're using `aov()`)

this_model_is_wrong <- ___(___ ~ ___, ___ = ___)

# This model doesn't take into account that the values in `animal` are 
# gathered from the same people (which makes them dependent).

# Because of this, we need to change how our error term is calculated. We
# can do this in the `aov()` function, like:

m1_bush <- ___(___ ~ ___ + ___(___/___), ___ = ___)
___(m1_bush)

# This new error term accounts for the within-subject error in `participant` for `animal`.

# Running this model in aov() may lead to violations of the assumption of sphericity
# (variances of the differences between all possible pairs of within-subject conditions
# [i.e., levels of the independent variable] are assumed equal). So that we don't have
# to worry about that, we can use `lme()`.

# This new function, `lme()` takes the following general form:

Model <- lme(outcome ~ predictor(s), 
             random = random effects, 
             data = dataset, method = "ML")

#########################
# Our model using lme() #
#########################

m2_bush <- ___(___ ~ ___, 
                  ___ = ~ ___|___/___, 
                  ___ = ___, ___ = "___")

# This is almost identical to the lm() commands that you've become used to,
# except for the "random" term. The code `random = ~1|participant/animal`
# tells the model that the variable `animal` is made up of the same `participant`s
# repeated multiple times across the variable `animal`.

# We can also compare this to a model without the effect of animal.

baseline_bush <- ___(___ ~ ___, 
                        ___ = ~___|___/___, 
                        ___ = ___, ___ = "___")

# This command creates a model that doesn't include the effect of `animal`
# on our predictions of time-until-retch.

# Comparing the models:

___(baseline_bush, m2_bush)

# Can anyone tell me what the AIC and/or BIC values mean?

# Summaries of our preferred model:

___(___)
___(___)

# And to generate our Tukey post-hoc analyses, we can
# do the following.

post_hoc_m2 <- ___(___, ___ = ___(___ = "___"))
___(post_hoc_m2)
___(post_hoc_m2)

#################################
## Factorial Repeated-Measures ##
#################################

##########################
# Fixing the Data Format #
##########################

# Currently the data is in wide format, but we want it to be
# in tall format for our repeated-measures analyses.

att_tall <- ___ %>% 
  ___(___ = ___, ___ = ___, ___:___, ___ = ___) %>% 
  ___(___ = ___(___, ___, ___ = ___("___", "___", "___"))) %>% 
  ___(___ = ___(___, ___, ___, ___ = ___("___", "___", "___")))
att_tall <- ___[___(att_tall$___),]

########################
# Visualizing the Data #
########################

___(___ = ___, ___ = ___(x = ___, y = ___)) +
  ___() + 
  ___(~___, ___ = ___) + 
  ___(___ = "Type of Drink", ___ = "Mean Preference Score")

##############################
# Constructing our Contrasts #
##############################

# Let's set up our contrast codes for the drink variable.
# Remember that our drink order is - Beer, Wine, and Water.

# C1: If we think that alcoholic drinks will differ from water,
# how should we code our first contrast?

alc_v_water <- ___(___, ___, ___)

# C2: How should we code the second to see if there's a 
# difference between alcoholic drinks?

beer_v_wine <- ___(___, ___, ___)

# And apply them to our variable:

___(att_tall$___) <- ___(alc_v_water, beer_v_wine)

att_tall$___

# Don't forget that we also need to set up contrast codes for 
# our imagery variable.
# Remember that our imagery order is - Positive, Negative, and Neutral.

# C1: If we think that negative imagery will differ from all other types,
# how should we code our first contrast?

neg_v_others <- ___(___, ___, ___)

#C2: How should we code the second to see if there's a 
# difference between the remaining types?

pos_v_neut <- ___(___, ___, ___)

# Don't forget to apply them: 

___(att_tall$___) <- ___(neg_v_others, 
                                     pos_v_neut)

att_tall$___

#########################
# Our model using lme() #
#########################

# Let's start off with our baseline model.

baseline_att <- ___(___ ~ ___, 
                       ___ = ~___|___/___/___,
                       ___ = ___, ___ = "___")
___(baseline_att)
___(baseline_att)

# And then move up to our model with drink as a predictor.
# We'll use a new function, `update()` to make running similar
# models a bit easier.

m1_att<- ___(___, .~. + ___)
___(m1_att)
___(m1_att)

# Our model with drink and imagery as main effects.

m2_att <- ___(___, .~. + ___)
___(m2_att)
___(m2_att)

# And finally our full model with the interaction term.

full_att <- ___(___, .~. + ___:___)
___(full_att)
___(full_att)

# And our model comparison ANOVA

___(baseline_att, m1_att, m2_att, full_att)


# Now let's visualize the results of our analyses

___(___ = ___, ___ = ___(x = ___, y = ___, color = ___)) + 
  ___(fun.y = ___, geom = "___") + 
  ___(fun.y = ___, geom = "___", ___(___ = ___)) + 
  ___(fun.data = ___, geom = "___", width = ___) + 
  ___() +
  ___(___ = "Type of Drink", ___ = "Mean Attitude", ___ = "Type of Imagery") 

###################
## Mixed Designs ##
###################

##########################
# Fixing the Data Format #
##########################

speed_tall <- ___ %>% 
  ___(___ = ___, ___ = ___, ___:___, ___ = ___) %>% 
  ___(___ = ___(___, ___, ___ = ___("Charismatic",
                                            "Average", "Dullard"))) %>% 
  ___(___ = ___(___, ___, ___, ___ = ___("Attractive",
                                           "Average", "Ugly")))
speed_tall <- ___[___(speed_tall$___),]

########################
# Visualizing the Data #
########################

___(___ = ___, ___ = ___(x = ___, y = ___, color = ___)) + 
  ___() + 
  ___() +
  ___(___ = "Attractiveness", ___ = "Mean Rating of Date", ___ = "Charisma") + 
  ___(~___)

##############################
# Constructing our Contrasts #
##############################

# We'll again set up our contrasts codes for our models.
# However, since we're using a multi-level model, we don't
# need to worry about having orthogonal contrast codes like
# we previously did. Because of that, let's set up some dummy codes.

att_v_not <- ___(___, ___, ___)
ug_v_not <- ___(___, ___, ___)
contrasts(speed_tall$looks) <- cbind(att_v_not, ug_v_not)

high_v_not <- ___(___, ___, ___)
dull_v_not <- ___(___, ___, ___)
___(speed_tall$___) <- ___(high_v_not, dull_v_not)

#########################
# Our model using lme() #
#########################

# First, our baseline.

baseline_date <- ___(___ ~ ___, 
                        ___ = ~ ___|___/___/___,
                        ___ = ___, ___ = "___")
___(baseline_date)
___(baseline_date)

# Then we'll add looks.

m1_date <- ___(___, .~. + ___)
___(m1_date)
___(m1_date)

# Then we'll add personality.

m2_date <- ___(___, .~. + ___)
___(m2_date)
___(m2_date)

# Then we'll add in gender.

m3_date <- ___(___, .~. + ___)
___(m3_date)
___(m3_date)

# Add the looks and gender interaction.

m4_date <- ___(___, .~. + ___:___)
___(m4_date)
___(m4_date)

# Add the personality and gender interaction.

m5_date <- ___(___, .~. + ___:___)
___(m5_date)
___(m5_date)

# Add the looks and personality interaction

m6_date <- ___(___, .~. + ___:___)
___(m6_date)
___(m6_date)

# And add the final 3-way interaction.

full_date <- ___(___, .~. + ___:___:___)
___(full_date)
___(full_date)

# And a good ol' model comparison.

___(baseline_date, m1_date, m2_date,
      m3_date, m4_date, m5_date,
      m6_date, full_date)

#################################
# Visualizations of the Results #
#################################

# I won't be having you type all of the code, because there's an obscene amount of it;
# however, you will have it available to you for future reference.

## Main Effect of Gender

ggplot(data = speed_tall, mapping = aes(x = gender, y = date_rating)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "White", color = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + 
  theme_apa() + 
  labs(x = "Gender", y = "Mean Rating of Date") 

## Main Effect of Looks

ggplot(data = speed_tall, mapping = aes(x = looks, y = date_rating)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "White", color = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + 
  theme_apa() + 
  labs(x = "Attractiveness", y = "Mean Rating of Date") 


## Main Effect of Personality

ggplot(data = speed_tall, mapping = aes(x = personality, y = date_rating)) + 
  stat_summary(fun.y = mean, geom = "bar", fill = "White", color = "Black") + 
  stat_summary(fun.data = mean_cl_boot, geom = "pointrange") + 
  theme_apa() + 
  labs(x = "Personality", y = "Mean Rating of Date") 

## Interaction between Gender and Looks

ggplot(data = speed_tall, mapping = aes(x = looks, y = date_rating, color = gender)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group= gender)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_apa() + 
  labs(x = "Attractiveness", y = "Mean Rating of Date", color = "Gender") + 
  scale_y_continuous(limits = c(0,100)) 

## Interaction between Gender and Personality

ggplot(data = speed_tall, mapping = aes(x = personality, y = date_rating, color = gender)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group= gender)) +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_apa() + 
  labs(x = "Personality", y = "Mean Rating of Date", color = "Gender") + 
  scale_y_continuous(limits = c(0,100)) 

## Interaction between Looks and Personality

ggplot(data = speed_tall, mapping = aes(x = looks, y = date_rating, color = personality)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group= personality)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  theme_apa() +
  labs(x = "Attractiveness", y = "Mean Rating of Date", color = "Personality") + 
  scale_y_continuous(limits = c(0,100)) 

## Threeway Interaction

ggplot(data = speed_tall, mapping = aes(x = looks, y = date_rating, color = personality)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group= personality)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  theme_apa() + 
  labs(x = "Attractiveness", y = "Mean Rating of Date", color = "Personality") + 
  scale_y_continuous(limits = c(0,100)) + 
  facet_wrap(~gender)