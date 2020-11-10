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

bush_tall <- bushtucker %>% 
  gather(key = animal, value = retch, stick_insect, kangaroo_testicle, fish_eye, witchetty_grub, na.rm = T)
bush_tall$animal <- factor(bush_tall$animal, 
                     labels = c(fish_eye = "Fish Eye", 
                                      kangaroo_testicle ="Kangaroo Testicle",
                                      stick_insect = "Stick Insect",
                                      witchetty_grub = "Witchetty Grub"))
bush_tall <- bush_tall[order(bush_tall$participant),]

############################
# Getting to know the data #
############################

# As always, it's a good idea to get a feel for your data before diving straight
# in to analyzing it.

############
# Visually #
############


ggplot(data = bush_tall, mapping = aes(x = animal, y = retch)) + 
  geom_boxplot() + 
  theme_apa() +
  labs(x = "Type of Animal Eaten", 
         yy = "Mean Time to Retch (Seconds)")

##############################
# Constructing our Contrasts #
##############################

# Remember: Our Animal order is - Fish Eye, Kangaroo Testicle, Stick Insect, and Witchetty Grub

# C1: We think that eating an insect will have a different time-to-retch than eating a vaguely human-looking body part.

bug_v_human <- c(-1, -1, 1, 1)

# C2: We need a contrast code to separate the first group.

testicle_v_eye <- c(-1, 1, 0, 0)

# C3: We need a contrast code to separate the second group.

stick_v_grub <- c(0, 0, 1, -1)

# Put 'em all together and apply 'em to the variable.

contrasts(bush_tall$animal) <- cbind(bug_v_human, 
                                     testicle_v_eye, 
                                     stick_v_grub)

# Did it work?

bush_tall$animal

#####################
# Running our Model #
#####################

# What's wrong with this model? (Hint: it's not that we're using `aov()`)

this_model_is_wrong <- aov(retch ~ animal, data = bush_tall)

# This model doesn't take into account that the values in `animal` are 
# gathered from the same people (which makes them dependent).

# Because of this, we need to change how our error term is calculated. We
# can do this in the `aov()` function, like:

m1_bush <- aov(retch ~ animal + Error(participant/animal), data = bush_tall)
summary(m1_bush)

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

m2_bush <- lme(retch ~ animal, 
                  random = ~ 1|participant/animal, 
                  data = bush_tall, method = "ML")

# This is almost identical to the lm() commands that you've become used to,
# except for the "random" term. The code `random = ~1|participant/animal`
# tells the model that the variable `animal` is made up of the same `participant`s
# repeated multiple times across the variable `animal`.

# We can also compare this to a model without the effect of animal.

baseline_bush <- lme(retch ~ 1, 
                        random = ~1|participant/animal, 
                        data = bush_tall, method = "ML")

# This command creates a model that doesn't include the effect of `animal`
# on our predictions of time-until-retch.

# Comparing the models:

anova(baseline_bush, m2_bush)

# Can anyone tell me what the AIC and/or BIC values mean?

# Summaries of our preferred model:

summary(m2_bush)
anova(m2_bush)

# And to generate our Tukey post-hoc analyses, we can
# do the following.

post_hoc_m2 <- glht(m2_bush, linfct = mcp(animal = "Tukey"))
summary(post_hoc_m2)
confint(post_hoc_m2)

#################################
## Factorial Repeated-Measures ##
#################################

##########################
# Fixing the Data Format #
##########################

# Currently the data is in wide format, but we want it to be
# in tall format for our repeated-measures analyses.

att_tall <- attitude %>% 
  gather(key = groups, value = attitude, beerpos:waterneu, na.rm = T) %>% 
  mutate(drink = gl(3, 60, labels = c("Beer", "Wine", "Water"))) %>% 
  mutate(imagery = gl(3, 20, 180, labels = c("Pos", "Neg", "Neu")))
att_tall <- att_tall[order(att_tall$participant),]

########################
# Visualizing the Data #
########################

ggplot(data = att_tall, mapping = aes(x = drink, y = attitude)) +
  geom_boxplot() + 
  facet_wrap(~imagery, nrow = 1) + 
  labs(x = "Type of Drink", y = "Mean Preference Score")

##############################
# Constructing our Contrasts #
##############################

# Let's set up our contrast codes for the drink variable.
# Remember that our drink order is - Beer, Wine, and Water.

# C1: If we think that alcoholic drinks will differ from water,
# how should we code our first contrast?

alc_v_water <- c(1, 1, -2)

# C2: How should we code the second to see if there's a 
# difference between alcoholic drinks?

beer_v_wine <- c(1, -1, 0)

# And apply them to our variable:

contrasts(att_tall$drink) <- cbind(alc_v_water, beer_v_wine)

att_tall$drink

# Don't forget that we also need to set up contrast codes for 
# our imagery variable.
# Remember that our imagery order is - Positive, Negative, and Neutral.

# C1: If we think that negative imagery will differ from all other types,
# how should we code our first contrast?

neg_v_others <- c(1, -2, 1)

#C2: How should we code the second to see if there's a 
# difference between the remaining types?

pos_v_neut <- c(1, 0, -1)

# Don't forget to apply them: 

contrasts(att_tall$imagery) <- cbind(neg_v_others, 
                                     pos_v_neut)

att_tall$imagery

#########################
# Our model using lme() #
#########################

# Let's start off with our baseline model.

baseline_att <- lme(attitude ~ 1, 
                       random = ~1|participant/drink/imagery,
                       data = att_tall, method = "ML")
summary(baseline_att)
anova(baseline_att)

# And then move up to our model with drink as a predictor.
# We'll use a new function, `update()` to make running similar
# models a bit easier.

m1_att<- update(baseline_att, .~. + drink)
summary(m1_att)
anova(m1_att)

# Our model with drink and imagery as main effects.

m2_att <- update(m1_att, .~. + imagery)
summary(m2_att)
anova(m2_att)

# And finally our full model with the interaction term.

full_att <- update(m2_att, .~. + drink:imagery)
summary(full_att)
anova(full_att)

# And our model comparison ANOVA

anova(baseline_att, m1_att, m2_att, full_att)


# Now let's visualize the results of our analyses

ggplot(data = att_tall, mapping = aes(x = drink, y = attitude, color = imagery)) + 
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line", aes(group = imagery)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  theme_apa() +
  labs(x = "Type of Drink", y = "Mean Attitude", color = "Type of Imagery") 

###################
## Mixed Designs ##
###################

##########################
# Fixing the Data Format #
##########################

speed_tall <- speeddate %>% 
  gather(key = groups, value = date_rating, att_high:ug_none, na.rm = T) %>% 
  mutate(personality = gl(3, 60, labels = c("Charismatic",
                                            "Average", "Dullard"))) %>% 
  mutate(looks = gl(3, 20, 180, labels = c("Attractive",
                                           "Average", "Ugly")))
speed_tall <- speed_tall[order(speed_tall$participant),]

########################
# Visualizing the Data #
########################

ggplot(data = speed_tall, mapping = aes(x = looks, y = date_rating, color = personality)) + 
  geom_boxplot() + 
  theme_apa() +
  labs(x = "Attractiveness", y = "Mean Rating of Date", z = "Charisma") + 
  facet_wrap(~gender)

##############################
# Constructing our Contrasts #
##############################

# We'll again set up our contrasts codes for our models.
# However, since we're using a multi-level model, we don't
# need to worry about having orthogonal contrast codes like
# we previously did. Because of that, let's set up some dummy codes.

att_v_not <- c(1, 0, 0)
ug_v_not <- c(0, 0, 1)
contrasts(speed_tall$looks) <- cbind(att_v_not, ug_v_not)

high_v_not <- c(1, 0, 0)
dull_v_not <- c(0, 0, 1)
contrasts(speed_tall$personality) <- cbind(high_v_not, dull_v_not)

#########################
# Our model using lme() #
#########################

# First, our baseline.

baseline_date <- lme(date_rating ~ 1, 
                        random = ~ 1|participant/personality/looks,
                        data = speed_tall, method = "ML")
summary(baseline_date)
anova(baseline_date)

# Then we'll add looks.

m1_date <- update(baseline_date, .~. + looks)
summary(m1_date)
anova(m1_date)

# Then we'll add personality.

m2_date <- update(m1_date, .~. + personality)
summary(m2_date)
anova(m2_date)

# Then we'll add in gender.

m3_date <- update(m2_date, .~. + gender)
summary(m3_date)
anova(m3_date)

# Add the looks and gender interaction.

m4_date <- update(m3_date, .~. + looks:gender)
summary(m4_date)
anova(m4_date)

# Add the personality and gender interaction.

m5_date <- update(m4_date, .~. + personality:gender)
summary(m5_date)
anova(m5_date)

# Add the looks and personality interaction

m6_date <-update(m5_date, .~. + looks:personality)
summary(m6_date)
anova(m6_date)

# And add the final 3-way interaction.

full_date <- update(m6_date, .~. + looks:personality:gender)
summary(full_date)
anova(full_date)

# And a good ol' model comparison.

anova(baseline_date, m1_date, m2_date,
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
