##################################
###  Multilevel Linear Models  ###
###         11/15/19           ###
##################################

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
library(nlme)

# Data used:
cos_surg <- read_tsv("https://raw.githubusercontent.com/RipleyKyleR/class_files/master/Cosmetic%20Surgery.dat")
honeymoon <- read_tsv("https://raw.githubusercontent.com/RipleyKyleR/class_files/master/Honeymoon%20Period.dat")

#####################################
# If ANOVA was the only tool we had #
#####################################

# If all we knew how to do was ANOVA, we may want to see
# if there are differences in post quality of life depending
# on the type of surgery.

surg_aov <- aov(Post_QoL ~ Surgery, data = cos_surg) # Not acc for clinic
summary(surg_aov)

# Are there?

###############################
# If we knew a bit about GLMs #
###############################

# We could also attempt to answer this question
# with our old friend `lm()`.

surg_lm <- lm(Post_QoL ~ Surgery, data = cos_surg) # Same
summary(surg_lm)

# As expected, we get the same answer.

#################################
# What if we knew about ANCOVA? #
#################################

# We might think that it would be useful to 
# include the pre-test Quality of Life as a 
# covariate for our simpler model.

surg_anc <- aov(Post_QoL ~ Surgery + Base_QoL, data = cos_surg)
summary(surg_anc)
Anova(surg_anc, type = "III") # Type is no matter since no interaction btw cov and pred

###############
# Or as a GLM #
###############

surg_anc2 <- lm(Post_QoL ~ Surgery + Base_QoL, data = cos_surg)
summary(surg_anc2)
Anova(surg_anc2, type = "III")

####################################################
## How can we tell if we need a Multilevel Model? ##
####################################################

#########################
# Start with a baseline #
#########################

# First, you can start with a baseline model that only
# contains the intercept. (Again, this is represented by a 1).

x_baseline_surg <-lm(Post_QoL ~ 1, 
                     data = cos_surg)   # Using lm will cause apple orange prob
summary(x_baseline_surg)

# We can get our correct answer with `lm()`, but we'll run
# into some issues with model comparisons later, so let's
# use a different function.

baseline_surg <- gls(Post_QoL ~ 1,      # Just for base (?)
                     data = cos_surg,
                     method = "ML")     # Since ML begins to diverge from OLS
summary(baseline_surg)

#######################
# Let intercepts vary #
#######################

# Here, we're going to let the intercepts vary
# by clinic (as can be seen in the`random =` term).

m1_surg <-lme(Post_QoL ~ 1, data = cos_surg,
              random = ~1|Clinic, method = "ML")
summary(m1_surg)

#####################
# Model Comparisons #
#####################

anova(baseline_surg, m1_surg)  # Results show intercepts should vary

# Based on these comparisons, we can see that intercepts
# vary significantly based off of the clinics. That's a 
# good indication that we may benefit from a multilevel
# model.

#################
# Fixed Effects #
#################

# We had originally hypothesized that surgery and baseline
# quality of life affect quality of life after surgery. So
# let's go ahead and add those main effects back into our
# model.

m2_surg <-lme(Post_QoL ~ Surgery, 
              data = cos_surg, 
              random = ~1|Clinic, method = "ML")
summary(m2_surg)

anova(m1_surg, m2_surg)

m3_surg <-lme(Post_QoL ~ Surgery + Base_QoL,      # Base as other fixed effect 
              data = cos_surg, 
              random = ~1|Clinic, method = "ML")
summary(m3_surg)                                  # Presence of clin nulls covar aid seen in prev anova

x_m3_surg <-lme(Post_QoL ~ Base_QoL,      
              data = cos_surg, 
              random = ~1|Clinic, method = "ML")
summary(x_m3_surg)                                

anova(m1_surg, x_m3_surg)

##########################
# More Model Comparisons #
##########################

anova(m1_surg, m2_surg, m3_surg)  # VS: Do 1 df comp

# So it looks like having our random intercepts and
# the fixed effects for both predictors is our best model
# so far.

# Let's try adding random slopes into the model to see
# if that is also necessary.

#################
# Random Slopes #
#################

# So far our slopes have not been allowed to vary, but 
# we'll change that now.

m4_surg <- lme(Post_QoL ~ Surgery + Base_QoL, 
               data = cos_surg, 
               random = ~ Surgery|Clinic,
               method = "ML") 
summary(m4_surg)

# In this code, we've replaced `random = ~1|Clinic` with
# `random = ~Surgery|Clinic`. This means that we want the 
# slope of Surgery to be able to vary across Clinics.
# With this code, we get random intercepts for Clinic and
# random slopes for surgery.

##########################
# More Model Comparisons #
##########################

anova(m3_surg, m4_surg)

# Our model with random intercepts and slopes seems to be superior.

###################
# More Predictors #
###################

# We'll now finish adding the rest of our predictors.

m5_surg <- update(m4_surg, .~. + Reason)    #m4 best so far
summary(m5_surg)

m6_surg <- update(m5_surg, .~. + Reason:Surgery)
summary(m6_surg)


###############################
# Even More Model Comparisons #
###############################

anova(m4_surg, m5_surg, m6_surg)

####################
# Further Analyses #
####################

# It may be interesting to see if quality of life
# after surgery is different depeding on the reason
# for the surgery. Fortunately, `lme()` has a subset 
# argument that makes this easy to do.

# But first, we need to specify our subsets of reason.

phys_sub <- cos_surg$Reason == 1
cosm_sub <- cos_surg$Reason == 0

# Now we can run our model for physical reasons.

phys_m <- lme(Post_QoL ~ Surgery + Base_QoL, 
              data = cos_surg, 
              random = ~ Surgery|Clinic, 
              subset = phys_sub, method = "ML")
summary(phys_m)

# And our model for cosmetic reasons.

cosm_m <- lme(Post_QoL ~ Surgery + Base_QoL, 
              data = cos_surg, 
              random = ~ Surgery|Clinic, 
              subset = cosm_sub, method = "ML")
summary(cosm_m)


###################
## Growth Models ##
###################

# Make that data tall!

hm_tall <- honeymoon %>% 
  gather(key = time, value = satisfaction, Satisfaction_Base:Satisfaction_18_Months, na.rm = T)
hm_tall <- hm_tall[order(hm_tall$Person),]

hm_tall$time_num <- with(hm_tall,
                      ifelse(time == "Satisfaction_Base", 0,
                             ifelse(time == "Satisfaction_6_Months", 1,
                                    ifelse(time == "Satisfaction_12_Months", 2, 
                                           ifelse(time == "Satisfaction_18_Months", 3, 
                                                  NA)))))   # Look at data, err message won't be triggered

##################
# Baseline Model #
##################

baseline_hm <- gls(satisfaction ~ 1, 
                   data = hm_tall, method = "ML", 
                   na.action = na.exclude)
summary(baseline_hm)

###################
# Vary Intercepts #
###################

m1_hm <- lme(satisfaction ~ 1, 
             data = hm_tall, 
             random = ~ 1|Person, method = "ML",
             na.action = na.exclude, 
             control = list(opt = "optim"))
summary(m1_hm)

# Andy notes in his book that the default optimizer 
# doesn't always succeed in calculating for this data,
# so he chose another one that does work.

############
# Compare! #
############

anova(baseline_hm, m1_hm)

#################
# Fixed Effects #
#################

m2_hm <- update(m1_hm, .~. + time_num)   # Other time var is text, must be numeric (not factor)
summary(m2_hm)

#################
# Random Slopes #
#################

m3_hm <- update(m2_hm, random = ~ time_num|Person)
summary(m3_hm)

# this allows the slopes of time_num to vary by person

#########################
# Covariance Structures #
#########################

# You may wish to specify a particular type of covariance
# structure for your model. This is pretty easy to do with `lme()`.

# The three most common options are:

# `corAR1()` : first-order autoregressive covariance structure (equally spaced time points)'
# `corCAR1()` : same but for continous time covariate (not equally spaced time points)
# `corARMA()` : Allows the correlation structure to involve a moving average of error variance
# See the Field book for more information.
# Also,
?corClasses

m4_hm <- update(m3_hm, 
                correlation = corAR1(0, form = ~ time_num|Person))
summary(m4_hm)
intervals(m4_hm) # Specific to this type of object

# Remember how your book is called "Data Analysis: A Model Comparison Approach"?

###########################
# More Model Comparisons! #
###########################

anova(baseline_hm, m1_hm, m2_hm, m3_hm, m4_hm)  # Stick with m2_hm

#################
## Polynomials ##
#################

# You may wish to add polynomial terms to your models
# at some point. They don't make a ton of sense in this
# model, but it's what we've got, so let's roll with it.

#############
# Quadratic #
#############

m5_hm <- update(m2_hm, .~. + I(time_num^2))
summary(m5_hm)

#########
# Cubic #
#########

m6_hm <-update(m5_hm, .~. + I(time_num^3))
summary(m6_hm)

###############
# Alternative #
###############

m7_hm <- update(m2_hm, .~ poly(time_num, 3))
summary(m7_hm)

###########
# Compare #
###########

anova(m2_hm, m5_hm, m6_hm, m7_hm)





hm_tall %>%
    ggplot(mapping = aes(x = time_num, y = satisfaction)) +
             geom_point(stat = "summary", position ="dodge") +
             geom_line(position = "dodge", stat = "summary", fun.y = "mean") +
             geom_errorbar(stat = "summary", position = "dodge", fun.data = "mean_cl_normal") +
             theme_apa() +
             labs(x = "Time Points", y = "Satisfaction")


## My copy-paste script
ggplot(data = hm_tall, mapping = aes(x = time_num, y = satisfaction)) +
  stat_summary(fun.y = mean, geom = "point") + 
  stat_summary(fun.y = mean, geom = "line") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) +
  theme_apa() + 
  labs(x = "Time Points", y = "Satisfaction") 





