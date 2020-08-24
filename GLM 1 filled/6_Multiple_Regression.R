#########################################
## Multiple Regression and Diagnostics ##
##              Kyle Ripley            ##
##                 9/6/19              ##
#########################################

# Packages needed:
install.packages("car")
install.packages("GGally")
library(car)
library(GGally)
library(olsrr)
library(tidyverse)
library(ggplot2)
library(haven)

# Data used:
dat <- read_sav("https://github.com/RipleyKyleR/public_data_files/blob/master/dataset%20for%20victoria.sav?raw=true") 

which(is.na(dat))           ##remove missing

dat <- dat %>% 
  filter(complete.cases(dat)) %>% 
  select(id, scorumin, pos, anxious, depcesd)

# Data Key:

# scorumin: Co-rumination with specific friend (sr)
# pos: Positive friendship quality
# anxious: RCMAS anxiety
# depcesd: CESD depression

##############################
# Initial Data Visualization #
##############################

# As always, it's a good idea to get a feel for the structure
# of your data before ever conducting any analyses. An easy way
# to do this is through data visualization.

# We can make a correlation matrix to examine the correlations
# between all of our variables.                                      ##breaks are for corr tiers

ggcorr(data = dat, nbreaks = 8, palette = "RdBu", name = "r", label = T,
    label_round = 2, label_color = "black")

# We could also make multiple scatter plots to examine the 
# different relationships in the data that we're interested in.
# We'll explore some more of ggplot2's options here.

ggplot(data = dat, mapping = aes(x = pos, y = scorumin, color = pos)) +
  geom_point(shape = 16, size = 3, show = legend) +
  theme_minimal() +                                     ## can use theme_apa
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  geom_smooth(method = "lm", color = "springgreen3")

ggplot(data = dat, mapping = aes(x = anxious, y = scorumin, color = pos)) +
  geom_point(shape = 21, size = 3, show.legend = T) +
  theme_minimal() +
  scale_color_gradient(low = "darkslategray1", high = "darkviolet") +
  geom_smooth(method = "lm", color = "deeppink")

ggplot(data = dat, mapping = aes(x = anxious, y = depcesd, color = anxious)) +
  geom_point(shape = "*", size = 6, show.legend = T) +
  theme_minimal() +
  scale_color_gradient(low = "springgreen", high = "midnightblue") +
  geom_smooth(method = "lm", color = "purple")

ggplot(data = dat, mapping = aes(x = depcesd, y = scorumin, color = depcesd)) +
  geom_point(shape = 16, size = 3, show.legend = T) +
  theme_void() +
  scale_color_gradient(low = "paleturquoise1", high = "palevioletred4") +
  geom_smooth(method = "lm", color = "black")

##############
# The Models #
##############

## Running the models ##

# We'll first start out with our simplest model:
# co-rumination predicted by positive friendship quality

ModelC <- lm(scorumin ~ pos, data = dat)
summary(ModelC)

# Next, we'll add anxiety as a predictor. Notice that we 
# simply add it into the lm() model.

ModelA <- lm(scorumin ~ pos + anxious, data = dat)
summary(ModelA)

# Finally, we'll add depression as our last predictor.

ModelA2 <- lm(scorumin ~ pos + anxious + depcesd, data = dat)
summary(ModelA2)

## Comparting the models ##

# We'll start by comparing our simplest model to our
# 2 predictor model.

anova(ModelC,ModelA)

# We can then get the ANOVA tables for each of the
# individual models.

Anova(ModelC)               ##Can do lowercase but different
Anova(ModelA)

# We can then compare our 2-predictor model with the 
# full model.

anova(ModelA,ModelA2)

###############
# Diagnostics #
###############

# Because `ModelA` was our preferred model,
# we'll move ahead with that one.

# These functions should mostly be familiar
# from last week's lesson.

## Leverage ##
lev <- hat(model.matrix(ModelA))
plot(lev)

ols_plot_resid_lev(ModelA)

## Studentized Deleted Residuals ##

ols_plot_resid_stud(ModelA)

## Cook's Distance ##

ols_plot_cooksd_bar(ModelA)

## Normality Assumption ##

qqnorm(ModelA$res)
qqline(ModelA$res)
hist(ModelA$res)

## Homogeneity of Variance ##

plot(ModelA)            ##Can keep hitting return to see other graphs


## Redundancy, tolerance ##

# We'll now take a look at some new functions
# that deal with your Variance Inflation Factors.

vif(ModelA2)
ols_vif_tol(ModelA2)

vif(ModelA)
ols_vif_tol(ModelA)

## Model without Obs 338 ##

dat2 <- dat[-338, ]
ModelA_clean <- lm(scorumin ~ pos + anxious, data = dat2)
summary(ModelA_clean)

# For a reminder...

summary(ModelA)


