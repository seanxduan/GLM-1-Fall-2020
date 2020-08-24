###################################
## Linear Regression Diagnostics ##
##         Kyle Ripley           ##
##           9/6/19              ##
###################################

# Packages needed:
install.packages("olsrr")
library(olsrr)
library(tidyverse)
library(ggplot2)

# Data used:
dat <- read_tsv("https://raw.githubusercontent.com/RipleyKyleR/public_data_files/master/Album%20Sales%202.dat")

# In this dataset, each row represents an album with the following variables:
# adverts - amount (in thousands of dollars) spent promoting the album before release
# sales - sales of the album (in thousands) the week after release
# airplay - number of times songs from the album played on the radio the week prior to release
# attract - attractiveness of the band on a scale from 0 (hideous potato-heads) to 10 (gorgeous sex objects)

#############
# Our Model #
#############

# Because we're focusing on simple one predictor regressions right now,
# we'll create a model that has album sales as the outcome and money
# spent on advertisements as the predictor.

mAdvert <- lm(sales ~ adverts, data = dat)    ## Whoops! X<->Y
summary(mAdvert)

############
# Outliers #
############

# You've already learned some ways to identify outliers in your
# data during the lesson Tidying Data. However, now we will look
# at how to identify our outliers visually through scatterplots
# - which may be a bit easier to do. 

# I'll also take this opportunity to gently introduce you to the
# ggplot2 package from the tidyverse.

?ggplot

ggplot(data = dat, mapping = aes(x = adverts, y = sales)) + 
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)                   ## Stick to this for scatter

# Do you see any points that you think might be outliers?

############
# Leverage #
############

# Leverage values are useful for identifying abberant values,
# and we have a few ways to easily obtain and visualize
# those values in R.

# First, let's create a new variable in our data frame
# that contains the leverage value for each album.

dat$lev <- hat(model.matrix(mAdvert))

# We can then make a simple plot of these lever values
# to see if any observations have a much larger value
# than the other observations.

plot(dat$lev)

# Looking at the plot, we can see that we have a few
# observations above .025 (a subjective value for this
# data that doesn't translate to other data). Let's 
# find out which observations those are.

ind <- which(dat$lev > 0.025)
dat[ind,]

#################################
# Studentized Deleted Residuals #
#################################

# We can also use studentized deleted residuals to get an
# idea of influential observations in our data.

dat$stud_del_resid <- rstudent(mAdvert)

# We can get a handy plot of these values with the 
# `ols_plot_resid_stud()` function.

ols_plot_resid_stud_fit(mAdvert)

# If we wish to make this plot without the default diagnostics,
# we can pull the necessary values and plot it ourselves.
# NOTE: we already pulled the studentized deleted residuals

dat$predicted <- predict(mAdvert)

plot(dat$predicted,dat$stud_del_resid)

#################
# A Useful Plot #
#################

# We also have a plot that will look aat both leverage
# and studentized deleted residuals.

ols_plot_resid_lev(mAdvert)

###################
# Cook's Distance #
###################

# Cook's distance is also a useful diagnostic for our data.
# We can use these values much like we've used the previous.

dat$cooks_d <- cooks.distance(mAdvert) 

plot(dat$cooks_d, ylab = "Cook's Distance")

# But we also have more useful plots.

ols_plot_cooksd_bar(mAdvert)

#################################
# Non-Normal Error Distribution #
#################################

# It's also a good idea to get an idea of the structure
# of your distribution of errors. We can do this easily
# by making a qq plot.

qqnorm(mAdvert$res) 
qqline(mAdvert$res)

# Ideally, you want the errors to fall on the line.

#################################
# Homogeneity of Error Variance #
#################################

# You also want to check that the error variances
# of your data are homogeneous.

# Will again go back to using ggplot2 for this graph.

ggplot(data = dat, mapping = aes(x = predicted, y = stud_del_resid)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)

############################
# Removing Abberant Values #
############################

# You very rarely ever want to TRULY delete data. We can get around 
# this by just creating a new dataframe that doesn’t include the 
# cases we choose to delete.

# This can be helpful in determining if removing these values 
# has a significant impact on your analyses.

dat_no_infl <- dat[-c(169, 184),]                      ## Straightfwd way to remove things


