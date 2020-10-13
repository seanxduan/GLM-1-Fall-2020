#########################################
### Visualization in R: ggplot2 Intro ###
###            3/22/19                ###
#########################################

# Packages needed:

install.packages("tidyverse")
install.packages("jtools")
install.packages("RColorBrewer")
library(tidyverse)
library(ggplot2)
library(jtools)
library(RColorBrewer)

# Data used:

mtcars <- mtcars

# Optional Readings:
# The Grammar of Graphics (Wilkinson, 1999)
## https://www.google.com/search?q=the+grammar+of+graphics&oq=the+grammar+of+graphics&aqs=chrome..69i57j0l5.3410j0j7&sourceid=chrome&ie=UTF-8
# A Layered Grammar of Graphics (Wickham, 2010)
## https://byrneslab.net/classes/biol607/readings/wickham_layered-grammar.pdf


##############
## Overview ##
##############

# The ggplot2 packages is designed around the "Grammar of Graphics"
# concept (book linked above), hints the name "gg"plot2.

# In this lesson, we'll dip our toes into the ggplot2 package.
# This package is an incredibly versatile and powerful package
# for visually displaying your data and results.
# In this intro lesson, we'll cover the elements used in graphs
# in ggplot2, as well as common arguments used in the ggplot2 
# functions.

#https://ggplot2-book.org/introduction.html#welcome-to-ggplot2
#######################################
## The Elements of Graphical Grammar ##
#######################################

# The Grammar of Graphics details 7 key elements that make up good
# graphical representations. These are:
# 1) Data - simply the data being plotted
# 2) Aesthetics - The scales that we map our data onto
# 3) Geometries - The visual elements that represent our data
# 4) Facets - How to plot multiples
# 5) Statistics - Representations of our data
# 6) Coordinates - Where we'll plot our data
# 7) Themes - Everything in the graph that isn't our data

# We'll use some or all of these elements explicitly or behind
# the scenes when making our graphs in ggplot2.


#######################
## What is Required? ##
#######################

#####################
# Data & Aesthetics #
#####################

# The first step to making a graph in ggplot2 is to make a "ggplot object."
# This object will specify various pieces of information that will be used
# in making your graph, such as the dataset, the x and y variables, and 
# any other variables that you'll reference in your graph.
# We make this object with the following function:

ggplot(data = , mapping = aes())

# Now let's go ahead and start filling this in to make a graph.
# We'll use the mtcars dataset to look at the relationship between
# cars' mpg and weight.

mpg_wt <- ggplot(data = mtcars, mapping = aes(x = wt, y = mpg))

# Notice that running this code doesn't generate a graph. All we've
# done is specify the data and the variables - we need to also specify
# how we want this data to be graphed. 

##############
# Geometries #
##############

# Let's make a scatterplot from our ggplot object. We can do this
# by simply adding the point geometry to our object.

mpg_wt + geom_point()

# You'll notice two things about the previous line of code:
# First, running it generated our scatterplot for us. This is because the
#
# DATA, AESTHETIC, AND GEOMETRY ELEMENTS ARE ALL that is REQUIRED to 
#
# create a plot (and they've all been specified).
# Second, we didn't have to specify any arguments inside the geom_point()
# function. This is because it defaults to inheriting it's values from 
# the ggplot object we created. But, if we choose, we can go in and change
# these arguments.

############################
# Sometimes more is Better #
############################

# We can also add additional geometries or aesthetics to our graph.
# Let's edit some of our existing code.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_smooth(method = lm, se = TRUE)

# Here, we've added in the smooth geometry which adds a regression
# line to our graph. We've specified that we want the "method" to be
# lm and the we don't want confidence intervals.

# We can add additional aesthetics and geometries to change what our
# graphs display or how they display it.

# Aesthetics can be used to specify how we want our graph to look.
# For instance, this code will make the data points in our scatterplot 
# be red triangles. This is referred to as using the aesthetics as
# attributes.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + 
  geom_point(shape = 17, color = "red") + 
  geom_smooth(method = lm, se = FALSE)

# But more importantly, aesthetics allow us to represent additional 
# variables in our plots. For example, the following code allows us 
# to color the points in our graph based off of the cyl variable.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE)

# Note: "cyl" was factored since it is a categorical variable.
# Here, we've added an additional aesthetic to split our data by the number
# of cylinders in each engine and specified these groups using colors.
# Additional aesthetic options include, but are not limited to: color, size,
# linetype, alpha, fill, and shape. (Some aesthetic options only
# work with certain types of graphs.)

# To demonstrate a few more options:

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl),
                                    shape = factor(vs), size = hp)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE)
?mtcars
# Notice that different groups (e.g., 6 cylinder with v-shaped and 6 
# cylinder straight) have their own regression lines.

###############################
## More on Aesthetic Options ##
###############################

##############
# Color/Fill #
##############

# In ggplot2, we are given a host of color options that we can
# choose to apply to our graphs. Here is a link to a pretty
# good list to get you started on your coloring journey:
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf

# Here is one of our original graphs. 

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE)

# Let's say we didn't like the default colors. We can go in and 
# choose the colors we want in various ways.
# One is to use scale_color_manual (note that we use "color"
# because we used the "color" aesthetic; there are also options
# for "fill").

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  scale_color_manual("legend", values = c("4" = "black", "6" =
                                            "gray45", "8" = "white"))

# We can also use prespecified color palettes. I personally
# like Dark2.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  scale_color_brewer(palette = "Dark2")

# The world is your oyster as far as color, you have a 
# functionally infinite amount of options at your disposal.

########
# Size #
########

# The size options in ggplot2 are more limited than
# the color options, but you really don't need
# THAT many different size options.

# Size options range from 1 - 6.

# Note: Size is based on area, not radius.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point(size = 1) + 
  geom_smooth(method = lm, se = FALSE)

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point(size = 3.5) + 
  geom_smooth(method = lm, se = FALSE)

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point(size = 6) + 
  geom_smooth(method = lm, se = FALSE)

# Notice that we put the size code in the
# the geom_*() that we're wanting to edit.

#########
# Shape #
#########

# We have more options with shape than we do size. 
# We have numeric options that range from 0-25, as well
# as options for *, ., o, and O.
# My personal preference is 21.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point(shape = 0) + 
  geom_smooth(method = lm, se = FALSE)

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point(shape = 21) + 
  geom_smooth(method = lm, se = FALSE)

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point(shape = 25) + 
  geom_smooth(method = lm, se = FALSE)

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point(shape = "*", size = 6) + 
  geom_smooth(method = lm, se = FALSE)

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point(shape = "o", size = 6) + 
  geom_smooth(method = lm, se = FALSE)

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point(shape = "O", size = 6) + 
  geom_smooth(method = lm, se = FALSE)

############
# Linetype #
############

# We have 6 line types that I'm aware of, but we can mix and match
# them with colors and other aesthetics to increase
# the variety that we have.

# Note: geom_line() will default to "solid."

ggplot(data = mtcars, mapping = aes(x = carb, y = am, color = factor(cyl))) + 
  geom_line() +
  geom_point()

ggplot(data = mtcars, mapping = aes(x = carb, y = am, color = factor(cyl))) + 
  geom_line(linetype = "twodash") +
  geom_point()

ggplot(data = mtcars, mapping = aes(x = carb, y = am, color = factor(cyl))) + 
  geom_line(linetype = "longdash") +
  geom_point()

ggplot(data = mtcars, mapping = aes(x = carb, y = am, color = factor(cyl))) + 
  geom_line(linetype = "dotted") +
  geom_point()

ggplot(data = mtcars, mapping = aes(x = carb, y = am, color = factor(cyl))) + 
  geom_line(linetype = "dotdash") +
  geom_point()

ggplot(data = mtcars, mapping = aes(x = carb, y = am, color = factor(cyl))) + 
  geom_line(linetype = "dashed") +
  geom_point()

# If you ever need it, we technically also have "blank."

ggplot(data = mtcars, mapping = aes(x = carb, y = am, color = factor(cyl))) + 
  geom_line(linetype = "blank") +
  geom_point()

#########
# Alpha #
#########

# Alpha can be combined with nearly every other aspect of
# your graphs in ggplot2. The alpha values you assign
# affect the transparency of the associated elements.
# Values can range from 0 (completely transparent) to
# 1 (completely opaque).

ggplot(data = mtcars, mapping = aes(x = carb, y = am, color = factor(cyl))) + 
  geom_line(linetype = "dashed", alpha = 0) +
  geom_point()

ggplot(data = mtcars, mapping = aes(x = carb, y = am, color = factor(cyl))) + 
  geom_line(linetype = "dashed", alpha = 0.25) +
  geom_point()

ggplot(data = mtcars, mapping = aes(x = carb, y = am, color = factor(cyl))) + 
  geom_line(linetype = "dashed", alpha = 0.5) +
  geom_point()

ggplot(data = mtcars, mapping = aes(x = carb, y = am, color = factor(cyl))) + 
  geom_line(linetype = "dashed", alpha = 0.75) +
  geom_point()

ggplot(data = mtcars, mapping = aes(x = carb, y = am, color = factor(cyl))) + 
  geom_line(linetype = "dashed", alpha = 1) +
  geom_point()

#########################
## Additional Elements ##
#########################

##########
# Facets #
##########

# As mentioned earlier, facets allow us to plot multiple similar
# plots. This allows to "split plots" by a variable (another handy
# way for us to plot additional variables).

# In this code (which is similar to the previous), we'll facet our
# graph by cylinder number instead of coloring it by cylinder.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = hp,
                                    shape = factor(vs))) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(facets = ~ cyl)

# As you can see, this generated a graph for each level of cyl
# and put them all on the same scale for easy comparison.

##############
# Statistics #
##############

# Statistics are additional arguments that we can specify in
# ggplot2 code that specify how we want to display our data.
# You'll likely notice that many stat_*() and geom_*() functions
# have similar names.

# In practice in ggplot2, geom_* and stat_* are largely
# interchangeable - that is, you can use one or the other.
# However, your code will be formatted differently depending
# on which you choose to use.

# Let's rewrite some of our earlier code using stat_smooth()
# instead of geom_smooth(). We'll also add a regression line
# for the data if not split on cyl.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point() + 
  stat_smooth(method = lm, se = FALSE) + 
  stat_smooth(method = lm, se = FALSE, formula = y ~ x, color = "black")

###############
# Coordinates #
###############

# Like mentioned earlier, coordinates are where we display our
# data - that is, the values of the coordinate (typically x, y)
# planes of our graphs.

# Like geometries and statistics, there are many coordinate
# functions to explore in the ggplot2 package. Let's take a closer
# look at a few of these functions that are commonly used.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point() + 
  stat_smooth(method = lm, se = FALSE) + 
  stat_smooth(method = lm, se = FALSE, formula = y ~ x, color = "black")

# Here is our graph from earlier. Let's use a function to zoom in on
# just weight from 3 to 4.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point() + 
  stat_smooth(method = lm, se = FALSE) + 
  stat_smooth(method = lm, se = FALSE, formula = y ~ x, color = "black") +
  coord_cartesian(xlim = c(3, 4))

# As you noticed, the regression lines continued outside of the graphed
# area. This is because the coord_cartesian() function doesn't filter
# out the unused data.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point() + 
  stat_smooth(method = lm, se = FALSE) + 
  stat_smooth(method = lm, se = FALSE, formula = y ~ x, color = "black") +
  scale_x_continuous(limits = c(3, 4))

# This is handy if we're only interested in what is going on in this
# section, but not the sections to either side, because, as you hopefully
# noticed, our lines changed. That's because this code filters out the
# unused data (hence the warning messages).

# There are also coordinate functions that allow for transformations of
# the variables on the x- and y-axis. For example, we can perform a log
# transform on our new x-variable disp.

ggplot(data = mtcars, mapping = aes(x = disp, y = mpg, color = factor(cyl))) + 
  geom_point() + 
  stat_smooth(method = lm, se = FALSE) + 
  stat_smooth(method = lm, se = FALSE, formula = y ~ x, color = "black") +
  scale_x_log10()

# And we could also do a square-root transformation on the y-axis.

ggplot(data = mtcars, mapping = aes(x = disp, y = mpg, color = factor(cyl))) + 
  geom_point() + 
  stat_smooth(method = lm, se = FALSE) + 
  stat_smooth(method = lm, se = FALSE, formula = y ~ x, color = "black") +
  scale_x_log10() +
  scale_y_sqrt()

# A sometime handy transformation is the the reverse transformation
# which will perform as advertised - reversing the axis.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg, color = factor(cyl))) + 
  geom_point() + 
  stat_smooth(method = lm, se = FALSE) + 
  stat_smooth(method = lm, se = FALSE, formula = y ~ x, color = "black") +
  scale_x_reverse()

##########
# Themes #
##########

# Themes allow us to modify the appearance of our the elements
# of our graphs that aren't data.

# There are many pregenerated themes() in ggplot2. Such as:

# theme_bw()

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  theme_bw()

# theme_void()

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  theme_void()

# but probably the most useful to you will be theme_apa()
# which comes to use from the jtools package.This will
# make good looking APA format figures.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  theme_apa()

# You can also pick and choose to add certain
# "theme" elements without using a whole theme.
# For example, adding labels to your graphs.

ggplot(data = mtcars, mapping = aes(x = wt, y = mpg)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  theme_apa() +
  labs(x = "Weight", y = "Miles Per Gallon (mpg)")


################
## Conclusion ##
################

# In a future lesson, we'll go into depth on
# how to use the knowledge you gained today to
# create many common types of graphs, as well as
# some rather sophisticated graphs.