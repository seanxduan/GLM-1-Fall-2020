##########################################
### Visualization in R: ggplot2 Graphs ###
###              3/22/19               ###
##########################################
# Packages needed:

install.packages("tidyverse")
install.packages("jtools")
install.packages("RColorBrewer")
install.packages("carData")
devtools::install_github("kassambara/ggcorrplot")
install.packages("Hmisc")
install.packages("plyr")
install.packages("ggExtra")
install.packages("GGally")
library(tidyverse)
library(ggplot2)
library(jtools)
library(RColorBrewer)
library(carData)
library(ggcorrplot)
library(Hmisc)
library(reshape2)
library(plyr)
library(ggExtra)
library(GGally)

# Data used:

mtcars <- mtcars
iris <- iris
diamonds <- diamonds
orange <- Orange
vocab <- Vocab
mpg <- mpg

# Optional Readings:
# The Grammar of Graphics (Wilkinson, 1999)
## https://www.google.com/search?q=the+grammar+of+graphics&oq=the+grammar+of+graphics&aqs=chrome..69i57j0l5.3410j0j7&sourceid=chrome&ie=UTF-8
# A Layered Grammar of Graphics (Wickham, 2010)
## https://byrneslab.net/classes/biol607/readings/wickham_layered-grammar.pdf


##############
## Overview ##
##############

# In this lesson, we'll cover how to generate commonly 
# used graphs by implementing the information you 
# learned in the previous lesson.

# We will not examine EVERY specific combination of 
# functions possible, so in the future, you'll need
# to be comfortable editing the code presented here 
# in order to make your desired graphs.


###########################
## Basic Types of Graphs ##
###########################

################
# Scatterplots #
################

# You're already pretty familiar with scatterplots
# from the previous lesson. However, we'll go ahead
# and cover them here.

# Here is a simple scatterplot that plots sepal length
# against sepal width in the iris data.

ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point() + 
  theme_apa() +
  labs(x = "Sepal Length", y = "Sepal Width")

# We can also add species to the color aesthetic to
# get a better picture of the data. And let's go ahead
# and add in my favorite shape and move the legend.

ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point(shape = 21) + 
  theme_apa(legend.pos = "bottom") +
  labs(x = "Sepal Length", y = "Sepal Width")

# Or if you so wish, we can facet by Species instead
# of assigning it to the color aesthetic.

ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(shape = 21) + 
  theme_apa(legend.pos = "bottom") +
  facet_wrap(~ Species) +
  labs(x = "Sepal Length", y = "Sepal Width")

# Why not both?

ggplot(data = iris, mapping = aes(x = Sepal.Length, y = Sepal.Width, color = Species)) + 
  geom_point(shape = 21) + 
  theme_apa(legend.pos = "bottom") +
  facet_wrap(~ Species) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Sepal Length", y = "Sepal Width")

# We've gotten quite familiar with scatterplots up to
# this point, but an additional geom_*() that you may
# want to consider adding to your scatterplots is
# geom_jitter(). This is handy when your data fall on
# specific discrete values. Like with:

ggplot(data = mtcars, mapping = aes(x = cyl, y = carb)) + 
  geom_point() + 
  theme_apa() +
  labs(x = "# of Cylinders", y = "# of Carburetors")

# geom_jitter() adds some variation into your data
# that allows you to better see the clusters of points

ggplot(data = mtcars, mapping = aes(x = cyl, y = carb)) + 
  geom_point() + 
  geom_jitter() +
  theme_apa() +
  labs(x = "# of Cylinders", y = "# of Carburetors")

# An example with color as a factor of gear and regression
# lines for each group.

ggplot(data = mtcars, mapping = aes(x = cyl, y = mpg, color = factor(gear))) + 
  geom_point() + 
  geom_jitter() +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  theme_apa(legend.pos = "bottom", legend.use.title = TRUE) + # theme_apa defaults this to FALSE
  labs(x = "# of Cylinders", y = "Miles Per Gallon (mpg)", color = "# of Gears")

##############
# Histograms #
##############

# Histograms display the values of a variable on the
# x-axis and their respective frequencies on the y-axis.
# This can be accomplished using geom_histogram().

ggplot(data = mtcars, mapping = aes(x = carb)) + 
  geom_histogram() + 
  theme_apa() +
  labs(x = "# of Carburetors")

# You'll notice that any time you create a histogram
# you'll get the following warning:
# "`stat_bin()` using `bins = 30`. Pick better value with `binwidth`."
# This is because we haven't specified the number of
# bins we want our data to be sorted into.

# Since carb ranges from 1 - 8, we'll specify
# that we want 8 bins.

ggplot(data = mtcars, mapping = aes(x = carb)) + 
  geom_histogram(bins = 8, color = "black", fill = "white") + 
  theme_apa() +
  labs(x = "# of Carburetors")

# That looks much better.

#################
# Density Plots #
#################

# Density plots display very similar information
# as the histograms.

ggplot(data = mtcars, mapping = aes(x = carb)) + 
  geom_density() + 
  theme_apa() +
  labs(x = "# of Carburetors")

# As always, you can add color and what-not.
# NOTE: some graphs only allow certain aesthetics.
# Try to add "shape" to this graph.

ggplot(data = mtcars, mapping = aes(x = carb)) + 
  geom_density(color = "blue", fill = "skyblue") + 
  theme_apa() +
  labs(x = "# of Carburetors")

#############
# Box Plots #
#############

# Boxplots allow you to get a quick idea of the 
# distribution of one variable (typically categorical)
# in relation to another (typically continuous).

ggplot(data = iris, mapping = aes(x = Species, y = Sepal.Length)) + 
  geom_boxplot() + 
  theme_apa() +
  labs(x = "Species", y = "Sepal Length")

# While not necessary, you can also add colors
# to your boxplots, but you'll probably want
# to get rid of the unnecessary legend.

ggplot(data = iris, mapping = aes(x = Species, y = Sepal.Length)) + 
  geom_boxplot(aes(fill = Species)) + 
  theme_apa(legend.pos = "none") +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Species", y = "Sepal Length")

##############
# Bar Graphs #
##############

# These graphs are handy for showing means for
# groups in your data.

ggplot(data = iris, mapping = aes(x = Species, y = Sepal.Length)) + 
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") + 
  geom_errorbar(stat = "summary", position = "dodge", width = 0.2, fun.data = "mean_cl_normal") +
  theme_apa() +
  labs(x = "# of Cylinders", y = "Miles Per Gallon (mpg)")

# Don't forget that we can include additional 
# variables if we need to through facetting
# or additional aesthetics.

# Facet on "cut"

ggplot(data = diamonds, mapping = aes(x = color, y = price)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") + 
  geom_errorbar(stat = "summary", position = "dodge", fun.data = "mean_cl_normal") +
  theme_apa(legend.use.title = TRUE) +
  facet_wrap(~ cut) +
  labs(x = "Color", y = "Price")

# Fill by "color"

ggplot(data = diamonds, mapping = aes(x = cut, y = price, fill = color)) +
  geom_bar(position = "dodge", stat = "summary", fun.y = "mean") + 
  geom_errorbar(stat = "summary", position = "dodge", fun.data = "mean_cl_normal") +
  theme_apa(legend.use.title = TRUE) +
  labs(x = "Cut", y = "Price", fill = "Color")

###############
# Line Graphs #
###############

# Line graphs are particularly useful for displaying
# trends over time. 

# In this graph, we're showing the MEAN change in circumference
# across time for 5 trees.

ggplot(data = orange, mapping = aes(x = age, y = circumference)) +
  geom_line(position = "dodge", stat = "summary", fun.y = "mean") + 
  geom_errorbar(stat = "summary", position = "dodge", fun.data = "mean_cl_normal") +
  theme_apa(legend.use.title = TRUE) +
  labs(x = "Age (in days)", y = "Circumference (in mm)")

# In this graph, we're showing the  change in circumference
# across time for the 5 individual trees.
# Note: There are no errorbars here since we're graphing
# individual measurements, not means.

ggplot(data = orange, mapping = aes(x = age, y = circumference, color = Tree)) +
  geom_line(position = "dodge", stat = "summary", fun.y = "mean") + 
  theme_apa(legend.use.title = TRUE) +
  labs(x = "Age (in days)", y = "Circumference (in mm)")

# Let's make a graph (with different data) that has a bit
# more flair and shows off some of our other options

ggplot(data = vocab, mapping = aes(x = year, y = education, color = sex, shape = sex)) +
  geom_line(aes(linetype = sex), position = "dodge", stat = "summary", fun.y = "mean") +
  scale_linetype_manual(values = c("longdash", "dotted")) +
  stat_summary(fun.y = mean, geom = "point", position = "dodge") +
  scale_shape_manual(values = c(15, 17)) +
  geom_errorbar(stat = "summary", position = "dodge", fun.data = "mean_cl_normal") +
  theme_apa(legend.pos = "bottom") +
  scale_color_brewer(palette = "Dark2") + 
  labs(x = "Year Collected", y = "Education (number of years)")


##############################
## Advanced Types of Graphs ##
##############################

# In this section, we'll talk about more complex,
# but also informative, graphs.

# This section will be pulling heavily from information
# found on the internet, and some will require some
# setup before making the actual graph.

# This page is super nifty:
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html

###################
# Raincloud Plots #
###################

# https://micahallen.org/2018/03/15/introducing-raincloud-plots/

# We need to create the necessary theme for this:

raincloud_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

# A function we need

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}
GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )

# Summary Statistics

lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

sumld<- ddply(diamonds, ~cut, summarise,
              mean = mean(price), 
              median = median(price), 
              lower = lb(price), 
              upper = ub(price))

head(sumld)

# The plot

ggplot(data = diamonds, aes(y = price, x = cut, fill = cut)) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = price, color = cut), position = position_jitter(width = .15), size = .5, alpha = 0.8) +
  geom_boxplot(width = .1, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 5.25) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Spectral") +
  scale_fill_brewer(palette = "Spectral") +
  coord_flip() +
  theme_bw() +
  raincloud_theme

###############
# Correlogram #
###############

# Make a correlogram for the data in this case, mtcars.

corr <- round(cor(mtcars), 1)

# Then use that to make the correlogram.
# "type" can be "full", "lower", or "upper"

ggcorrplot(corr, hc.order = TRUE, 
           type = "full", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of mtcars", 
           ggtheme=theme_bw)

# Similarly:
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

######################
# Correlation Matrix #
######################

# Prepare some data
df <- mtcars[, c(1,3,4,5,6,7)]

# Correlation Matrix
ggcorr(data = df, nbreaks = 8, palette = "RdBu", name = "rho", label = TRUE,
       label_round = 3, label_color = "black")

######################
# Scatterplot Matrix #
######################

ggpairs(data = df)

##############################
# Marginal Histogram/Boxplot #
##############################

# This is how you can add boxplots or
# histograms to your scatterplots.

# Scatterplot
g <- ggplot(mpg, aes(cty, hwy)) + 
  geom_count() + 
  geom_smooth(method="lm", se=F)

ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")
# ggMarginal(g, type = "density", fill="transparent")