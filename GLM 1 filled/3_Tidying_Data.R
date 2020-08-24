########################################
## Data Management in R: Tidying Data ##
##             Kyle Ripley            ##
##               9/6/19               ##
########################################
# Packages needed:
install.packages("tidyverse")
library(tidyverse)
library(lubridate)

# Data used:
weather <- read_csv("https://raw.githubusercontent.com/RipleyKyleR/public_data_files/master/weather.csv")
# This is historical weather data from Boston

##########################
# Why is this important? #
##########################

# Almost all real world datasets that you will work with start out dirty.

# It is often said that 80% of data analysis is spent on the process of cleaning 
# and preparing the data (Dasu and Johnson 2003).

# The ability to clean data, especially in the emerging world of "big data" 
# is an invaluable skill.

# For big data scientists, 'janitor work' is key hurdle to insights - NYT, 2014
# https://www.nytimes.com/2014/08/18/technology/for-big-data-scientists-hurdle-to-insights-is-janitor-work.html


#########################
# Meeting Your Raw Data #
#########################

# Before we get into the nuts and bolts of tidying data,
# let's first test your instincts about what makes data messy.

# To start off, let's make sure that we've imported a dataframe:

class(x = weather)  ##golden if data.frame

# Now, let's take a look at our weather data and see why we might need 
# to clean it in the first place.
head(x = weather, n = 5)

# The data looks pretty similar to what we find in spreadsheets and other
# data sources that we receive for analysis.
tail(x = weather, n = 5)

# What problems do you notice with the dataset?
# "X" appears meaningless
# What do "X1", "X2", etc mean?
# If they represent days, then we have time in both rows and columns
# The "measure" variable appears to contain multiple variables
# All the missing values


########################
# What is "Tidy Data?" #
########################

# "Tidy data" is essentially a philosophy of how to standardize data structure
# to make analyses and understand the data simpler and more straightforward.
# "Happy families are all alike; every unhappy family is unhappy in its own way" — Leo Tolstoy
# "Like families, tidy datasets are all alike but every messy dataset is messy in its own way" - H.W.
# For a full overview, check out Hadley Wickham's paper -> https://vita.had.co.nz/papers/tidy-data.pdf

## Five most common problems with messy datasets ##
# 1. Column headers are values, not variable names
# 2. Multiple variables are stored in one column
# 3. Variables are stored in both rows and columns
# 4. Multiple types of observational units are stored in the same table
# 5. A single observational unit is stored in multiple tables

# Before we move on, let's take a look at how to fix each of these.
# Note: #4 and #5 are outside the scope of this lesson, but
# can fairly easily be adapted with things you'll learn here.


############################
# Welcome to the Tidyverse #
############################

# To see a list of all packages include in the tidyverse, use:
tidyverse_packages()

## What is the Tidyverse?
# https://www.tidyverse.org/
# The Tidyverse is "a set of packages that work in harmony because they 
# share common data representations and API design."


###########################
# Exploring Your Raw Data #
###########################

## Understanding our data's structure ##

# We can quickly check to verify that our data is in a data.frame format
class(x = weather)

# We may also want to quickly get its dimensions
dim(x = weather)

# or the names of its variables.
names(x = weather)   ##Coding for resolving coding errors 

# We can also get a general overview of its structure with:
str(object = weather)
# OR
glimpse(x = weather)

# We may also want to get a quick summary of each "variable" <- (not strictly speaking because this is far from tidy)
summary(object = weather)

# Now that we have a better idea of what tidy data looks like,
# let's take another peak at our data.
head(x = weather, n = 3)
tail(x = weather, n = 3)

# Now that you're familiar with the principles of tidy data and the
# weather dataset, what are we going to need to fix for this dataset
# to be considered tidy?

#####################
# Tidying Your Data #
#####################

# The tidyr package (included in the tidyverse) is incredibly useful
# for tidying data. Go figure.
# We won't use all of the functions in the package, but we'll explore some
# of the most commonly used:
gather()                       ## Grabs multiple columns labels and stacks them into a "key" column w values in a seperate "value" column
spread()                       ## Oppo of gather; Take a cat "key" column and make each level into a column and use "value" to gather assoc values 
separate()
unite()                        ## Unite values from seperate column into a single value in a single column
# as well as a number of functions from other tidyverse packages.

## Let's get started with the tidying! ##

# 1. Column names are values of what should be a new variable "day"
# To address this, we'll use the gather() function.
weather2 <- gather(data = weather, key = day, value = value, X1:X31, na.rm = TRUE)
# This line of code is saying that we want to make a new data set called
# "weather2" with a variable called "day" that has the names "X1:X31" as its
# values and a variable called "value" that has the values of "X1:X31" as its 
# values, and also to remove rows that would have missing values.

# 2. Meaningless variable "X"
# To address this, we'll simply delete the column:

# 3. Variable names represented as values, particularly in "measure"
# To address this, we'll use the spread() function.
weather3 <- spread(data = without2, key = measure, value = value)
# This line of code is saying that we want to make a new data set called
# "weather3" by taking the values in the "measure" variable and making a new 
# column for each value of "measure." We then want to assign all of the corresponding
# values in the "value" variable to the appropriate measurement and observation.

# At this point, our data is technically tidy. However, to ease our burden later when
# it's time for analyses, we're going to do a bit more cleaning and polishing.


#######################
# Analysis Ready Data #
#######################

# 4. Fix the dates
# To address this, we're going to make use of some of the handy functions
# in the stringr and lubridate packages (included in the tidyverse).
# First we need to remove the "X"s from the days
weather3$day <- str_replace(string = weather3$day, pattern = "X", replace = "")
# This code is saying that we want to look for the pattern "X" inside of the day 
# variable and replace it with nothing (or, ya know...remove it).
# Next, we need to combine all of the date variables together.
weather4 <- unite(data = weather3, col = date, year, month, day, sep = "-")
class(weather4$date)
# This line is saying that we want to create a new variable "date" by 
# combining year, month, and day separated by a dash.
# We then want to convert our new column to the proper ymd format.
weather4$date <- ymd(weather4$date)
class(weather4$date)

# 5. Sensible column ordering
# Next, we want to arrange our columns in a sensible order.
weather5 <- select(.data = weather4, date, Events, CloudCover:WindDirDegrees)
head(weather5, n = 1)
# Looking good?

# 6. Change values to numeric
# Next, so that we can analyze the data, we want our numbers to be numbers
str(weather5)
# Check out the 2nd to last variable, "PrecipitationIn"
# "What do your elf eyes see?"
test <- as.numeric(weather5$PrecipitationIn)  ## Be careful. Lost data ("T" means trace, make into 0)
test
# In case you didn't catch it, there are "T"s in the dataset to denote
# "trace amounts" of precipitation. Let's just replace this with 0.
weather5$PrecipitationIn <- str_replace(string = weather5$PrecipitationIn,
                                        pattern = "T", replacement = "0")
# This line is saying that we want to find every "T" in "PrecipitationIn"
# and replace it with "0"
# Now that we've fixed that, we can go about making our variables numeric.
weather6 <- mutate_at(.tbl = weather5, .vars = vars(CloudCover:WindDirDegrees),
                   .funs = funs(as.numeric))
str(weather6)
summary(weather6)
# Well isn't that just nifty?

# 7. Always check for aberrant values and missing data
# To do this, we'll use some pretty handy techniques that should
# be applicable to almost any situation where you're worried about
# strange values in your dataset.
sum(is.na(weather6))
# This isn't the most intuitive since we have character vectors.
sum(is.na(weather6[,-2]))
# This line is doing two things: checking for missing values and 
# then counting them up. How do we find them?
summary(weather6) # Will identify the variables
# Next, we want to find the observations.
ind <- which(is.na(weather6$Max.Gust.SpeedMPH))   ## Vector of rows w missing data
weather6[ind, ]
# These lines allow us to identify the observations with the NAs
# and then see the full observation for each.
# In practice, do with your NAs what is most situationally appropriate.
# Next, we need to look for any bizzare values.
summary(weather6)
# What'd you find or not find?
# What observation is it?
ind <- which(weather6$Max.Humidity == 1000)
weather6[ind, ]
# We can probably assume it was a typo
weather6$Max.Humidity[ind] <- 100
# Are there any more?
summary(weather6)
# Let's follow through the same process...
ind <- which(weather6$Mean.VisibilityMiles == -1)
weather6[ind, ]
# How do we know what to replace this with? Do we know?
weather6$Mean.VisibilityMiles[ind] <- 10
# Don't forget your handy histograms for checking extreme values
hist(weather6$Mean.TemperatureF)
# Note: there's nothing wrong with this one

# 8. Don't use obnoxious column names
# My opinion on name format is as follows
# _ > . > camelback
# That is, I prefer to seperate words with underscores more than
# periods, and I think camelback is PrettyMuchTheMostObnoxiousThing.
# Also, don't capitalize variable names, it's annoying to get errors
# because you forgot to capitalize something.
# So, let's make our variable names a bit easier on the eyes:
new_colnames <- c("date", "events", "cloud_cover", "max_dew_point_f",
                  "max_gust_speed_mph", "max_humidity", "max_sea_level_pressure_in",
                  "max_temperature_f", "max_visibility_miles", "max_wind_speed_mph",
                  "mean_humidity", "mean_sea_level_pressure_in", "mean_temperature_f",
                  "mean_visibility_miles", "mean_wind_speed_mph", "mean_dew_point_f",
                  "min_dew_point_f", "min_humidity", "min_sea_level_pressure_in",
                  "min_temperature_f", "min_visibility_miles", "precipitation_in",
                  "wind_dir_degrees")
names(weather6) <- new_colnames
# Much better.

# 9. Don't have blank spaces in your data
# This is a pretty easy fix. We only have blanks in the "events" variable.
ind <- which(is.na(weather6$events))
weather6[ind, 2] <- "None"
# This line of code is just going into the events variable and replacing
# the empty cells with "None"

# Now, take a moment to appreciate the majesty of your clean and tidy data!
head(weather6)


########################
# Your New Best Friend #
########################

# One other giant perk about the tidyverse packages is that they
# allow for the use of a pipe operator, %>%, to simplify your code and    ## !All to L becomes 1st arg
# make it more straightforward.
# Remember how we went through 7 iterations of the weather data?
# Well, no more!
# When reading this code replace the pipe operator with "then."
# Notice that this one line of code makes all of our changes from
# #1. through #7. (you'll want to find weird values in the normal 
# process).
# It is worth noting that there are a few functions here from the
# dplyr package (yay tidyverse!) that we haven't covered yet.
# That will come soon enough.                                     ## Aberrant ona case by case bases

awesome_new_weather <- weather %>%
  gather(key = day, value = value, X1:X31, na.rm = TRUE) %>%
  select(-X) %>%
  spread(key = measure, value = value) %>%
  mutate(day = str_replace(string = day, pattern = "X", replacement = "")) %>%
  unite(col = date, year, month, day, sep = "-") %>%
  mutate(date = ymd(date)) %>%
  select(date, Events, CloudCover:WindDirDegrees) %>%
  mutate(PrecipitationIn = str_replace(string = PrecipitationIn,
                                       pattern = "T", replacement = "0")) %>%
  mutate_at(.vars = vars(CloudCover:WindDirDegrees),
            .funs = funs(as.numeric))

##########################
## Answers From Earlier ##
##########################

# Now that you're familiar with the principles of tidy data and the
# weather dataset, what are we going to need to fix for this dataset
# to be considered tidy?
# 1. Column names are values of what should be a new variable "days"
# 2. Meaningless variable "X"
# 3. Variable names represented as values, particularly in "measure"
# 4. Fix the dates
# 5. Sensible column ordering
# 6. Change values to numeric
# 7. Always check for aberrant values and missing data
# 8. Don't use obnoxious column names
# 9. Don't have blank spaces in your data