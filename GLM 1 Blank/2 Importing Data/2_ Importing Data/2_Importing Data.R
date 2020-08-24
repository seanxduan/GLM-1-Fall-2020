##########################################
## Data Management in R: Importing Data ##
##              Kye Ripley              ##
##               8/23/19                ##
##########################################
# Packages needed:
install.packages("tidyverse")
library(tidyverse)
library(utils) # automatically loads with R
library(readr) # part of the tidyverse
library(haven) # part of the tidyverse
library(readxl) # part of the tidyverse
library(httr) # part of the tidyverse
library(dplyr) # part of the tidyverse

# Data used:
# You'll import these on your own this week, because...
# you know, it's the topic of this week's lesson.
# DON'T IMPORT NOW.
# However, the files you should be sure to have ready are:
hotdogs.txt <- "/Users/Kyle/Desktop/MizzouWork/GLM (TA)/2019 - Spring/Data/hotdogs.txt"
potatoes.csv <- "/Users/Kyle/Desktop/MizzouWork/GLM (TA)/2019 - Spring/Data/potatoes.csv"
potatoes.txt <- "/Users/Kyle/Desktop/MizzouWork/GLM (TA)/2019 - Spring/Data/potatoes.txt"
urbanpop.xls <- "/Users/Kyle/Desktop/MizzouWork/GLM (TA)/2019 - Spring/Data/urbanpop.xls"
urbanpop.xlsx <- "/Users/Kyle/Desktop/MizzouWork/GLM (TA)/2019 - Spring/Data/urbanpop.xlsx"
# Note: when we call on these file paths, we won't need to 
# put them in quotation marks; however, if you're putting
# a file path inside of an import command, you will need
# to put it inside quotation marks.

##############
## Overview ##
##############

## In this lesson, we'll cover the basics of importing data
# from some of the most common data file structes and sources:
# Flat Files - e.g., .csv, .txt, etc.
# Excel Files - e.g., xls, xlsx
# Web Files
# Files from other statistical software - e.g., SAS, SPSS, STATA

## To this end, we'll mostly focus on how to import data
# with the tidyverse packages. We'll focus on these instead
# of the functions available in base R, because these are,
# in general, faster and more consistent across functions.
# If you're isinterested in using the basic commands, 
# the functions in base R operate similarly.


################
## Flat Files ##
################

################
# read_delim() #
################

# Your most general function for importing a flat
# file is going to be read_delim(). This function
# allows you to specify the delimiter that is used
# in your data. It requires two arguments, but there
# are many more that you can also specify.
read_delim(file, delim)

# Let's import our first data file: potatoes.txt
# It's important to note that this is a 
# tab-delimited file with no column names.
# We'll use the col_names argument to fix that.
names1 <- ___("___", "___", "___", "___", "___",
            "___", "___", "___")
potatoes_txt <- ___(file = ___, 
                    ___ = "___", 
                    ___ = ___)

# If for some reason we only wanted certain rows,
# we could also use the skip and n_max arguments.
partial_potato <- ___(file = potatoes.txt,
                      ___ = "___",
                      ___ = ___,
                      ___ = ___,
                      ___ = names1)
# This skips the first 5 observations and imports
# only the next 10 observations.

# We can also specify the types of columns we'd 
# like to have, instead of manually changing them
# at a later time with the col_types argument.
# c = character
# d = double
# i = integer
# l = logical
# _ = skip the column
new_potatoes <- ___(file = potatoes.txt,
                           delim = "\t",
                           skip = 5,
                           n_max = 10,
                    ___ = "___",
                           col_names = names1)
___(___)

# We can instead use a collector function to
# pass col_types to our data. This is 
# particularly handy for factors.
fac <- ___(___ = ___("___", "___", "___"))
int <- ___()

hotdog_factor <- ___(file = ___,
                            delim = "___",
                            col_names = ___("___", "___", "___"),
                            col_types = ___(___, ___, ___))
___(hotdog_factor)

# It gets a little old having to specify the delim = argument over
# and over again. Surely there's a way to deal with that?


##############
# read_csv() #
##############

# Here's the answer to our prayers for importing CSV files.
# This function is a wrapper for read_delim(). That means
# that it has all the same arguments, except its defaults
# are set to import CSV files.
potatoes_csv <- ___(file = potatoes.csv)
# Notice that we didn't have to specify the delim as a comma
# or provide column names. The default for read_csv()
# assumes that column names are present.

# Note: If you're working with files from a country
# where they use commas instead of decimals, you can
# use the read_csv2() function to import this data.


##############
# read_tsv() #
##############

# What about tab seperated value files, you might ask?
# There's another wrapper to help with those. read_tsv()
potatoes_tsv <- ___(file = potatoes.txt,
                         col_names = names1)
                         

#################
## Excel Files ##
#################

##########
# ReadXL #
##########

# One super useful function of the ReadXL
# packagge allows us to view the sheet names
# of an excel file.
___(___)

# Similar to previous functions, we can use
# read_excel() to import the data.
pop_1 <- ___(path = ___, ___ = ___)
pop_2 <- ___(path = ___, ___ = "___")
pop_3 <- ___(path = ___, ___ = ___)
# Notice that you can use the sheet name or number.

###############
## Web Files ##
###############

# One particularly great thing about importing
# data in R is that you can import it directly
# from the internet - no downloading required!
internet_pools <- ___("https://raw.githubusercontent.com/KRR1114/class_files/master/swimming_pools.csv")
internet_potatoes <- ___("https://raw.githubusercontent.com/KRR1114/class_files/master/potatoes.txt",
                              col_names = FALSE)

# You can also download files through R so 
# that you can use them locally. This also
# improves reproducibility for your analyses.
___("https://raw.githubusercontent.com/KRR1114/class_files/master/potatoes.txt", 
    ___ = "local_potatoes.txt")
local_potatoes <- ___("local_potatoes.txt", col_names = FALSE)


###############################
## Statistics Software Files ##
###############################
# These handy functions brought to you
# by the Haven package.

#####
# R #
#####

download.file("https://github.com/KRR1114/class_files/blob/master/wine.RData?raw=true",
              destfile = "wine.RData")
___("wine.RData")

download.file("https://github.com/KRR1114/class_files/blob/master/weather.rds?raw=true",
              destfile = "weather.rds")
weather <- ___("weather.rds")

#######
# SAS #
#######

download.file("https://github.com/KRR1114/class_files/blob/master/sales.sas7bdat?raw=true",
              destfile = "sas7bdat")
sales <- ___("sas7bdat")

#########
# STATA #
#########

download.file("https://github.com/KRR1114/class_files/blob/master/trade.dta?raw=true",
              destfile = "trade.dta")
trade <- ___("trade.dta")

########
# SPSS #
########

download.file("https://github.com/KRR1114/class_files/blob/master/person.sav?raw=true",
              destfile = "person.sav")
traits <- ___("person.sav")
