#######################
## Introduction to R ##
##    Kyle Ripley    ##
##      8/23/19      ##
#######################

# The RStudio environment

## Note: anything written in an R script (this type of document)
## will be interpreted by R as code UNLESS you specify that it
## isn't code by putting the number sign in front of it.

## As you can see, RStudio splits your display into 4 distinct quadrants:
## the top left is where any scripts or documents will be displayed,
## the bottom left is your console (where you can directly run code),
## the top right is your environment (where everything you assign or run will be listed),
## and the bottom right is where you can view your files, graphs, packages, and help docs.

######################
# Basic Calculations #
######################

## To start our voyage into R, let's point out that it can function
## like a simple calculator.

2 + 2

## Note: to run the above code, you can do the following:
## click the Run button while your cursor is on the correct line,
## use the short cut command+return (Mac) or control+enter (Windows),
## or highlight the code you wish to run and do either of the two
## previously mentioned methods.

2 - 2

2 * 3

8 / 2

5 ^ 2

## But beyond simple arithmetic, R can evaluate logical arguments as well.

1 < 10

1 > 10

10 == 10 ## Double equal sign means "is equal to"

10 != 11 ## Exclamation equal means "is not equal to"

#######################
# Assigning Variables #
#######################

## Arithmetic is cool and all, but we want to do more with R.
## This all starts with being able to assign values to variables.
## We used 10 a lot earlier, so let's save ourselves some time
## by assigning it to a variable.

x <- 10

## Note: you can also use a single equal sign to assign in R,
## but I recommend sticking to <-
## Note2: notice that x has now appeared in your environment

## We can use variable just like we can any other input.

x + 5

x == 10

## Let's make another variable, y.

y <- 4

## Time for more arithmetic!

x + y

z <- x / y

z
###################
# Using Functions #
###################

## To effectively use R, we need to start exploring functions.
## Functions are the meat and potatoes of R, and they're how
## you'll accomplish the vast majority of your work.
## They all follow a general format:

name_of_function(argument1 = blah, argument2 = blah blah, etc.)

## Note: notice the error symbol on line 85. This is because
## this is not runnable code.

## One of our most basic functions is sum(), which simply 
## sums all of the numbers passed to it.

sum(1,2,3,4)

?sum

## Running a question mark followed by a function name
## will take you to that functions documentation.
## Here you can see all the information about a function.

## We now know that sum() takes two arguments. 

sum(... = 1:6, na.rm = F)

## Note: in general, it's good to include the argument names.
## Note2: using a colon (:) tells R that you want to include
## both numbers and all numbers in between.

####################
# Creating Vectors #
####################

## Similar to assigning variables, we can assign multiple
## values to one object, known as a vector.

## To do this, we'll use the concatenate function, c().
## This function takes all of the information given to it
## and makes a vector.

## Let's make a vector of the numbers from 1 to 10 and call it x.

x <- c(1:10)

## Note: This will overwrite our original x variable.

## Now, let's create a new vector, y, which contains
## the numbers 11 though 16, a missing value for 17,
## and 18 through 20.f

y <- c(11:16,NA,18:20)

## Note: R uses "NA" to denote missing values

## Let's now use a function to calculate the averages
## of both of our vectors.

mean(x)

mean(y)

## That didn't seem to work for y. Let's take a peek
## at the function documentation.

?mean

mean(y, na.rm = F)

mean(y, na.rm = T)

## We have another function that is helpful for making vectors, rep().
## Let's look at it's R documentation.

?rep

## This function will replicate whatever input is given to it. The 
## way that it replicates can be specified with the times and each 
## arguments.

## For example, if we wanted to create a vector of 10 1s, we could do:

d <- c(1,1,1,1,1,1,1,1,1,1)



## Note: running the name of a variable/vector will return its value.

## Instead, we can use our new friend, rep()

e <- rep(x = 1, times = 10)

e==d

## If we wanted to make a vector that included the 
## numbers 1 through 5 reapeted 3 times:

g <- c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5)
g

## Or, we could use rep() and make our lives easier

h <- rep(x = 1:5, times = 3)

h

h==g
## If on the other hand, we wanted the numbers to be in order,
## we could use the each argument.

i <- rep(x = 1:5, each = 3)
i

########################
# Creating Data Frames #
########################

## If a collection of variables is a vector, then what is a collection
## of vectors? A data frame.

## Data frames are R's most common way of storing datasets, such
## as what you may have seen in excel or SPSS.

## We'll use another function, data.frame(), to create these.
## Inside of your data.frame() code, you have the option to 
## use already created vectors or to create the vectors in
## your code. We'll do this both ways.

?data.frame

old_vectors <- data.frame(g, h, i)

## Note: Looking at your environment will reveal a new section, data.
## You can view data here as if they were a spreadsheet by calling
## the name of the data or by clicking on it in the environment.

## Now, we'll create a data frame from scratch using your age data.

grad_age <- data.frame(age = c(38, 26, 24, 25, 33, 22, 24,
                               27, 28, 28, 29, 22, 23, 29, 32),
              pred_age = rep(x = 26, times = 15))

##########################
# Age Example from Class #
##########################

## Let's start off by calculating the mean age of the class.
## We'll use some new notation - the dollar sign - to specify
## variables inside of our data frame.

mean_age <- mean(grad_age$age)

## We'll now cover how to add new variables to our dataframe
## using the same notation. We'll do this with our model error.

grad_age$error <- grad_age$age - grad_age$pred_age
grad_age

## As you can see, that calculated the error and made a new column.
## We'll do the same for some other measures.

grad_age$abs_error <- abs(grad_age$error)

grad_age$sqr_error <- grad_age$error^2

## We'll now calculate some of our other measures using the formulas 
## from your book.

error_count <- sum(grad_age$error !=0)

error_sum <- sum(grad_age$error)

SAE <- sum(grad_age$abs_error)

SSE <- sum(grad_age$sqr_error)

## And some more

grad_age$mean_error <- grad_age$age - mean_age

grad_age$mean_error_sq <- grad_age$mean_error^2

PRE <- 1 - sum(grad_age$mean_error_sq)/SSE

MSE <- sum(grad_age$mean_error_sq)/(length(grad_age$age) - 1)

SD <- sqrt(MSE)

CV <- SD/mean_age

#######################
# Installing Packages #
#######################

## Remember how I said that functions were super important?
## Well that would make packages even more important, because
## packages are how we are able to utilize functions that
## other R users have written.

## Installing packages is easy, simply do the following:

install.packages("name_of_package")

## Note: The name of the package (like any string in R)
## must be placed in quotation marks.

## Let's go ahead and install some packages that we'll be
## using throughout the semester.

install.packages("tidyverse") # This is a large suite of packages
install.packages("papaja")

## When you want to use functions from a package that you've
## installed, you need to load the package by doing
## the following:

library(tidyverse)

## Note: Since tidyverse is now technically an object,
## it doesn't need to be in quotations.

##############
# Extra Help #
##############

## If you'd like a more in depth review of the R basics,
## feel free to visit the accompanying tutorial at:

https://mizzou-r-resources.netlify.com/beginning_with_r.html