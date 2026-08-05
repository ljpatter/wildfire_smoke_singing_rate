install.packages("dplyr")
library(dplyr)

# The first time you go to use a package, you need to install it. You can do this by running 
# the following code: 



# R studio has a number of different elements. I am currently typing
# in a what is called a script. A script is essentially your instructions of what you want R to do. You can write your code in a script and then run it all at once or line by line. This is useful for keeping track of your work and for reproducibility.

# These number signs are called comments. Comments are not run as code, but they are useful 
# for explaining what your code is doing. You can write comments to remind yourself or others 
# what a particular line of code does.

# R is really just a fancy calculator. Let's do some math below; we weill run the code by either
# selecting the "Run" button above or by pressing "Ctrl + Enter".

# Let's add 2 + 2
2 + 2

# When we run code in the script, it doesn't print anything in here but will print the  
# output in the "console" below. The console is where R will print the results of your code.

# We can also assign values to variables. Variables are like containers that hold information.
# Let's assign the value 5 to a variable called "x"

x <- 5
y <- 10

# Now we can use the variable "x" in our calculations. Let's multiply "x" by 2
x * 2
y / x

# We can also use the function "print()" to print the value of a variable. 
# Let's print the value of "x")

print(x)

# Let's do something more complex. Let's create a vector of numbers from 1 to 10 and 
# assign it to a variable called "my_vector"

my_vector <- 1:10
print(my_vector)

# We can also use functions to perform operations on our data. Let's use the "mean()" 
# function to calculate the mean of "my_vector"

mean(my_vector)

# Another important concept in R is a matrix. A matrix is a two-dimensional array that 
# can hold numeric data. Let's create a 3x3 matrix and assign it to a variable called "my_matrix"

my_matrix <- matrix(1:9, nrow = 3, ncol = 3)
print(my_matrix)

# Lastly, a matrix is similar to a dataframe which is a two-dimensional data structure 
# BUT it can hold different types of data. Let's create a dataframe with some sample data and 
# assign it to a variable called "my_dataframe"

my_dataframe <- data.frame(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 35))
print(my_dataframe)







### Now we are ready to look at our data, which we downloaded from WildTrax. 

# Load csv file (I got this file path my right clicking on the file in Windows Explorer 
# and selecting "Copy as Path")
dat1 <- read.csv("C:/Users/leona/OneDrive/Desktop/Single_Species_-_WhiteThroated_Sparrow_(WTSP)_-_Sound_Rates_-_Wildfire_smoke_-_2023_-_Patterson_Tags_2026-07-07.csv")
            
# str() let's us look at the structure of our data. 
str(dat1)

# There are a lot of columns in here that we don't need for our analysis. Let's select only 
# the columns we need and assign it to a new variable called "dat2"

dat2 <- dat1 %>%
  select(location, recording_date_time, observer, species_code, individual_number,
         vocalization, abundance)

# We get an error message that says "could not find function "%>%". This is because we need 
# to load the dplyr "package", which contains the select() function and the pipe operator %>%. 
# Let's load the dplyr package by running the following code.

library(dplyr)

# The first time you go to use a package, you need to install it. You can do this by running 
# the following code: 

install.packages("dplyr")

# All subsequent times you want to use the package, you just need to load it with library(dplyr).

# Now we can run the code to select the columns we need and assign it to "dat2".

dat2 <- dat1 %>%
  select(location, recording_date_time, observer, species_code, individual_number,
         vocalization, abundance)

dat3 <- dat2 %>%
  filter(vocalization == "song")


# Alright, that is the end of the tutorial (for now). Below I have outlined the remaining tasks
# that need to be completed to get this dataset all cleaned up and ready for analysis

# 1. There are a number of rows that have "call" values - all we want is "songs"
# 2. species_code currently has abundance values of 1 - in reality, these are recordings where
# there are no WTSPs should we can delete these rows. 
# 3. Our dataframe is currently in a long-form while we need it in a wide-form (read online
# about the difference between these. Hint: We will need to pivot the dataframe so that 
# each species_code is a column and the abundance values are the values in those columns.

# Lastly, the whole reason we write code is so that we can reproduce our work. If we have 
# to do this analysis again in the future, or if someone else wants to do the same analysis, 
# they can just run this script and get the same result. This is one of the most important 
# aspects of coding and data analysis. It allows us to be efficient, accurate, and transparent 
# in our work.