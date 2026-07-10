# Example R script: Advanced Stats in R
# Week 1
# Bernd Figner Sept 2019; updated 2020, 2021, 2022, 2023, 2024, 2025

# this script covers the things that I showed you in the first class in the "live R session"

# BTW, every line that starts with # is treated as a comment, i.e., it will not be "run" (aka executed) as code

# We can use R as a simple calculator:
2 + 2

# But there are more interesting uses of R, of course

# Here, we create a vector called a
a <- 2 + 2

# let's see what's in a
a

# the arrow <- means "put the stuff right of the arrow into the thing left of the arrow"
# sometimes, but not always, you can also use =
# but  = sometimes means "equal to" in a logical sense. therefore, to avoid any confusion, for assigning a value to an object, I would always use the arrow, not =


# let's create a vector with more entries

c(8, 13, 17, 1, 9, 11, 0, 15, 17, 23) # this command doesn't create a vector, it just puts these numbers in the console window!

# to create a vector, we have to create a new object and assign the values to it with <-
vec1 <- c(8, 13, 17, 1, 9, 11, 0, 15, 17, 23) # save the numbers into an object called vec1

vec1 # see what's in the object


# if you need help regarding a specific function, for example regarding the function c()
?c()
#same as:
?c
help(c)

# let's create some super simple plots (more advanced and nicer plots will be shown later in the course)
plot(vec1) # this just plots the values in the order in which they are in the vector

# with this, we can create a simple histogram
hist(vec1)
?hist # if we want to have a bit more information. at the very top left of the help file, you can see, that this command comes from the "graphics" package, which is automatically installed when you install R


# we can change how many different bars we want to have (i.e., the number of categories on the x axis)
hist(vec1, breaks = 10)
hist(vec1, breaks = 50)

# we can also create a simple boxplot:
boxplot(vec1)

# there are different ways how to save a figure to a file (when you later want to add the figure for example to a word or powerpoint document)

# the simplest way is, once you have the graphics window open, to click  on the menu item "File" --> "Save as"
# but you can save figures also via commands; and then you can also choose which format (e.g., pdf, jpg, png, etc)
# we'll also look at that in week 3 when we talk about figures in ggplot2, but here are some links for these non-ggplot2 figures, if you are curious:
# https://www.stat.berkeley.edu/~s133/saving.html
# http://www.sthda.com/english/wiki/creating-and-saving-graphs-r-base-graphs

# one package that creates a bit more advanced figures is lattice (these lattice figures are not as pretty as ggplot2 figures, but I still use these kinds of plots often, especially when I want to quickly look at the data, rather than creating a nice figure for a publication)
install.packages("lattice") # let's install the package; in case R asks you whether you also want to install the "dependencies" then say yes (often, a package requires other packages to be installed to function properly, these other packages are the so-called dependencies). one way to make sure the dependencies are installed is by adding the argument dependencies = TRUE to the command, thus it would then look something like this (the name of the package will be of course different, if you want to install a different package): install.packages("lattice", dependencies = TRUE)

library(lattice) # let's load the package

# let's create a densityplot
densityplot(vec1)


# In week 2, we'll talk more about measures to describe data, such as describing the central tendency with the mean or median etc.
# here, only very quickly some very basic commands

# descriptives of the central tendency
mean(vec1) # mean

median(vec1) # median

min(vec1) # minimum
max(vec1) # maximum

# the summary command is used very often in R. depending on the kind of object a summary is requested for, different things are shown in the output.
# The summary output for a simple vector is this:
summary(vec1)

# But, as you will see later in the course, when we run for example a linear regression, we then also ask for the summary output for the regression model, and then the output will contain all the relevant information for the linear regression, including significance tests etc etc. But we'll talk about that quite soon.


# One VERY important and handy thing in dealing with data is indexing
# indexing in a vector:
vec1[2] # this prints to the console the second element in the vector
vec1[3:6] # this prints to the console the 3rd to 6th element (i.e., the 3rd, 4th, 5th, and 6th)


# let's look a bit more into indexing:
# let's create a "character" vector, i.e., a non-numeric vector
d <- c("class", "stats", "is", 'a', 'this')

d[1] # here, we print the first element to the console window

d[2:4] # the 2nd to 4th element

# in R, it is very convenient to combine different commands, here, we use the command c() within the brackets
d[c(5, 3, 4, 2, 1)]

# what does this do? if we want to understand a more complicated command like this, I always try to look first at the more inner part:
c(5, 3, 4, 2, 1) # ok, so that just is basically a vector with the numbers 5, 3, 4, 2, and 1

# and these numbers are then put into this: d[]

# which means the vector of numbers serves as the index numbers to print out the elements of the vector d in the specific order we asked for

# 2-dimensional objects: matrices and data frames
# usually, data sets are represented in data frames

# one way to create a data frame from scratch is to first create first a matrix (which is less flexible than a data frame)

matrix1 <- matrix(data = NA,  ncol = 3, nrow = 5) # this creates a matrix with 3 columns (these will be our variables) and 5 rows of data. We will the matrix with NA values (which means 'missing value')
matrix1

df1 <- as.data.frame(matrix1) # this turns the matrix into a data frame. A data frame is more flexible than a matrix, as it can contain different data types and has column names.

df1 # have a look at what you created

# we could also do both steps in one single step: 
df1 <- as.data.frame(matrix(data = NA,  ncol = 3, nrow = 5))

# data frames have column (= variable) names
names(df1)

# here, we give our columns meaningful names
names(df1) <- c("pp_code", "gender", "age")

# the $ after the name of the data frame tells R which variable is meant. in our case, the following command means something like "give me the variable pp_code of the data frame calles df1"
df1$pp_code

# now we replace the NA entries with values (e.g., the participant codes)
df1$pp_code <- c("p1", "p2", "p3", "p4", "p5")
df1$gender <- c("f", "m", "f", "f", "m") # here we do the same with the variable gender
df1 # now, this starts looking like something that makes sense (I hope)

# let's give our participants their ages (it seems this is a study with children...)
df1$age <- c(2, 5, 6, 4, 8)
df1 # let's have a look how it looks so far

df1$age # if we just want to look at the variable age

# if we do indexing with data frames, we have to keep in mind, that a data frame has 2 dimensions!
# the number before the , indexes the rows of the data frame. The number after the , indexes the columns in the data frame.
# If we leave the place before the , blank, it means "all rows"
# If we leave the place after the , blank, it means "all columns"

# what happens in the following commands?
df1[ , 2] # all rows of column 2 are shown
df1[3 , ] # the 3rd row of all columns is shown

df1[1, 2] # what does this?

df1[1, 2:3] # what does this?

df1[1:2, c("age", "gender")] # often, we will not remember whether a variable that we are interested in is column 3 or 4 or whatever. but we more likely will remember its name. thus, we can also index columns with their name

head(df1) # this is a handy command, especially for much larger data frames with many rows. heads() gives the first 6 rows of a data frame

tail(df1) # tail gives the last 6 rows of a data frame. Since our data frame has only 5 rows, head and tail always show all rows in this case

# the command str() for strucutre, is also often handy to get some basic information about a data frame
?str # for the help file

# let's use str() on our data frame
str(df1)

# some explanations
# 'chr' stands for 'character'
# 'num' indicates numeric values

# categorical variables should be factors (rather than character variables)
# let's add a new variable that contains the participant codes and is a factor; let';s call it f_pp_code, as the f_ will remind us that it is a factor
df1$f_pp_code <- as.factor(df1$pp_code)

# we can also look at the summary of a data frame to get some basic information
summary(df1)


# setting the working directory:
# use getwd() to check what the current working directory is
getwd()

# you probably want to change the working directory to a folder that makes more sense. for that, you use setwd()
# In the Dan Goldstein video, he shows you how to do these things on Windows/PC. On a Mac with R, you can simply drag and drop the folder (or file) into the R script window and you get the full path (you need to add the quotation marks etc, but at least you get the whole path).

# so, on my computer, I would use a path like this (yours will be different of course)
setwd("~/surfdrive/Radboud/Teaching/Stats_New_I_II/2025_2026/Classes/Week_01")

# after that, I should check again with getwd()

getwd() # yes, that worked! I get the following output: "/Users/u240148/surfdrive/Radboud/Teaching/Stats_New_I_II/2025_2026/Classes/Week_01"

# how to save a data frame:
# to save it in your working directory, you can just use a short command like this:
write.csv(df1, file = "MyFirstDataFrame_8Sept2025.csv", row.names = FALSE)


# If you want to save it in a place that is different from your working directory, you need to specify the whole file path, for example like this:
write.csv(df1, file = "~/surfdrive/Radboud/Teaching/Stats_New_I_II/2025_2026/Classes/Week_01/slides/Brightspace/MyFirstDataFrame_8Sept2025.csv", row.names = FALSE)


# If we want to load a csv file and put it into a dataframe called data1, you use the command read.csv()
# If the file happens to be in your current working directory, it is sufficient to just specify the name of the file
data1 <- read.csv("MyFirstDataFrame_8Sept2025.csv")

# if the file you want to load is NOT in your current working directory, you need to specify the whole file path (so R knows where you find that file)
data1 <- read.csv("~/surfdrive/Radboud/Teaching/Stats_New_I_II/2025_2026/Classes/Week_01/MyFirstDataFrame_8Sept2025.csv")

# Please remember: specifically on operating systems that are set to use a decimal *comma* (instead of a decimal point), using read.csv leads sometimes to data frames that are not loaded properly (what should be separate variables, i.e., columns are all mushed together in a single column). In these cases, try the command read.csv2(). And sometimes, it is necessary to also specify the "separator" i.e., the character that separates the values in the data file (in regular csv files, the values are separated by commas; but if the comma is used for decimals, then the separator-character will be a different character, for example a ;)

# thus, in these cases, try commands like this:
data1 <- read.csv2("MyFirstDataFrame_8Sept2025.csv")
# after you tried, always check whether it looks correct:
head(data1)

# if it's still doesn't look right, you can try something like
data1 <- read.csv2("MyFirstDataFrame_8Sept2025.csv", sep = ";")
# and check again
head(data1)


# if it still doesn't work:
data1 <- read.csv("MyFirstDataFrame_8Sept2025.csv", sep = ";")
data1
# and check; and so on. until you find out what works for your operating system. Once you have figured that out, I think it should always be the same on your computer.







