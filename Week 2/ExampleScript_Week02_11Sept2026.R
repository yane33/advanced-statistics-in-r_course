# R Example script class Week 2 (Advanced Statistics in R)
# September 2019; updated Sept 2020, Sept 2021, Sept 2022, Sept 2023, Sept 2024, Sept 2025, Sept 2026

# As you will see, this is a rather long example script, covering different things that we did in the class. 

# In part 1, we compute "manually" the variance of the variable Psycho of the parenting data set
# In part 2, we reshape the ratings data frame from wide to long format and back
# In part 3, we select specific rows and columns of a data frame


# Also, at the very end of the script, you see two pieces of code that generate some of the data sets I presented in class, the rating data, and the RT data; feel free to ignore that part, but feel free also to have a look if you're interested; it's pretty easy to generate data in R (and often very useful, if you want to try out things with small data sets).


# perhaps you first need to install some of the packages that I'm loading below (if you don't have them already)
install.packages("reshape") # or, does the same thing: install.packages("reshape", repos='http://cran.us.r-project.org')
install.packages("car") # install.packages("car", repos='http://cran.us.r-project.org'); for the command recode()
install.packages("pastecs") # for descriptive statistics; if this command doesn't work (but it should), you can try this command: install.packages("pastecs", repos='http://cran.us.r-project.org')

# I like to keep all the commands to load the required libraries at the top of the script
library(foreign) # needed to import the SPSS file in Part 1
library(lattice) # needed for the densityplot function in Part 1
library(pastecs) # for stat.desc()
library(psych) # for describe() and describeBy()
library(reshape) # for melt(), cast(), and reshape()
library(car) # for recode() (and, as we will see later during this and the next course, for many other things)

# BTW, the developer of the package reshape created a newer package reshape2, with very similar functionality:
# As the developer himself writes, "This version [reshape2] improves speed at the cost of functionality, so I have renamed it to reshape2 to avoid causing problems for existing users."
# see also here: http://stackoverflow.com/questions/12377334/reshape-vs-reshape2-in-r
# Since we are not concerned about speed when reshaping data, we are going to focus on reshape (not reshape2), as it seems a bit easier and user-friendly.

# you will understand this statement here below only later, but I strongly believe that you should always have it on top of every R script and run it whenever you start R, so to start implementing this habit in you, I include it already here (we don't really need it for what we're doing in this script)
# contrast setting
options(contrasts = c("contr.sum", "contr.poly"))


# I find scientific notation often a bit difficult, so I have an argument here that biases the output towards normal notation
options(scipen = 20)


######################################################################################
# Part 1: 
# let's compute the variance of the variable Psycho of the parenting data set, both as an example how to compute new variables and add them to a data frame, and to learn, well, how to compute a variance yourself (instead of just using a existing command)


# I first set the working directory to the folder where I have stored my parenting.sav data file

setwd("~/Nextcloud/Radboud/Teaching/Stats_New_I_II/2026_2027/Classes/Week_02/Homework")
# I check whether the setting of the working directory worked:
getwd() # yes, it worked because I get the following output:  "/Users/Bernd.Figner/Nextcloud/Radboud/Teaching/Stats_New_I_II/2026_2027/Classes/Week_02/Homework"

# now I load the SPSS file
df_parenting <- read.spss('parenting.sav', to.data.frame = TRUE)

# Let's check whether that worked
head(df_parenting) # looks good
tail(df_parenting) # looks good

# are there any NA values in the Psycho variable?
summary(df_parenting) # no, Psycho does not have any NA entries (the variable Control does, but we are not using that variable, so it does not matter for us here)

# OK, so we want to compute the variance for the variable Psycho. Let's have a look at the distribution (not strictly necessary, but it's always good to know what you are working with)

densityplot(df_parenting$Psycho) # looks at less like a normal-ish distribution (i.e., more or less symmetric and unimodal; perhaps a bit too narrow for a truly normal distribution, not sure though...)

# OK, to compute the variance, we first need to subtract the mean of the variable from each value in the variable

# To do so, we compute a new variable and add it to the data frame; let's call this new variable DiffMean_Psycho
df_parenting$DiffMean_Psycho <- df_parenting$Psycho - mean(df_parenting$Psycho)

# let's check what we created:
head(df_parenting)
df_parenting$DiffMean_Psycho

# to check what we created, we could also look at a densityplot
densityplot(df_parenting$DiffMean_Psycho) # good, the shape is the same, but the distribution is now centered around 0 (BTW, subtracting the mean like we did is also called "centering" -- this is something commonly done, e.g., in linear regression, where the predictors are often centered before we enter them in the model; but we'll talk about this later in the course)

# OK, the next step in computing the variance is squaring the differences; let's call this new variable SQ_DiffMean_Psycho. Squaring is multiplying each number with itself. We can do that with ^2 (^ indicates that we exponentiate)

df_parenting$SQ_DiffMean_Psycho <- df_parenting$DiffMean_Psycho ^ 2

# let's check again what we did:
df_parenting$SQ_DiffMean_Psycho
# and we can also look at the densityplot (for the more visually inclined)
densityplot(df_parenting$SQ_DiffMean_Psycho) # OK, that looks now different (but that was to be expected and we are here not really interested in the shape of the distribution)

# Now, if we sum up all these numbers, then we get the "sum of squares"
# So let's sum them up. Please note that from now on, I am NOT adding these things to the data frame anymore. From now on, we compute only always 1 single number, not a whole column
SumSquares_Psych <- sum(df_parenting$SQ_DiffMean_Psycho)
SumSquares_Psych # I get 16215.12

# now, if we divide this sum of squares by the number of observations, we get the variance. How can we get the number of observations? Well, if we don't have any NAs (and we don't), then we can use the command length(), which tells us the length of a vector
n_Psycho <- length(df_parenting$SQ_DiffMean_Psycho)
n_Psycho # 369

# OK, so the variance should be:
VAR_Psycho <- SumSquares_Psych / n_Psycho
VAR_Psycho # 43.94342

# We could compute it also like this, of course:
VAR_Psycho <- sum(df_parenting$SQ_DiffMean_Psycho) / length(df_parenting$SQ_DiffMean_Psycho) # that gives the same number: 43.94342

# OK, so we can compare that now with the command var() that exists in R
var(df_parenting$Psycho) # 44.06283; that gives a slightly number; why is that?

# Well, if you read the book carefully (and I am sure you did), then you know that there is a difference between the *descriptive* variance and the *inferential* variance.
# What we computed ourselves was the descriptive variance. What the var() command computes is the inferential variance (the inferential variance is an estimate of the population variance)

# The difference between the two is that the inferential variance does not divide the sum of squares by the number of observations, but it divides by the number of observations minus 1.

# so let's compute the inferential variance ourselves, then:
VAR_Psycho_Inf <- SumSquares_Psych / (n_Psycho - 1)
VAR_Psycho_Inf # 44.06283; yes, that's the same as what we get with var()!

# And if we wanted to compute the standard deviation of Psycho, we just need to compute the square root of the variance
sqrt(VAR_Psycho_Inf) #6.637984 for the inferential SD

# compare that to the sd() command
sd(df_parenting$Psycho) # yes, that's the same: 6.637984



#################################################################################
# Part 2: reshaping a data frame from wide to long and back
######################################################################################

# for the example here, we need some data
# such as: 20 participants, each rates their moood 3 times: baseline measure, then after watching a funny movie clip, and then again after watching a sad movie clip. the mood ratings are done on a continuous visual analogue scale ranging from 0 to 100

# we could either load a file I made or generate some data ourselves, either is fine
# here, I proceed with a data file that I created earlier and uploaded for you on Brightspace; if you are interested to find out how I created this data file, scroll down all the way towards the end of the script; there I have a section with the code I used to generate the data

# option 1: load a data file that I have created previously
# depending on where you have the file, you perhaps have to adjust the working directory using setwd() or you can also load the file using the whole file path, of course
setwd('~/Nextcloud/Radboud/Teaching/Stats_New_I_II/2026_2027/Classes/Week_02/')
getwd() # good!
r_0 <- read.csv("Rating_ExampleData.csv") 
head(r_0)
tail(r_0)

# look at the whole file:
r_0

# ok, so that looks good.

# let's check some more:
str(r_0)
summary(r_0)
# from both outputs, it looks like f_gender is treated as a character variable not as a factor, so let's turn it into a factor. this is a case where I overwrite the variable (instead of adding a new variable)

# but first: often, for pure safety reasons, I like to put my data frame into a new object; in case something goes wrong, I still can easily go back to the previous data frame; so here, I would do it like this:

r_1 <- r_0 # this simply copies the data frame r_0 into a new data frame r_1; if I do something stupid to r_1, I can still simply go back to r_0 (well, in this specific example here, I could also simply load again the csv file; but sometimes, there are many many steps between loading the csv file and creating a final version of the data frame, then it's nice not to start all the way at the beginning, i.e., loading the data frame)

# now let's turn f_gender into a factor
r_1$f_gender <- as.factor(r_1$f_gender)

# let's check again
summary(r_1)
str(r_1)
# good now!

# ok, so now let's do this: RESHAPING THE DATA FRAME: wide versus long format
# our data frame r_1 is in so-called wide format; repeated measures (rating_1, rating_2, rating_3) are separate columns. This is the format you are familiar with also from SPSS

# For many types of analyses in R (and for some analyses even in SPSS, actually), we need the data to be in LONG format; this is also sometimes called "stacked" format, because the repeated measures are 'stacked' on top of each other

# there are different ways how to get from wide to long format and back, ranging from simple (but not very flexible) to complex (but very flexible):
# stack() and unstack() [these commands are in the base package, i.e., available without loading any additional packages, if I am not mistaken]. I don't recommend using it, but I still want to show it briefly for the curious
# cast() and melt() [from the package reshape]
# reshape() [also from the package reshape]
# I typically use reshape(), but the other commands are a bit easier (but not always flexible enough)

# so, let's try the most simple, i.e., stack/unstack first
?stack # if you get a choice, you need the 'stack/unstack' help from "utils"

r_1_stack <- stack(r_1, select = c('rating_1', 'rating_2', 'rating_3'))

#let's check what we created:
r_1_stack

# the problem here is that we lose the information about which data are from which participant... so that's not handy for our case of repeated measures. It is a fine and simple command, if all one wants to do is indeed to put the variables on top of each other; and vice versa:
r_1_unstack <- unstack(r_1_stack)
r_1_unstack # yep, it's back in wide format, but as one could expect, the information about participants (and gender) is gone forever...





# on to cast/melt then: these commands are in the package reshape, so we need to install and load it first (I do this at the very top of this script!)

?melt # this is an extremely short and not very helpful help file... 
?melt.data.frame # this is a bit longer, but still not that fantastic

r_1_melt <- melt(r_1, id = c('pp_code', 'f_gender'), measured = c('rating_1', 'rating_2', 'rating_3'))

r_1_melt
# that worked very nicely!

# if we wanted, we could change the names of the columns "variable" and "value" but I'm not going to do this here
# so here's the basic syntax for melt:
# under measured =, we tell the function melt which variables form the repeated measures and need to be stacked on top of each other
# under id =, we specify the variables that are not the repeated measures and thus need to be copied several times: i.e., pp_1 and their gender need to be put into the long-format data frame 3 times, once for each of the 3 rating variables


# now let's try to undo that again:
r_1_cast <- cast(r_1_melt)
r_1_cast
# that worked fine as well!

# well, it doesn't always work that easily. for more complicated cases, the syntax can require some more details and specifications of variables
?cast

r_1_cast2 <- cast(r_1_melt, pp_code ~  variable) # left of the ~ is the variable that identified in the molten (=stacked) data frame which observations belong to the same participant; right of the ~ is the variable that identifies the new columns that will be created

r_1_cast2 # yep, looks good, but now we lost the gender information!

# so, if we want to keep the gender information, we do this:
r_1_cast3 <- cast(r_1_melt, pp_code + f_gender ~ variable)

r_1_cast3
# Looks good. So we tell cast() that the variables pp_code and gender are id variables (not repeated measurements) and, after the tilde (~), we specify which variable is the repeated-measures column



r_1_cast
r_1
# just to compare: the only difference between r_1_cast and r_1 seems to be how the rows are ordered:
# in r_1 it's pp_1, pp_2 etc
# in r_1_cast, the data have been sorted alphabetically, i.e., pp_1, pp_10, pp_11 etc
# but the sorting doesn't matter here (BTW: if you need to sort a data frame, you must NOT use the sort() command, as this can have the effect that only one column is sorted in your data frame and the others not, turning your data frame into a mess...)

# see for example here how to order the rows in a data frame: http://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns-in-r

# here I sort the rows according to the values in rating_1
r_1_ordered <- r_1[order(r_1[,3]),]

# this command looks more complicated than it is, as it combines several things
# in the middle is this: order(r_1[,3]) --> this says order r_1 according to the 3rd column in r_1
# this is then put into r_1[,] where the row index goes, resulting in the full command:
# r_1[order(r_1[,3]),]

# how should the command look like if you wanted to order the data frame according to the variable rating_3, but in descending order (i.e., highest values first)? HINT: have a look at ?order
# I paste the correct command at the very end of this script


# so, this cast command worked because the some of the variable names were created by the melt command: the variable indicating whether it is rating 1, 2, or 3 was named "variable" and variable with the actual rating values was called "value"

# But what if our variables in our data frame had different names? Then we need to specify additional arguments in the cast command.
# So, to show you how this works:

r_1_melt_newnames <- r_1_melt

# let's change the names of the last two variables:
names(r_1_melt_newnames)[3:4] <- c("rating_1or2or3", "rating")

head(r_1_melt_newnames)


# ok, so what happens if I try to simply use cast:

cast_attempt1 <- cast(r_1_melt_newnames)
# I get the following error: Error: Casting formula contains variables not found in molten data: variable
# the command cast() expects that one of the columns in the data frame is called "variable"

# so let's try the command which is a bit more complex:
cast_attempt1 <- cast(r_1_melt_newnames, pp_code + f_gender ~ rating_1or2or3)
# now it does something, but it also gives me a message:
# Using rating as value column.  Use the value argument to cast to override this choice
# OK, so the cast command made an educated guess, namely the it should use the variable "rating" as the repeated-measures variable. That was a correct guess, in our case. But it also tells us that we could change this.

# Well, if we don't want poor cast() to make guesses, we can specify the whole thing like this (for reasons that are not clear to me, this variable has to be in quotation marks):
 
cast_attempt1 <- cast(r_1_melt_newnames, pp_code + f_gender ~ rating_1or2or3, value = "rating")
cast_attempt1
# ok, that worked fine and we didn't get any message




# Ok, but back to reshaping
# now let's use the most complicated (and most flexible) command, reshape()
?reshape # ok, this help file is a bit longer...


r_1_long <- reshape(r_1, idvar = 'pp_code', varying = c('rating_1', 'rating_2', 'rating_3'), timevar = 'rating_1or2or3', v.names = 'rating', direction = 'long')

r_1_long # check: looks good!

# so, how does this function work?
# similar to before, with idvar = , we specify the ID variable. Note that we did not have to specify f_gender here, but it was still included in the new long format data frame
# with varying =, we specify the repeated-measures variables
# with timevar =, we give the name to a new variable that will be created, which tells us from which variable this row of data comes (rating 1, 2, or 3 in our case)
# with v.names =, we can specify the name that our new stacked rating variable should have
# with direction =, we tell the function whether we want to go from wide to LONG format (as we did here); or the other way around from long to WIDE

# If we want to go back from the long to the wide format:
r_1_wide <- reshape(r_1_long, direction = 'wide')

r_1_wide # looks good


# again, we can specify more arguments, if we have to

r_1_long_newnames <- r_1_long

names(r_1_long_newnames) <- c("SubjCode", "Sex", "WhichRating", "dependentvariable")

# so, this is a bit complicated, but we have to give the following arguments:
# idvar specifies the ID variables, i.e., the variables that don't change across repeated measures, such as participant code or gender etc
# v.names specifies the repeated-measures variables; reshape will give these columns the names of what we specify here, followed by a dot and a number
# timevar: this is the variable that specifies whether it's per participant the first, second, or third repeated-measures observation
r_1_wide_test1 <- reshape(r_1_long_newnames , idvar = c("SubjCode", "Sex"), v.names = "dependentvariable", timevar = "WhichRating", direction = "wide")

r_1_wide_test1 # ok, this looks good



######## end of going from wide to long format and from long to wide format!
#######################################################################################



##########################################################################################
# Part 3: selecting only parts of a data frame: selecting observations (=rows) and/or variables (=columns)
?subset # if you get a choice between different help files (I do, but I might have more packages installed than you), we need the one from the base package. the following also works:
?base::subset # the base:: before subset means something like "the command subset *from the package base*"

r_1_melt

r_1_melt_f <- subset(r_1_melt, f_gender == "female")
r_1_melt_f # ok, that worked

# BTW, we can achieve the same in this example by selecting all the observations in which f_gender is unequal to male

r_1_melt_f <- subset(r_1_melt, f_gender != "male")


# let's say we want all the observations, but only the variables of pp_code and f_gender (this is probably something we wouldn't want to do; but let's say we had a data frame with hundreds of variables and we want to create a smaller data frame with only a few variables, then this would make sense)

r_1_melt_ppg <- subset(r_1_melt_f, select = c("pp_code", "f_gender"))

r_1_melt_ppg <- subset(r_1_melt_f, select = c(pp_code, f_gender)) # that works, too. pp_code and f_gender are variable names, so it seems this works either way


# and we can combine selecting rows and selecting columns

r_1_melt_small <- subset(r_1_melt, variable != "rating_2", select = -f_gender)
# Note: here, putting f_gender in quotation marks DOESN'T work!
# Note: removing the quotation marks from rating_2 doesn't work! ("rating_2" is a specific value in the variable called variable, so we need the quotation marks in this case; ultimately, this doesn's always seem to be super-logic, but sometimes needs a little bit of trial and error...)


# droplevels() is a command that removes all the unused levels of a factor, so I always use it when I use subset or some other command to remove some levels of a factor

r_1_melt_f <- subset(r_1_melt, f_gender == "female")


r_1_melt_f$f_gender # as we can see, the level "male" is still retained in the Levels: part of the output we see, although it doesn't occur in the data frame!

# #  [1] female female female female female female female female female
# [10] female female female female female female female female female
# [19] female female female female female female female female female
# [28] female female female
# Levels: female male

levels(r_1_melt_f$f_gender) # this here shows the same thing, male seems to be still a level


# if we add droplevels()
r_1_melt_f <- droplevels(subset(r_1_melt, f_gender == "female"))
r_1_melt_f$f_gender
levels(r_1_melt_f$f_gender)
# now, we see that "male" is gone





##################################################################################################################################
# Generating the data for the "repeated-measures" rating data

# there are (as always) different ways how to generate your own data; here I show you one way, using a bunch of R commands: this is only for the interested, you can skip this part if you want

# The following lines of code create some 'fake' data; you can ignore that bit, if you prefer
r_1 <- as.data.frame(matrix(data = NA, nrow = 20, ncol = 5)) # you already know that from week 1: we create a matrix filled with NA values and turn that matrix into a data frame

names(r_1) <- c('pp_code', 'f_gender', 'rating_1', 'rating_2', 'rating_3') # now we give the variables some meaningful names

r_1$pp_code <- paste('pp', c(1:20), sep = '_') # I like to use participant codes that are NOT numbers (to make sure I never accidentally treat it as continuous variable)


# for gender, I use the sample() command; it randomly samples elements from a vector that I define

# There's different ways to do this, depending on what your goal is

# Option 1:
r_1$f_gender <- as.factor(sample(c('male', 'female'), 20, replace = TRUE)) # what this does is that it randomly assigns the gender female vs male (with 50% probability) to the participants so that approximately half the participants will have the gender female and approximately half of the participants will have the gender male

# You can quickly check the distribution
table(r_1$f_gender)
# and the order of the entries
r_1$f_gender # order looks pretty random


# Now, if you want to make sure that *exactly* half the participants are assigned female and male, then there's two other options (well, there's probably more, but I am aware of these two at least)

# Option 2
r_1$f_gender <- as.factor(sample(c('male', 'female'), replace = FALSE)) # This way here you obtain exactly half the participants male and half female, but it won't be very random, as female and male will always alternative from row to row.
table(r_1$f_gender) # yes, exactly half and half
r_1$f_gender # in my case it's always female then male, and this repeats, but I think whether it begins with male or female is random

# Option 3: If you want exactly half and half *and* random order, then the following command is the right one:
r_1$f_gender <- as.factor(sample(c(rep('male', 10), rep('female', 10)), replace = FALSE))
r_1$f_gender # order looks random
table(r_1$f_gender) # exactly half and half
# With this command, we create a "pool" of 10 male labels and 10 female labels and then we draw randomly from those 20 without replacement



# if you're still curious, have a look at:
?sample


# creating some fake data; I assume the rating scale goes from 0 to 100 and I generate some normally distributed data with a mean of 50 and a SD on 20 and round it to integers (i.e., no decimals)
r_1$rating_1 <- round(rnorm(20, mean = 50, sd = 20), digits = 0)

# Note: here I use the round() command
# the general syntax is like this: round(myvariable, digits = 2)
# with digits = I indicate how many digits after the comma I want to have

densityplot(r_1$rating_1) # just to check the distribution (since I said the ratings go from 0 to 100, I should make sure that there are no values smaller than 0 or larger than 100)

# I next create a second rating that should be on average higher than the first one (that's what the +20 does in the command below) and is correlated (but not perfectly correlated) with the first measure: that's what the + round(rnorm(20, mean = 0, sd = 3), digits = 0) does; it adds a random number to the previous rating (the random number is drawn from a normal distribution with a mean of 0 and an SD of 3; thus, sometimes the random number will be smaller, sometimes larger than 0)
r_1$rating_2 <- r_1$rating_1 + 20 + round(rnorm(20, mean = 0, sd = 3), digits = 0)

densityplot(r_1$rating_2) # let's check again

# ok, there are some that are above 100, so let's fix that

r_1$rating_2[which(r_1$rating_2 > 100)] <- 100

# and for the third rating, we create a average lower rating (thus, I subtract 30 from the first rating and again do the adding of a random number)
r_1$rating_3 <- r_1$rating_1 - 30 + round(rnorm(20, mean = 0, sd = 3), digits = 0)

densityplot(r_1$rating_3) # let's check

# that generated a few values below 0, so I change these to 0: I identify the entries below 0 using which() and assign the value 0 to these entries
r_1$rating_3[which(r_1$rating_3 < 0)] <- 0

densityplot(r_1$rating_3) # ok, no more values below 0


# save this fake data file
write.csv(r_1, file = 'Rating_ExampleData.csv', row.names = FALSE)

# this is the end of the part in which I created the rating data



#................................................

# solution sorting of data frame according to values in rating_3 in descending order:
r_1[order(r_1[,5], decreasing = TRUE),]



