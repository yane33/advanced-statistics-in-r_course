# R Example script class Week 3 (Advanced Statistics in R)
# September 2019; updated Sept 2020, Sept 2021, Sept 2022; Sept 2024; Sept 2025; Sept 2026

# This lecture example R script contains the things both for the data management part of the class and the ggplot2 part of the class

# 
# In part 1, we select specific rows and columns of a data frame using the command which [in week 2, we already learned other relevant methods/commands/arguments etc such as subset, ==, !=, and droplevels -- if you want to check again how these things work, please have a look at the example script from week 2!]
# In part 2, we identify and remove missing values
# In part 3, we recode some variables
# Part 4 is then about visualizing data using qplot() and ggplot() from the package ggplot2



# At the very end of the script, you see code that generates the RT data set with the missing data that I presented in class; feel free to ignore that part, but feel free also to have a look if you're interested; it's pretty easy to generate data in R (and often very useful, if you want to try out things with small data sets).



# I like to keep all the commands to load the required libraries at the top of the script
library(foreign) # needed to import the SPSS file in Part 1
library(lattice) # needed for the densityplot function in Part 1
library(pastecs) # for stat.desc()
library(psych) # for describe() and describeBy()
library(reshape) # for melt(), cast(), and reshape()
library(car) # for recode() (and, as we will see later during this and the next course, for many other things)


# you will understand this statement here below only later, but I strongly believe that you should always have it on top of every R script and run it whenever you start R, so to start implementing this habit in you, I include it already here (we don't really need it for what we're doing in this script)
# contrast setting
options(contrasts = c("contr.sum", "contr.poly"))


# I find scientific notation often a bit difficult, so I have an argument here that biases the output towards normal notation
options(scipen = 20)

# let's set the working directory
setwd("~/Nextcloud/Radboud/Teaching/Stats_New_I_II/2026_2027/Classes/Week_03/")
# I check whether the setting of the working directory worked:
getwd() # yes, it worked 


######################################################################################
# Part 1: selecting only parts of a data frame: selecting observations (=rows) and/or variables (=columns)
# In week 2, we learned how to use subset(), this week, we'll look at how we can use the very handy command which() for this (and for other things)
##########################################################################################


# first let;s have a general look at what which() does
?which

a <- c(49, 63, 49, 100, 20)

which(a == 49) # 1, 3

which(a != 100) # 1, 2, 3, 5

which(a > 60) # 2, 4

#which() can be used to also select observations; here, I am using the data frame r_1_melt that I created in last week's example script (so, if you don't have that data frame loaded, please see the example script of week 2)
r_1_melt_m <- droplevels(r_1_melt[which(r_1_melt$f_gender == "male"),])




#######################################################################################################
# Part 2. Identifying and removing missing values
# Missing values are "NA" in R

# here's an example of a data frame with missing values (if you are interested in how I created it, you can scroll all the way down to the end of this script, then a little bit up again)

t_1 <- read.csv("ExampleData_RT_with_Missings.csv") 

# One helpful command to identify NAs (and in general lots of descriptive statistics) is the command stat.desc() from the package pastecs
stat.desc(t_1, desc= FALSE) # that's ok, but it seems I don't get something helpful for the pp_code variable (so perhaps this works mostly for numeric variables)

# one helpful quick check whether there might be NA entries is also the command summary()
summary(t_1) # this tells me that there's 1 NA in the variable age, 1 NA in gender, and 2 NAs in RT

# the command is.na() asks whether an entry is an NA or not; since this is a logical operator, it gives TRUE or FALSE as answer
is.na(t_1)

is.na(t_1$pp_code) # this gives as many TRUE/FALSE entries as the variable is long

unique(is.na(t_1$pp_code)) # this gives only the unique values; here, since they are all FALSE, this gives only a single FALSE, which tells us that there are no missing values

# when we do the same for RT
is.na(t_1$RT)
unique(is.na(t_1$RT)) 


# if we want to know which ones are NA
which(is.na(t_1$RT) == TRUE) # the 2nd and 6th row


# if we want to create a data frame from which the rows with NA in the variable RT have been removed:
t_2 <- t_1[-which(is.na(t_1$RT) == TRUE), ]

# or, the same (no minus sign before the which, and FALSE instead of TRUE):
t_2 <- t_1[which(is.na(t_1$RT) == FALSE), ]


# if we want a data frame that has every row removed with any NA

t_3 <- na.omit(t_1)

# complete.cases is somewhat similar to is.na, but checks for any NAs per row
complete.cases(t_1)
#  [1]  TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE
# so the first TRUE means that the first row of data is complete (i.e., doesn't have any NA), the second entry is FALSE and means that there is at least on NA on this row
t_1[complete.cases(t_1) == TRUE,] # that gives us then also just the rows without any NAs

t_1[complete.cases(t_1),] # we can also write it like this, without the == TRUE



##########################################################################################
# Part 3. recoding data
##########################################################################################

# in the t_1 data frame, gender is coded with 1 and 2 instead of "male" and "female"
# as usual, there are many ways how to change this, but here's one command that I find very useful for many instances: recode()

# this command comes from the package car (which is a very handy package, mostly related to linear regression, but with other helpful commands as well)
# perhaps you need to first load the package car:
library(car)
?recode

# so, our goal is to create a new variable f_gender that (i) is a factor and (ii) in which "male" replaces the 1 from the original gender variable and "female" replaces the 2
t_1$f_gender <- as.factor(recode(t_1$gender, "1 = 'male'; 2 = 'female'"))

# check what we created:
t_1$f_gender # good, it's a factor; we can also verify it with str()

str(t_1$f_gender)


# age is a continuous variable, but let's say we want to create age categories
t_1$age # age ranges from 7 to 20; let's say we want to create a factor with the categories "child" (up to an age of 12), "teenager" (13 to 19), and "adult" (20 and older)

t_1$f_agegroup <- as.factor(recode(t_1$age, "0:12 = 'child'; 13:19 = 'teenager'; 20:99 = 'adult'"))

# if we want to have a factor with only two levels, indicating whether a participant is a teenager or not, we could do this like this:
t_1$f_TeenagerOrNot <- recode(t_1$f_agegroup, "'child' = 'noteenager'; 'teenager' = 'teenager';  'adult' = 'noteenager'")


# note: I also use single quotation marks for the new categories 'noteenager' and 'teenager'
# since f_agegroup was already a factor, the new variable will also be a factor 


# recode can also be used to recode outliers
# let's try that for t_1$RT
# for the sake of this example here, let's say for this specific task that was used to collect the data, the field agrees that RTs below 150 msec and RTs above 700 msec are outlier. These cutoffs are here completely arbitrary, just to explain how this works in principle. There are other definitions used often for outliers, such as based on the mean and SD or based on the median and the median absolute deviation. We'll talk about these things later, but if you are curious, here's a paper that makes a convincing argument (at least in my view) why using a outlier criterion based on the median and the so-called median absolute deviation (MAD) makes more sense than a criterion based on mean +/- SD: https://www.sciencedirect.com/science/article/pii/S0022103113000668




# Just for fun, let's first calculate the mean RT when the outliers are included
mean(t_1$RT) # I get NA! well, R is strict in that, strictly speaking, if there is an NA, you cannot really compute a mean (nor other descriptive statistics)
# but you can tell R to be nice and ignore the NA entries to compute the mean

mean(t_1$RT, na.rm = TRUE) # 341.6965

# perhaps we are also curious about the SD
sd(t_1$RT, na.rm = TRUE) # 249.8177

# OK, so let's now create a new variable in which the outliers (0-49 msec and 1001 and higher are treated as outliers, i.e., replaced with NA values)
# note that we can use lo and hi in the command: lo means whichever lowest value there is in this variable; hi means whichever highest value there is
t_1$RT_noOutliers <- recode(t_1$RT, "lo:149 = NA; 751:hi = NA") 

# OK, so just for fun and comparison, let's see how the mean and SD are now different compared to the variable with the outliers
mean(t_1$RT_noOutliers, na.rm = TRUE) # 390.9027; OK, the mean is a bit higher, so I guess we have removed more short than long RTs
sd(t_1$RT_noOutliers, na.rm = TRUE) # 172.5642; the SD is quite a bit lower than before, no surprise since we removed the extreme ends of the distribution and made it therefore a more narrow distribution


# Just in case you are curious how a mean + SD based strategy might work to remove outliers (again, while this is quite common, arguments have been made why this is probably not such a great strategy)
# ok, so here I compute a cutoff: what is the value mean plus 2 SDs (sometimes, mean + 3 * SD are used as well or 2.5 SDs or, arguably the best strategy but a bit advanced for now, based on the median and the median absolute deviation)

# I'm starting with the variable without outliers
mean(t_1$RT, na.rm = TRUE) + 2 * sd(t_1$RT, na.rm = TRUE) # 841.3319; that's our high cutoff (we could also do the same for the short cutoff, but it would give a negative number as cutoff and since there are no negative RTs, this doesn't make sense here, or is not necessary); well, see below:

# low cutoff: that gives a negative number, and since there are not negative RTs, this low cutoff does not need to be applied in our example
mean(t_1$RT, na.rm = TRUE) - 2 * sd(t_1$RT, na.rm = TRUE) # -157.9389; RTs shorter than minus 157.9 msec would be considered fast outliers, but of course negative RTs don't exist...


t_1$RT_noOutliers2 <- recode(t_1$RT, "841.34:hi = NA")


# And, lastly, a method based on the median +/- MAD (which is probably a better strategy than mean +/- SD because mean and SD are themselves influenced by outliers)

# we first compute the MAD of our variable of interest, using the function mad() (this is from the base package Stats, i.e., no need to load any package to access this function):
mad(t_1$RT, na.rm = TRUE) # 192.5237

# we also compute the median of the variable of interest
median(t_1$RT, na.rm = TRUE) # 251.6631

# In the paper, they propose to use as outlier criterion the cutoff of "median plus/minus 2.5 x the mad"; thus:
#high cutoff:
mad(t_1$RT, na.rm = TRUE) + 2.5 * median(t_1$RT, na.rm = TRUE) # 821.6815


#low cutoff:
mad(t_1$RT, na.rm = TRUE) - 2.5 * median(t_1$RT, na.rm = TRUE) # -436.6342 --> negative number, so nothing to be excluded on the lower end

t_1$RT_noOutliers3 <- recode(t_1$RT, "821.6815:hi = NA")





##########################################################################################
# Part 4. data visualization using the package ggplot2
##########################################################################################

# the slides of this data visualization part contain all the relevant R code always next to (or in very close close proximity, i.e., the previous slide or so) the respective figure, so I don't really have to add much more to it. For this reason, I simply copy/paste the relevant code here, without any/much explanation; I hope that works. If you would want more explanations, please let me know, then I can still add that.
# Please note that in addition to the code for the figures in the slide, I added here in the script sometimes little variations or additional figures that are not part of the slides.

# install and load the relevant package
install.packages("ggplot2")
library(ggplot2)

# creating a bunch of relatively simple qplots that I show on the various slides
qplot(carat, price, data = diamonds)

qplot(log(carat), log(price), data = diamonds)

# The following is not in the slides, but it shows that you can put simple formulas (like computing the volume with x * y * z) in the qplot command itself
qplot(carat, x*y*z, data = diamonds)

set.seed(100) # setting the random number generator to a specific starting point

dsmall <- diamonds[sample(nrow(diamonds), 100), ]

qplot(carat, price, data = dsmall, shape = cut)

qplot(carat, price, data = dsmall, color = cut)

qplot(carat, price, data = dsmall, size = cut)

# make the figure somewhat transparent (0.1) or even more strongly transparent (0.01)
qplot(carat, price, data = diamonds, alpha = I(1/10))
qplot(carat, price, data = diamonds, alpha = I(0.1))

qplot(carat, price, data = diamonds, alpha = I(1/100))
qplot(carat, price, data = diamonds, alpha = I(0.01))

qplot(carat, price, data = diamonds, alpha = I(1/10), color = "red")

qplot(carat, price, data = dsmall, geom = c("point", "smooth"))

qplot(carat, price, data = dsmall, geom = c("point", "smooth"), se = FALSE) # gives a warning, but still works

qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "lm")

# things can be even more complicated, by specifying the regression formula with formula = (here, it's even a polynomial regression formula; don't worry if you don't understand this, we'll talk about these things like linear and quadratic effects only later in the course)
qplot(carat, price, data = dsmall, geom = c("point", "smooth"), method = "lm", formula = y ~ poly(x, 2))

qplot(color, price/carat, data = diamonds, alpha = I(1/50))

qplot(color, price/carat, data = diamonds, geom = "jitter", alpha = I(1/50))

qplot(color, price/carat, data = diamonds, geom = "boxplot")

qplot(carat, data = diamonds, geom = "histogram")

qplot(carat, data = diamonds, geom = "histogram", xlim = c(0, 2))

qplot(carat, data = diamonds, geom = "histogram", binwidth = 1, xlim = c(0, 4))


qplot(carat, data = diamonds, geom = "histogram", binwidth = 0.1, xlim = c(0, 3))

qplot(carat, data = diamonds, geom = "density")

qplot(carat, data = diamonds, geom = "density", color = color)

qplot(carat, data = diamonds, geom = "histogram", fill = color)

qplot(color, data = diamonds, geom = "bar")



# for the next few plots, we use the diamonds data set (we have encountered that already in week 2)
qplot(carat, data = diamonds, facets = .~cut, geom = "histogram", binwidth = 0.5, xlim = c(0, 3))

p <- ggplot(diamonds, aes(x = carat, y = price))
p <- p + geom_point(aes(color = cut))
p

ggsave("plot.pdf", p) # save the plot as a pdf file in the working directory, so that one can later insert it into a word or powerpoint file, etc


p <- ggplot(diamonds, aes(x = carat, y = price))
p <- p + geom_point(alpha = 0.1) 
p <- p + geom_smooth(method = 'lm', aes(color = cut, fill = cut), alpha = 0.1)
p

vp <- ggplot(diamonds[1:30000,], aes(x = cut, y = price))
vp + geom_violin() + geom_boxplot(width = 0.1, alpha = 0.1)

vp + geom_violin() + geom_jitter(width = 0.1, alpha = 0.02, color = "blue")


# As mentioned in the slides, so-called Raincloud plots have become quite popular. There are many different tutorials and tips and even papers how to create them; I haven't yet used them much, so I could not tell you with high confidence what is the best to use. A fellow student in 2023/2024 recommended the package ggrain and I found also that this works quite well and they also have nice documentation:
# here is a link to their vignette (which is a bit like a tutorial): https://www.njudd.com/raincloud-ggrain/

# Here are some other resources (same or different package), so you can have a look for yourself:
# https://github.com/jorvlan/raincloudplots
# https://wellcomeopenresearch.org/articles/4-63/v2
# https://z3tt.github.io/Rainclouds/
# https://www.cedricscherer.com/2021/06/06/visualizing-distributions-with-raincloud-plots-and-how-to-create-them-with-ggplot2/

# Well, so here is what I used for the figure on my slides:
install.packages('ggrain')
library(ggrain)
# Let’s again plot price as a function of cut:

rp1 <- ggplot(diamonds[1:30000,], aes(x = cut, y = price, fill = cut))
rp2 <- rp1 + geom_rain(alpha = 0.25)
rp2

# And for the second raincloud plot (the one with the repeated-measures), I used mostly code from the ggrain web page and adjusted things at the end of the code a bit (to show just 2 groups instead of a plot for a 2 x 2 design):
# R code for the figure on the previous slide adjusted from here: https://www.njudd.com/raincloud-ggrain/
set.seed(42) # the magic number

iris_subset <- iris[iris$Species %in% c('versicolor', 'virginica'),]

iris.long <- cbind(rbind(iris_subset, iris_subset, iris_subset), 
                   data.frame(time = c(rep("t1", dim(iris_subset)[1]), rep("t2", dim(iris_subset)[1]), rep("t3", dim(iris_subset)[1])),
                              id = c(rep(1:dim(iris_subset)[1]), rep(1:dim(iris_subset)[1]), rep(1:dim(iris_subset)[1]))))

# adding .5 and some noise to the versicolor species in t2
iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t2"] <- iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t2"] + .5 + rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t2"]), sd = .2)
# adding .8 and some noise to the versicolor species in t3
iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t3"] <- iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t3"] + .8 + rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'versicolor' & iris.long$time == "t3"]), sd = .2)

# now we subtract -.2 and some noise to the virginica species
iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t2"] <- iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t2"] - .2 + rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t2"]), sd = .2)

# now we subtract -.4 and some noise to the virginica species
iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t3"] <- iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t3"] - .4 + rnorm(length(iris.long$Sepal.Width[iris.long$Species == 'virginica' & iris.long$time == "t3"]), sd = .2)

iris.long$Sepal.Width <- round(iris.long$Sepal.Width, 1) # rounding Sepal.Width so t2 data is on the same resolution
iris.long$time <- factor(iris.long$time, levels = c('t1', 't2', 't3'))

ggplot(iris.long[iris.long$time %in% c('t1', 't2'),], aes(time, Sepal.Width, fill = Species)) +
  geom_rain(alpha = .5, rain.side = 'f2x2', id.long.var = "id") +
  theme_classic() +
  scale_fill_manual(values=c("dodgerblue", "darkorange")) +
  guides(fill = 'none', color = 'none')

# for the actually plot shown on the slide, I (Bernd) made it simpler:
ggplot(iris.long[iris.long$time %in% c('t1', 't2'),], aes(time, Sepal.Width, fill = time)) +
  geom_rain(alpha = .5, , rain.side = 'f1x1', id.long.var = "id")




# for the next few plots, we use the data set ggplot2movies; since recently, it is its own package (it used to be part of ggplot2, but perhaps has become too big?)
install.packages("ggplot2movies")
library(ggplot2movies)
? ggplot2movies # that doesn't work, it seems...

movies$f_Documentary <- factor(movies$Documentary, labels = c('Other', 'Documentary'))
p <- ggplot(movies, aes(f_Documentary, rating)) 
p <- p + stat_summary(fun = mean, geom = "bar", fill = 'white', color = 'black')
p

msmall <- movies[sample(nrow(movies), 100),]

msmall$f_Documentary <- factor(msmall$Documentary, labels=c('Other', 'Documentary'))
p2 <- ggplot(msmall, aes(f_Documentary, rating)) + stat_summary(fun = mean, geom = "bar", fill = 'white', color = 'black')
p2 <- p2 + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)
p2


msmall$old <- ifelse(msmall$year < 1980, "old", "new")
p3 <- ggplot(msmall, aes(f_Documentary, rating)) + stat_summary(fun = mean, geom = "bar", aes(fill = old), color = 'black')
p3


msmall$old <- ifelse(msmall$year < 1980, "old", "new")
p3 <- ggplot(msmall, aes(f_Documentary, rating)) + stat_summary(fun = mean, geom = "bar", aes(fill = old), position = 'dodge')
p3


msmall$old <- ifelse(msmall$year < 1980, "old", "new")
p3 <- ggplot(msmall, aes(f_Documentary, rating, fill = old)) + stat_summary(fun = mean, geom = "bar", position = 'dodge')
p3 <- p3 + stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width = 0.9), width = .1)
p3

p4 <- p3 + labs(x = "Documentary", y = "Average Rating", fill = "Movie Age")
p4 <- p4 + scale_fill_manual(values = c("red", "blue"))
p4


# adding a theme to a figure
p3 <- p3 + theme_bw()
p3
?theme

# there are more themes out there; quite a few of them are in the package ggthemes
install.packages("ggthemes")
library(ggthemes)
p3 <- p3 + scale_fill_wsj("black_green", "") + theme_wsj()
p3

?theme_wsj
?scale_fill_wsj


# Please note that the APA theme is NOT part of the package ggthemes, as far as I know. There are, however, at least two other packages that contain a theme called APA:
# Both the package jtools and the package papaja each contain a theme called theme_apa(). Feel free to try them out (I have to admit that I have not yet tried them out).

# see links to some documentation here:

# jtools: https://rdrr.io/cran/jtools/man/theme_apa.html
# papaja: https://www.rdocumentation.org/packages/papaja/versions/0.1.0.9997/topics/theme_apa


# More/other information, these are links that I have on the slides

# related to ggthemes and similar things
https://github.com/jrnold/ggthemes
http://cran.r-project.org/web/packages/ggthemes/ggthemes.pdf
https://yutannihilation.github.io/allYourFigureAreBelongToUs/

# related to the tidyverse (of which ggplot2 is a part of, I think it was even the start of this while tidyverse thing?)
http://ggplot2.tidyverse.org/reference/
http://ggplot2.tidyverse.org/

# Many many colors that can be used in R
http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
 








#####################################################################################################
# Generating the data frame for the response time data with missing values

# for this second examople, we want a data frame that has some missing values, but we'll first create a data frame without missing values and then add a few missing values


t_1 <- as.data.frame(matrix(data = NA, nrow = 10, ncol = 4)) # pp_code, age, gender, RT


names(t_1) <- c('pp_code', 'age', 'gender', 'RT') # now we give the variables some meaningful names

t_1$pp_code <- paste('pp', c(1:10), sep = '_') # I like to use participant codes that are NOT numbers (to make sure I never accidentally treat it as continuous variable)


# for gender, I use the sample() command; it randomly samples elements from a vector that I define
t_1$gender <- sample(c(1, 2), 10, replace = TRUE) # In contrast to the previous example I want to create a variable that has 1=male and 2=female


# creating the age data
# here I use a command that creates not a normal distribution but a uniform distribution
t_1$age <- round(runif(10, min = 6, max = 22), digits = 0)


# creating the response time data
t_1$RT <- rnorm(10, mean = 300, sd = 100)

densityplot(t_1$RT) # that looks ok

# since true RT data are often negatively skewed, I try to fake this here a bit (just by playing around with different exponents etc)
densityplot(t_1$RT ^ 6 / 300^5 + 100) # ok, that looks good enough, so I add this to the data frame

t_1$RT <- t_1$RT ^ 6 / 300^5 + 100 # usually, I would never overwrite an existing variable, but since here I don't want the old variable, I do that

# OK, so now I replace some values with NA, just so I can later show you how to remove them
t_1$age[3] <- NA
t_1$gender[3] <- NA
t_1$RT[c(2, 6)] <- NA


# ok, that's fine for our purposes, so I save it as a csv file
write.csv(t_1, file = "ExampleData_RT_with_Missings.csv", row.names = FALSE) # with the argument row.names = FALSE I just avoid that the very first column in the csv file contains numbers counting up from 1 to however rows the data frame has (in this case 10)

?write.csv





