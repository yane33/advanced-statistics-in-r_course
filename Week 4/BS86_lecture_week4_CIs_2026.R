
# R script for Bill's lecture (week 4): Confidence intervals and significance tests 

# This script closely follows the examples in book chapters 3 and 4, which
# includes manual (and automated) calculations of different types of CIs and
# includes several bivariate statistical tests (e.g., t-tests and chi-square tests)

#############################################################################################

#set working directory
setwd("C:/Users/u148154/OneDrive - Radboud Universiteit/RU-Drive/Desktop/NEW Stats course")

# install and upload packages
install.packages('exactci') # for asymmetric CIs, such as Blaker CIs
library(exactci)
install.packages("foreign") # for importing SPSS data files
library(foreign)
install.packages("pastecs") # for obtaining descriptive statistics
library(pastecs)
install.packages("ggplot2") # for mpg datafile
library(ggplot2)
install.packages("Rmisc") # for symmetric confidence intervals
library(Rmisc)
install.packages("lattice") # for density plots
library(lattice)
install.packages("gmodels") # for chi-square tests
library(gmodels)
install.packages("boot") # for bootstrapped CIs
library(boot)

###############################################################
#PRIMER: In Week 2 you were shown that the standard deviation (SD) 
#        is a measure of dispersion based on the "sum of squares".
#        It is also the basis of calculating confidence intervals, 
#        so, below I briefly show (again) how to calculate the SD.

# Let's use the city mileage variable in the mpg dataframe in the ggplot2 package...
# 
# As per Bernd's script...

#create individual deviations from the mean
mpg$diffMean_city <- mpg$cty - mean(mpg$cty)
#square the individual deviation scores
mpg$SQdiffMean_city <- mpg$diffMean_city**2
#calculate the variance (sum of squares / n-1)
var <- sum(mpg$SQdiffMean_city) / (length(mpg$cty)-1)
var
# calculate SD (square root of variance)
sd <- sqrt(var)
sd
# double-check
sd(mpg$cty)
# the stat.desc() function provides several descriptive statistics (which we will use later)
stat.desc(mpg$cty, basic = F) # in pastecs() package

# Below I calculate the confidence intervals based on the standard deviation,
# which for a sampling distribution is called the "standard error" (SE)

###############################################################

#### Confidence intervals ####

# Example 3.1: Normal CI for the mean of a single sample with known population variance

#input summary data
m.hat <- 94.6                    # sample mean
sigma <- 15                      # population SD
n <- 50                          # sample size
conf <- 95                       # confidence level
alpha <- (100-conf)/100          # convert confidence level to alpha 

#perform calculations
se.hat <- sigma/n**.5            # calculate SE
#se.hat <- sigma/sqrt(n)         # alternative to previous line, but perhaps more intuitive
moe <- se.hat*qnorm(1-alpha/2)   # calculate MOE based on quantile function for normal distribution (qnorm)
lower = m.hat - moe              # calculate lower bound
upper <- m.hat + moe             # calculate upper bound

c(lower, upper)                  # combine (and print) lower and uppor bounds

##################################

# Example 3.2: CI for the mean of a single sample with known sample variance (using t)

#input summary data
m.hat <- 94.6                      # sample mean
sigma.hat <- 19.6                  # sample SD
n <- 50                            # sample size
conf <- 95                         # confidence level
alpha <- (100-conf)/100            # convert confidence level to alpha

#perform calculations 
se.hat <- sigma.hat/n^.5           # calculate SE
moe <- se.hat * qt(1-alpha/2,n-1)  # calculate MOE based on quantile function for t-distribution (qt) with n - 1 df
lower <- m.hat - moe               # calculate lower bound
upper <- m.hat + moe               # calculate upper bound

c(lower, upper)

### alternative manual calculation for example 3.2 that combines the input and calculation steps
#lower <- 94.6 - qt(.975,49) * 19.6/50^.5
#upper <- 94.6 + qt(.975,49) * 19.6/50^.5
#c(lower, upper)

###################################

# manual calculations are needed when only summary statistics are available, but...
# there are "automated" ways of obtaining the SEM, MOE, and CIs when raw data is available

#using the mpg dataframe from ggplot2
names(mpg)

# OPTION #1: calculate based on info provided by stat.desc()

# if we want to obtain SEM, MOE, and CIs for city mileage of all cars
stat.desc(mpg$cty, basic = F)

# SEM is labeled SE.mean
# MOE is labeled CI.mean.95

# for CIs, add/subtract MOE (CI.mean.0.95) from M (mean)
lower <- 16.8589744 - 0.5481481
upper <- 16.8589744 + 0.5481481
c(lower,upper)

# OPTION #2: calculate CIs and M directly using CI()
CI(mpg$cty)

####

# here is the manual calculation demonstrating equivalence with automated options
m.hat <- 16.8589744                     # sample mean
sigma.hat <- 4.2559457                  # sample SD
n <- 234                                # sample size
conf <- 95                              # confidence level
alpha <- (100-conf)/100                 # convert confidence level to alpha

se.hat <- sigma.hat/n^.5           # calculate SE
moe <- se.hat * qt(1-alpha/2,n-1)  # calculate MOE based on quantile function for t-distribution (qt) with n - 1 df
lower <- m.hat - moe               # calculate lower bound
upper <- m.hat + moe               # calculate upper bound

c(lower, upper)

###################################

#Example 3.3: Obtaining asymmetric CIs using logarithm (not covered in class)

mu.hat <- log(9.6)
sig.hat <- log(2.5)
n <- 338
alpha <- 0.05
se.hat <- sig.hat/n^.5
moe <- qt(1-alpha/2, n-1)*se.hat
ll <- mu.hat - moe
ul <- mu.hat + moe

exp(ll)
exp(ul)

###################################

#Example 3.4: Confidence intervals for discrete data (binomial)

# example calculating Wald CI
s <- 19
n <- 25
alpha <- .01

p.hat <- s/n
se.p <- ((p.hat*(1-p.hat))/n)^.5
moe <- qnorm(1-alpha/2)*se.p
c(p.hat-moe, p.hat+moe)

#same example calculating adjusted Wald CI
s <- 19 + 2
n <- 25 + 4

p.hat <- s/n
se.p <- ((p.hat*(1-p.hat))/n)^.5
moe <- qnorm(1-alpha/2)*se.p
c(p.hat-moe, p.hat+moe)

# same example illustrating impossible values (P > 1) from Wald CI
s <- 24
n <- 25

p.hat <- s/n
se.p <- ((p.hat*(1-p.hat))/n)^.5
moe <- qnorm(1-alpha/2)*se.p
c(p.hat-moe, p.hat+moe)

# same example illustrating impossible values (P > 1) from adjusted Wald CI
s <- 24 + 2
n <- 25 + 4

p.hat <- s/n
se.p <- ((p.hat*(1-p.hat))/n)^.5
moe <- qnorm(1-alpha/2)*se.p
c(p.hat-moe, p.hat+moe)

# same example for calculating Wilson continuity-corrected CI (default 95%)
prop.test(19, 25)$conf.int

# same example for calculating Wilson CI (override correction and default confidence level)
#So, this is for a 99% Wilson CI (w/o continuity correction)
prop.test(19, 25, conf.level = 0.99, correct = FALSE)$conf.int

# same example for calculating 99% Clopper-Pearson CI
binom.test(24, 25, conf.level = 0.99)$conf.int

#example for calculating 99% Blaker CI (recommended)
#defaults are 95% Clopper-Pearson, so these are overridden
binom.exact(19, 25, tsmethod='blaker', conf.level=0.99)

# NOTE: The (adjusted) Wald statistic (and CIs) should be used cautiously, Blaker is recommended 

############################

#Example 3.5: Confidence intervals for discrete data (Poisson)
l.hat <- 7
alpha <- 0.05

ll <- qchisq(alpha/2, 2*l.hat)/2
ul <- qchisq(1-alpha/2, 2*(l.hat+1))/2

#calculate CIs for 2 week period
c(ll, ul)

#calculate for one week period
c(ll, ul)/2

#calculate exact CI for Poisson distribution
poisson.test(7, T= 2, conf.level = .95)$conf.int

#calculate Blaker CI for Poisson distribution (recommended)
poisson.exact(7, T=2, tsmethod='blaker', conf.level = .99)

#############################################################

#Examples 3.6 and 3.7: CI for a difference in independent means 

#import data file
bp2000 <- read.spss("baguley_payne_2000.sav", to.data.frame = T)
names(bp2000)

#select variable from dataframe
pc.acc <- bp2000$percent_accuracy

# you can also select the 4th variable using indexing...note the double [] 
pc.acc <- bp2000[[4]]

#select specific cases (28 rows from 2 conditions)
lsc <- pc.acc[1:28]
hsc <- pc.acc[29:56]

# you could also select the data in other ways...

#using subset()
lsc <- subset(bp2000, group == "low study" ,select = percent_accuracy)
hsc <- subset(bp2000, group == "high study" ,select = percent_accuracy)

#using indexing (note: this produces values, not objects)
lsc <- bp2000[1:28, 4] # [rows , columns]
hsc <- bp2000[29:56, 4]

######################################

# numerically and visually inspect "new" data

stat.desc(lsc, basic = F, norm = T)
stat.desc(hsc, basic = F)

#histograms or density plots
hist(hsc)
hist(lsc)

densityplot(hsc)
densityplot(lsc)

# you could also simply examine the two groups using the original dataframe

by(bp2000$percent_accuracy, bp2000$group, stat.desc, basic = F, norm = T)

densityplot(bp2000$percent_accuracy, group = bp2000$group)

######################################

# here are some additional ways of calculating CIs for Ms of multiple groups

#calculate confidence intervals for (the mean of) each group in separate objects
t.test(lsc, conf.level=.95)$conf.int
t.test(hsc, conf.level=.95)$conf.int

#alternative ways of calculating confidence intervals for each group by selecting rows
t.test(bp2000$percent_accuracy[1:28])$conf.int
t.test(pc.acc[1:28])$conf.int

# using by() and CI() to calculate CIs separately for each group in original dataframe
by(bp2000$percent_accuracy, bp2000$group, CI)

######################################

#here is an example that calculates the SE, MOE, and CIs for the difference manually
m1 <- 87.2 ; m2 <- 74.1
sd1 <- 8.5 ; sd2 <- 24.0
n1 <- n2 <- 28
alpha <- .05

var.p <- ((n1-1)*sd1^2+(n2-1)*sd2^2)/(n1+n2-2)
sd.p <- var.p^.5
se.p <- sd.p*(1/n1+1/n2)^.5
moe <- qt(1-alpha/2,n1+n2-2) * se.p
ll <- m1-m2 - moe
ul <- m1-m2 + moe

c(ll, ul)


#here is the automatic way of calculating the CIs (default is with Welch-Satterthwaite correction)
t.test(hsc, lsc)$conf.int
#NOTE: the CI is for the mean-level difference between groups (not for the t-statistic)

#here is the automatic way of calculating the CIs (assuming homogeneity of variance)
t.test(hsc, lsc, var.equal=TRUE)$conf.int

#NOTE: these two calculations use the separate lsc and hsc objects that were created above

########

#here is an alternative format using attach() and detach()
attach(bp2000)
t.test(percent_accuracy[29:58], percent_accuracy[1:28])$conf.int
detach(bp2000)

########

#here is another alternative using with() instead of attach()
with(bp2000, t.test(percent_accuracy[29:58], percent_accuracy[1:28])$conf.int)

#######

#here is a final way using the original dataframe
t.test(percent_accuracy ~ group, bp2000)$conf.int

#NOTE: This last calculation provides the inverse (because of different ordering of the groups)

############################################################

#3.7.5 CI for a difference in proportions (not covered in class)

s <- c(24, 19)
n <- c(25, 25)

#this function calculates the Wilson CI (default is with continuity correction)
prop.test(s, n)$conf.int

#the function calculates the Wilson CI w/o continuity correction
prop.test(s, n, correct = FALSE)$conf.int

###############################################################

# 3.7.6 Monte Carlo methods (not covered in class)

s <- 19
n <- 25
alpha <- .01
B <- 999
P <- s/n
x <- rbinom(B, n, P)/(n)
mean(x)
quantile(x, c(alpha/2,1-alpha/2))

P - mean(x)

# Example 3.8: Setting up a percentile bootstrap

bl1 <- sample(lsc, 28, replace=TRUE)
bh1 <- sample(hsc, 28, replace=TRUE)

par(mfrow=c(2,2), mar = c(4,4,3.5,2), pty="s")

hist(lsc, ylim = c(0,20), xlab = 'Percentage accuracy', main='(a) Low study condition: observed')
hist(hsc, ylim = c(0,20), xlab = 'Percentage accuracy', main='(b) High study condition: observed')

hist(bl1, ylim = c(0,20), xlab='Percentage accuracy', main=' (c) Low study condition: bootstrap')
hist(bh1, ylim = c(0,20), xlab = 'Percentage accuracy', main='(d) High study condition: bootstrap')

set.seed(1234)
B <- 9999

mbl <- replicate(B, mean(sample(lsc,28,replace=TRUE)))
mbh <- replicate(B, mean(sample(hsc,28,replace=TRUE)))

mdiffs <- mbh-mbl

sd(mdiffs)
quantile(mdiffs, c(.025,.975))
bias <- mean(mdiffs) - (mean(hsc)-mean(lsc))
bias

hist(mdiffs)

####################################################################

#Example 3.9: Percentile and BCa bootstrap methods in R (not covered in class)

#create function to perform the bootstrapping of median for 9999 samples
medboot <- boot(lsc, function(x,i) median(x[i]), R = 9999)
#calculate the percentile bootstrapped CIs
boot.ci(medboot, conf=.95)

######################

#creating bootstrapped CIs for mean difference between 2 groups

#creating an empty 2 x 28 matrix
pm <-matrix(nrow=28, ncol=2)
#adding the values from the low and high study condition values to matrix
pm[,1] <-lsc
pm[,2] <-hsc

#create function to perform the bootstrapping of mean difference for 9999 samples
bootdiffs <- boot(pm, function(x,i) mean(x[i,2])-mean(x[i,1]), R=9999)

#calculate bca bootstrapped CIs
boot.ci(bootdiffs, type='bca')

#histograms of the bootstrapped sampling distributions
hist(bootdiffs[[2]])
hist(bootdiffs$t)

##########################################################

#Plotting CIs with a bar chart (with basic plot functions...not covered in class)

#create objects that include needed statistics for the bp2000 data (lsc and hsc)
cmeans <- c(mean(lsc), mean(hsc))
names(cmeans) <-c("Low Study","High Study")
n <- c(28,28)
csd  <- c(sd(lsc), sd(hsc))                           # standard deviations for the two groups
se <- csd/sqrt(n)                                     # calculate standard error
cl <- cmeans + qt(.025, n[2]-1) * se                  # calculate lower bound for CIs of means
cu <- cmeans + qt(.975, n[1]-1) * se                  # calculate upper bound for CIs of means

il <- cmeans + qt(.025, n[2]-1) * se * sqrt(2)/2      # calculate lower bound for CIs of difference
iu <- cmeans + qt(.975, n[2]-1) * se * sqrt(2)/2      # calculate upper bound for CIs of difference

# create bar chart (w/ labels) for means of the tewo groups (and CIs for individual means)

#creates basic bar chart
cbars <- barplot(height = cmeans, beside = T, ylab="Mean percentage accuracy", ylim = c(50,100), xlim = c(0,4), space=0.7, 
                 density=c(50,90), col=c('light gray', 'dark gray'), cex.main=.925, xaxt='n', xpd=FALSE)

#add CIs of individual means
segments(x0 = cbars, x1 = cbars, y0=cl, y1=cu)
#add lower tick
segments(x0 = cbars-.05, x1 = cbars +.05, y0=cl, y1=cl)
#add upper tick
segments(x0 = cbars-.05, x1 = cbars +.05, y0=cu, y1=cu)
# add x-axis
segments(x0 = 00, x1 = max(cbars)+1, y0=50, y1=50, lty = 1, lwd = 2)
# add labels for x-axis
axis(1, at = cbars, labels=names(cmeans), tick = FALSE)


# create bar chart (w/ labels) for means of two groups (and CIs for individual means AND difference-adjusted CIS)
#NOTE: this chart is identical to the previous except for the use of il and ul (instead of cl and cu)

cbars2 <- barplot(height = cmeans, beside = T, ylab="Mean percentage accuracy", ylim = c(50,100), xlim = c(0,4), space=0.7, 
                  density=c(50,90), col=c('light gray', 'dark gray'), cex.main=.925, xaxt='n', xpd=FALSE)

segments(x0 = cbars2, x1 = cbars2, y0=cl, y1=cu)
segments(x0 = cbars2-.05, x1 = cbars +.05, y0=il, y1=il)
segments(x0 = cbars2-.05, x1 = cbars +.05, y0=iu, y1=iu)
segments(x0 = 00, x1 = max(cbars2)+1, y0=50, y1=50, lty = 1, lwd = 2)
axis(1, at = cbars, labels=names(cmeans), tick = FALSE)

# resets graphical parameters
dev.off()

# NOTE: plotting CIs for barchart using ggplot can also be done with geom = "errorbar"
### this plot could be made more similar to the plots created above, but it is close:)


ggplot(bp2000, aes(x =group, y =percent_accuracy, fill = group)) +
  stat_summary(geom = "bar", fun = mean) +
  stat_summary(geom = "errorbar",width=0.2, fun.data = mean_cl_normal) + 
  coord_cartesian(ylim = c(50, 100)) +ylab("Mean percentage accuracy")+ xlab("Groups") + 
  scale_fill_grey()+ theme_bw() + guides(fill="none")


################################################################################
################################################################################
################################################################################

#### Significance tests ####

# Calculating p values from a t or z test statistic

# several (equivalent) options for calculating one-sided p-values from normal (z) distribution

#option 1
1 - pnorm(1.65)
#option 2
pnorm(1.65, lower.tail = FALSE)
#option 3
pnorm(-1.65)

#to obtain a two-sided p-value then you need to multiply the one-sided value by 2

#option 1a
(1 - pnorm(1.65)) * 2
#option 2a
2 * pnorm(1.65, lower.tail = FALSE)
#option 3a
2 * pnorm(-1.65)

########################

# two options for calculating a one-sided p-value from a t-distribution
pt(-1.95, 49)       # value and df

2 * pt(-1.95, 49)   # value and df

#same as above, to obtain a two-sided p-value, then multiply by 2
2 * pt(-2.72, 54)
2 * pt(2.72, 54, lower.tail = FALSE)

# here the df are corrected using the Welch-Satterthwaite adjustment (does not assume homogeneity of variance)
2 * pt(-2.72, 33.7)
# note that the 33.7 df for the Welch Satterthwaite correction can be computed using the nu.prime function from Chapter 15

###################################

# Examples 4.3 and 4.4: t tests from summary statistics

##perform one sample t-test manually

#create objects with appropriate statistics
m.hat <- 94.6                # sample mean
sigma.hat <- 19.6            # sample variance
n <- 50                      # sample size
se.hat <- sigma.hat/n^0.5    # calculate SE
m.null <- 100                # indicate population mean

#calculate t-value
t.obs <- (m.hat - m.null)/se.hat
t.obs

#calculate p-value for the t-value
2 * pt(t.obs, n - 1)
2 * pt(-abs(t.obs), n - 1)

#############################

## perform an independent samples t-test manually

# create objects with appropriate statistics
m1 <- 87.2
m2 <- 74.1
sd1 <- 24.0
sd2 <- 8.5
n1 <- n2 <- 28

var.p <- ((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2)/(n1 + n2 - 2)
sd.p <- var.p^0.5

se.p <- sd.p * (1/n1 + 1/n2)^0.5

#calculate t-value
t.ind <- (m1 - m2)/se.p
t.ind

#compute p-value
2 * pt(-abs(t.ind), n1 + n2 - 2)

########################################################################

#Examples 4.3, 4.4 and 4.5: t tests from raw data

#the initial example uses the bp2000 data file that was also used in the chapter 3 examples

# this example is not presented in class because proportion scores are known to not be normally distributed,
# so using a t-test is not the most appropriate manner to test differences in percent accuracy
# instead, I present an example from the mpg dataframe (see below)

# it is also possible to present an example of paired t-tests using the mpg data,
# instead of using another dataframe like is done in the textbook, so I present both here,
# but only present the mpg example in class

########################################################################

# examples from the book (not presented in class)

bp2000 <- read.spss("baguley_payne_2000.sav")

pc.acc <- bp2000[[4]]
lsc <- pc.acc[1:28]
hsc <- pc.acc[29:56]

#perform one sample t-test comparing mean to zero (the default) 
t.test(lsc)
t.test(hsc)

#perform one sample t-test comparing mean to 50
t.test(lsc, mu = 50)
t.test(hsc, mu = 50)

#perform independent sample t-test (does not assume homogeneoty of variance)

#using orginal dataframe (NOTE: DV is on left side of tilde (~) and IV is on right side)
t.test(bp2000$percent_accuracy ~ bp2000$group)

# using separated group objects
t.test(lsc, hsc)

#to obtain SDs
sd(lsc)
sd(hsc)

#perform independent sample t-test (assuming homogeneity of variance)
t.test(bp2000$percent_accuracy ~ bp2000$group,var.equal=T)
t.test(lsc, hsc, var.equal=TRUE)

#for the paired sample t-test we need another data file that has two continuous measures

bren.dat <- read.spss("brennen_et_al_1990.sav", to.data.frame = TRUE)

summary(bren.dat)
head(bren.dat)

#perform a paired sample t-test automatically
t.test(bren.dat$face, bren.dat$question, paired = TRUE)

mean(bren.dat$face)
sd(bren.dat$face)
mean(bren.dat$question)
sd(bren.dat$question)

#perform a paired sample t-test using difference scores
pdiffs <- bren.dat$face - bren.dat$question      #calculate difference scores
t.test(pdiffs)

#perform a paired sample t-test manually
n.paired <- length(pdiffs)
t.paired <- mean(pdiffs)/(sd(pdiffs)/sqrt(n.paired))
t.paired

p.obs <- 2 * pt(-abs(t.paired), n.paired - 1)
p.obs

#####################################################

# examples presented in class using mpg data:

# does average city mileage differ from 20 miles per gallon? (one-sample t-test)
t.test(mpg$cty, mu = 20)

#does average city mileage differ for front and rear-wheel drive vehicles? (independent sample t-test)

#first, remove 4-wheel drive vehicles
mpg_small <- subset(mpg, drv == "f" | drv== "r") # NOTE: use of double equal sign (==) to indicate specific factor levels

# then perform t-test
t.test(mpg_small$cty ~ mpg_small$drv)

#here is an alternative that does not use $
t.test(cty ~ drv, mpg_small)

# to obtain SDs or CIs
by(mpg_small$cty, mpg_small$drv, stat.desc, sd)
by(mpg_small$cty, mpg_small$drv, CI)


# does average city mileage differ from average highway mileage? (paired samples t-test)
t.test(mpg$cty, mpg$hwy, paired = T)

# to obtain Ms and SDs
stat.desc(mpg$cty)
stat.desc(mpg$hwy)

######################################################

# Example 4.6: The binomial test

#several equivalent options for obtaining p-value for 4 successes out of 10 trials when p = .15

#option 1
sum(dbinom(4:10, 10, 0.15))

#option 2
1 - pbinom(3, 10, 0.15)

#option 3
pbinom(3, 10, 0.15, lower.tail = FALSE)

#option 4...with Clopper-Pearson CIs
binom.test(4, 10, 0.15, alternative = "greater")

#option 5...with Blaker CIs
binom.exact(4, 10, 0.15, tsmethod = "blaker")

#this method can also be applied to test differences between proportions
prop.test(c(24, 19), c(25, 25))

#also for data from Poisson distribution
poisson.test(18, T = 5, r = 2)
poisson.exact(18, T = 5, r = 2, tsmethod = "blaker")

############################################

# Example 4.7: The Pearson chi-square goodness-of-fit (GOF) test

#test significance of 3 successes out of 10 trials (7 fails)
chisq.test(c(3, 7))

#several options for computing GOF comparing observed frequencies vs. population 

#create data object
observed <- c(12, 56, 43, 8)
expected.prop <- c(0.141, 0.494, 0.297, 0.068)

#option 1
chisq.test(observed, p = expected.prop)

#option 2 (rescaling so proportions equal 1)
chisq.test(observed, p = expected.prop, rescale.p = TRUE)

#examine expected frequencies and residuals for each group
chisq.test(observed, p = expected.prop)$expected
chisq.test(observed, p = expected.prop)$residuals

#option 3, 4, and 5 (for calculating test statistic)
#3
sum(chisq.test(observed, p = expected.prop)$residuals^2)
#4
chisq.test(observed, p = expected.prop)$statistic
#5
expected <- expected.prop * sum(observed)
chi.obs <- sum((observed - expected)^2/expected)
chi.obs

# option 5 (for calculating p-value)
pchisq(chi.obs, length(observed) - 1, lower.tail = FALSE)
1 - pchisq(chi.obs, length(observed) - 1)

###################################################################

# Example 4.8: The Pearson chi-square test of independence

#create data objects of observed frequencies in each category (summary data)
males <- c(17975, 55710, 38175, 10400)
females <- c(21370, 81590, 44485, 8465)

#label rows and columns 
row.names <- c("male", "female")
col.names <- c("1st", "2:1", "2:2", "3rd/pass")

#create contingency table
observed <- matrix(, 2, 4, dimnames = list(row.names, col.names))
observed[1, ] <- males
observed[2, ] <- females
observed

# test whether grouping variables are associated
chisq.test(observed)

#examine expected values and (standardized) residuals
chisq.test(observed)$residuals
chisq.test(observed)$expected
chisq.test(observed)$stdres

#create bar chart of observed frequencies 
barplot(observed, beside = TRUE)
barplot(observed, beside = TRUE, space = c(0, 0.4), legend = row.names, 
        xlab = "Degree class", ylab = "Frequency")
abline(h = 0)

#create bar chart of observed proportions
obs.prop <- matrix(, 2, 4, dimnames = list(row.names, col.names))
obs.prop[1, ] <- males/sum(males)
obs.prop[2, ] <- females/sum(females)
barplot(obs.prop, beside = TRUE, space = c(0, 0.4), legend = row.names, 
        xlab = "Degree class", ylab = "Proportion")
abline(h = 0)

###############################

# 2x2 chi-square (using summary data)

#restrucutre data to fit a 2 x 2 contingency table
males.2 <- c(males[1], sum(males[2:4]))
females.2 <- c(females[1], sum(females[2:4]))

#create new 2 x 2 contingency table
new.obs <- matrix(,2,2, dimnames=list(row.names, c('1st', 'not 1st')))
new.obs[1,] <- males.2
new.obs[2,] <- females.2

#test observed frequencies, examine residuals, and expected frequencies
chisq.test(new.obs, correct=FALSE)
chisq.test(new.obs)$residuals
chisq.test(new.obs)$expected

# check via hand calculation
n.all <- sum(new.obs)
row1 <- new.obs[1] + new.obs[3]
row2 <- new.obs[2] + new.obs[4]
col1 <- new.obs[1] + new.obs[2]
col2 <- new.obs[3] + new.obs[4]
cs.obs <- (n.all*(new.obs[1]*new.obs[4]-new.obs[2]*new.obs[3])^2)/(row1*row2*col1*col2)
cs.obs

# with continuity correction
cs.obs.cc <- (n.all*(abs(new.obs[1]*new.obs[4]-new.obs[2]*new.obs[3]-n.all*0.5)^2))/(row1*row2*col1*col2)
cs.obs.cc

##########################################

#calculate chi-square based on raw data

# let's use the mpg data again:)

names(mpg)
# let's examine if there is a relationship between type of drivetrain (front-, rear-, and 4-wheel drive) and fuel type (premium and regular)
# so test a 3 x 2 design

# first, we need to subset to obtain only premium and regular fuel type
mpg_2 <-subset(mpg, fl == "r" | fl == "p")

CrossTable(mpg_2$drv, mpg_2$fl, chisq = TRUE, expected = TRUE, prop.c = FALSE, prop.r = FALSE, prop.t = FALSE, prop.chisq = FALSE,  asresid = TRUE, format = "SPSS")

chisq.test(mpg_2$drv, mpg_2$fl)

#Note: the CrossTable() function can also produce McNemar test with option mcnemar = T

################################################

#Examples 4.9 and 4.10: The sign test and McNemar test (not covered in class)

#sign test mimics a binomial distribution 
binom.test(6, 11)

#create data for |Mcnemar test
rouge.data <- matrix(c(35, 0, 10, 0), 2, 2)

#perform test...

#with continuity correction (default)
mcnemar.test(rouge.data)
#w/o continuity correction
mcnemar.test(rouge.data, correct = FALSE)
