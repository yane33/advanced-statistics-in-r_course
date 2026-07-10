#######################################################
#######################################################
################## WEEK 1 #############################
###################################### EDITER: YANYAN #
#######################################################

#################### 1
### CLASS CODES #### 1
#################### 1

# value is assigned to object
a <- 2 + 2
a

# vector c() 向量
vec1 <- c(8, 13, 20, 4, 1, 13)
vec1

# graphics
hist(vec1)
hist(vec1, breaks = 2) # default setting
hist(vec1, breaks = 50) #breaks = the number of categories on the x axis
boxplot(vec1)

### need to install outside packages to draw a graphic
install.packages("lattice")
library(lattice)
densityplot(vec1)

# characteristic
mean(vec1) #均值
median(vec1) #中位数
min(vec1)
max(vec1)
range(vec1)

summary(vec1) #everything above is included

# extraction = index
vec1
vec1[1]
vec1[2:4]
vec1[c(1, 3:5, 2)] #self-made order to extract

### from disordered to coherent
d <- c("stat", "class", "this", "a", "is")
d
d[c(3,5,4,1,2)]

# two simple types of data types: matrix 矩阵 and the data frame 数据框
matrix1 <- matrix(data = NA, ncol = 3, nrow = 5) #column = 列 row = 行
matrix1
###transform the matrix to data frame
df1 <- as.data.frame(mat1)
df1
### assign the names, actual values to the column vectors 通过列来填补数据框
names(df1) <- c('pp_code', 'gender', 'age') #"" also works
df1
df1$pp_code <- c('p1', 'p2', 'p3', 'p4', 'p5')
df1$gender <- c('f','f', 'm', 'f', 'm')
df1$age <- c(5, 18, 99, 2, 10)
df1
### extraction in the data frame
df1[2,] #[row, column]
df1[,2]
df1[2,1]

# check the data structure
head(df1)
tail(df1)
str(df1)#property
df1$f_gender <- as.factor(df1$gender)#command to change the property
str(df1)
summary(df1)

# get and set the work directory; save the .csv => stop work
getwd()
setwd("/Users/heyanyan/Desktop/2025_Master_8,0/R_2026Summer/FA_R_3.16_8,5-9,0/Week1")
write.csv(df1, file = 'MyFirstcsv.csv')#.csv = excel, get the first column named the row name
write.csv(df1, file = 'MyFirstcsv.csv',row.names = FALSE)

df2 <- read.csv('MyFirstcsv.csv')
df2
df2$X <- NULL #delete one column from the data frame
df2
str(df2)
df2$f_age <- as.factor(df2$age) #add one column to the data frame
df2

# importing the data
### default setting: export from spss/excel as csv file, then
mydata <- read.csv("")#file name or path

### dutch os uses decimal comma, instead of decimal point 十进制而不是小数点
### after importing, check data using head() or tail(), if it doesn't look right, try some of the following:
read.csv("", sep = ";")
read.csv2("")# first try
read.csv2("", sep = ";")# second try
read.csv2("", sep = ",")

### other formats rather than .csv
install.packages("foreign")
library(foreign)
df <- read.xls("")
df <- read.spss("", to.data.frame = TRUE)# important to spss

# missing values in r: NA


#################### 2
#### HOME WORK ##### 2
#################### 2

# part c: watch and learn (data set: pedometer.csv)
getwd()
setwd("/Users/heyanyan/Desktop/2025_Master_8,0/R_2026Summer/FA_R_3.16_8,5-9,0/*Week1")
df <- read.csv("pedometer.csv")
df #n = 88
str(df)
x = hist(df$Steps, col = "lightblue")
x = hist(df$Steps, breaks = 20, col = "lightblue")
hist(df$Steps)# equal

macaron_colors <- c("#FFB6C1", "#B5EAD7", "#C7A4E0", "#FFD6A5", "#A2D2FF")

plot(df$Steps ~ df$Observation, col = macaron_colors[1])# y~x relationship
mymodel =lm(df$Steps ~ df$Observation)#linear model
summary(mymodel)
lines(fitted(mymodel), lwd = 2, col = macaron_colors[2])#to see the result of the regression line = increase per day
lines(fitted(loess(mymodel)), lwd = 2, col = macaron_colors[3])

### homework answers
df
head(df)
tail(df)

mean(df$Steps)
sd(df$Steps)

x = boxplot(df$Steps)
x = hist(df$Steps)

##### Q5 plot() is a figure creator to show the column and number of each elements from one variable
# a easy way
str(df)
df$f_Day <- as.factor(df$Day)
plot(df$f_Day)

# practice for myself
str(df)
df$f_Day <- factor(df$Day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
number_data_points <- table(df$f_Day)#f_Day in order
number_data_points
plot(number_data_points,
     main = "Number of Data Points for Each Day of the Week",
     xlab = "Day", ylab = "Number",
     col = macaron_colors[3], lwd = 10)

install.packages("lattice")
library(lattice)
densityplot(df$Steps, col = macaron_colors[1])

# part d: apply what you have learned (data set: parenting.sav)
install.packages("foreign")
library(foreign)
df2 <- read.spss("parenting.sav", to.data.frame = TRUE)
df2

head(df2)
tail(df2)

table(df2$Gender)#check the proportion of different elements in each column

hist(df2$Psycho, col = macaron_colors[2])
densityplot(df2$Psycho)

mean(df2$Psycho)
sd(df2$Psycho)

write.csv(df2, file = "parenting.csv", row.names = FALSE)









