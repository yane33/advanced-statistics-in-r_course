## Homework 2  2026v ##

#01
getwd()
setwd("/Users/heyanyan/Desktop/2025_Master_8,0/2026-2027/2026_FA_B1/26F_R_10/Week 1")
df <- read.csv("parenting.csv")
df

#02
str(df)
df$SES
mean(df$SES) #41.03136
median(df$SES) #45
min(df$SES) #0
max(df$SES) #81
range(df$SES) #0 81
IQR(df$SES) #46

#03
df$SES
N <- length(df$SES) #369 sample size
str(df)
M <- sum(df$SES) / N # mean
# descriptive variance
var_des <- sum((df$SES - M)^2) / N
var_des #646.6607
# inferential variance
var_inf <- sum((df$SES - M)^2) / (N -1)
var_inf #648.418
# descriptive standard deviation
SD_des <- sqrt(var_des)
SD_des #25.42953
# inferential standard deviation
SD_inf <- sqrt(var_inf)
SD_inf #25.46405

#04
var(df$SES)
sd(df$SES)

#05
matrix <- matrix(data = NA, nrow = 10, ncol = 6)
matrix
df_wide <- as.data.frame(matrix)
df_wide
names(df_wide) <- c("pp_code", "gender", "age", "FAT_1", "FAT_2", "FAT_3")
df_wide$pp_code <- c('1','2','3','4','5','6','7','8','9','10')
df_wide$gender <- c('f','f','m','m','f','m','m','f','f','m')
df_wide$age <- c(20,30,25,20,24,32,35,21,22,27)
df_wide$FAT_1 <- c(6,7,7,6,8,5,8,9,5,10)
df_wide$FAT_2 <- c(8,8,6,6,9,8,9,10,10,10)
df_wide$FAT_3 <- c(9,9,8,8,9,10,7,8,9,8)
df_wide

#06
install.packages("lattice")
library(lattice)
densityplot(df_wide$age,xlab = "Age", ylab = "Density value")
?densityplot
#07
boxplot(df_wide$FAT_1, xlab = "FAT trail 1", ylab = "Task value")
boxplot(df_wide$FAT_2, xlab = "FAT trail 2", ylab = "Task value")
boxplot(df_wide$FAT_3, xlab = "FAT trail 3", ylab = "Task value")
?boxplot
#08
df_wide
install.packages("reshape")
library(reshape)
df_long_1 <- melt(df_wide,id.vars = c("pp_code","gender","age"), measured.vars = c("FAT_1", "FAT_2", "FAT_3"))
df_long_1

df_long_2 <- reshape(df_wide, idvar = c("pp_code","gender","age"), varying = c("FAT_1", "FAT_2", "FAT_3"),timevar = "trail1or2or3", v.names = "rating", direction = "long")
df_long_2

#09
df_long_1
df_wide_2 <- cast(df_long_1, pp_code + gender + age ~ variable, value = c("FAT_1","FAT_2","FAT_3"))
df_wide_2

# bonus
getwd()
setwd("/Users/heyanyan/Desktop/2025_Master_8,0/2026-2027/2026_FA_B1/26F_R_10/Week 2")
write.csv(df_long_1, "long.csv", row.names = FALSE)
write.csv(df_wide_2, "wide.csv", row.names = FALSE)

