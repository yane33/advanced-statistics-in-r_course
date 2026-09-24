## Lecture 3 ## data visualization

getwd()
setwd("/Users/heyanyan/Desktop/2025_Master_8,0/Small data set/✅Social Employee-Rose")
install.packages("openxlsx", type = "source")
library(openxlsx)
df <- read.xlsx("SPSS data.xlsx")
df
head(df)
str(df)

install.packages("ggplot2")
library(ggplot2)
qplot(data = df)

a <- c(49, 63, 49, 100, 20)
which(a == 49) # yes - affirmative
which(a != 100) # no - negative
which(a > 60)

# missing values NA
# how to find
# how to remove
stat.desc(myyvariable, norm = TRUE)
is.na(t_1)
is.na(t_1$pp_code)
unique(is.na(t_1$pp_code)) # T/F F means that those observations are not missing values
is.na(t_1$RT)
unique(is.na(t_1$RT)) # F/T T means that those observations include the missing values
which(is.na(t_1$RT) == TRUE) # get the specific index of the missing values
# remove the rows with NA in the variable RT
t_2 <- droplevels(t_1[-which(is.na(t_1$RT) == TRUE),])
t_2 <- droplevels(t_1)

install.packages("car")
library(car)
t_1$f_TeenagerOrNot <- as.factor(recode(t_1$f_agegroup, "'child' = 'noteenager'; 'teenager' = 'teenager'; 'adult' = 'noteenager'"))

install.packages("ggplot2")
library(ggplot2)

## Homework 3 2026v ##
# 01
matrix <- matrix(data = NA, nrow = 10, ncol = 6)
matrix
df_wide <- as.data.frame(matrix)
df_wide
names(df_wide) <- c("pp_code", "gender", "age", "FAT_1", "FAT_2", "FAT_3")
df_wide$pp_code <- c('1','2','3','4','5','6','7','8','9','10')
df_wide$gender <- c('f','f','m','m','f','m','m','f','f','m')
df_wide$age <- c(39,30,25,20,24,32,35,37,22,27)
df_wide$FAT_1 <- c(6,7,7,6,8,5,8,9,5,10)
df_wide$FAT_2 <- c(8,8,6,6,9,8,9,10,10,10)
df_wide$FAT_3 <- c(9,9,8,8,9,10,7,8,9,8)
df_wide
install.packages("reshape")
library(reshape)
df_long_1 <- melt(df_wide,id.vars = c("pp_code","gender","age"), measured.vars = c("FAT_1", "FAT_2", "FAT_3"))
df_long_1

install.packages("car")
library(car)
range(df_long_1$age) # 20-39
recode() # age continuous demographic variable => categorical factor with four different levels
df_long_1$f_age <- as.factor(recode(df_long_1$age, "20:25 = 'young'; 26:30 = 'mid-young'; 31:35 = 'mid-old'; 35:40 = 'old'"))
str(df_long_1)
table(df_long_1$f_age)

# 02
df_subset <- df_long_1[-which(df_long_1$f_age %in% c("mid-old", "mid-young")), ]
df_subset
nrow(df_subset)

# 03
install.packages("ggplot2")
library(ggplot2)
str(mpg) # cty hwy
qplot(manufacturer, cty, data = mpg, geom = "boxplot",
      xlab = "Different Manufacturers", ylab = "The city mileage")
qplot(manufacturer, hwy, data = mpg, geom = "boxplot",
      xlab = "Different Manufacturers", ylab = "The highway mileage")

# 04
df_subset_VFHT <- mpg[which(mpg$manufacturer %in% c("volkswagen", "ford", "honda", "toyota")), ]
df_subset_VFHT
head(df_subset_VFHT)

# 05
qplot(cty, data = df_subset_VFHT,geom = "density", colour = manufacturer,
      xlab = "City mileage", ylab = "Density")

qplot(cty, data = df_subset_VFHT, geom = "histogram", facets = .~manufacturer,
      xlab = "City mileage", ylab = "Density") +
  theme_bw(base_size = 13)

# 06
qplot(cty, hwy, data = df_subset_VFHT, shape = class,
      xlab = "City mileage", ylab = "Highway mileage") # colour is a different color

# 07
install.packages("dplyr")
library(dplyr)
df_subset_VFHT$class <- as.factor(df_subset_VFHT$class)
p <- ggplot(df_subset_VFHT, aes(x = displ, y = hwy, colour = class)) +
  geom_point(alpha = 0.5, size = 1.5) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_colour_brewer(palette = "Set1") +
  labs(x = "Engine displacement (L)", y = "Highway fuel economy (mpg)", colour = "Vehicle class") +
  theme_bw(base_size = 13) +
  theme(legend.position = "right")
p

# 08 (a)
names(df_subset_VFHT)

ggplot(df_subset_VFHT, aes(x = class, y = hwy, fill = class)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Vehicle class", y = "Mean highway mileage (mpg)", fill = "Vehicle class") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# (b)
ggplot(df_subset_VFHT, aes(x = class, y = hwy, fill = class)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(x = "Vehicle class", y = "Mean highway mileage (mpg)", fill = "Vehicle class") +
  theme_bw(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)

#(c)
install.packages("ggthemes")
library(ggplot2)
library(ggthemes)

ggplot(df_subset_VFHT, aes(x = class, y = hwy, fill = class)) +
  geom_bar(stat = "summary", fun = "mean") +
  scale_fill_few() +
  labs(
    title   = "Mean Highway Fuel Economy by Vehicle Class",
    x       = "Vehicle class",
    y       = "Mean highway mileage (mpg)",
    fill    = "Vehicle class",
    caption = "Note. Bars represent mean highway fuel economy (mpg) for each class."
  ) +
  theme_few(base_size = 13) +
  theme(
    axis.title    = element_text(face = "bold"),
    axis.text.x   = element_text(angle = 45, hjust = 1),
    plot.title    = element_text(size = 15, face = "bold"),
    plot.caption  = element_text(size = 10, hjust = 0),
    legend.title  = element_text(face = "bold")
  )

# 09
str(df_subset_VFHT)
head(df_subset_VFHT)

df_subset_VFHT_df <- as.data.frame(df_subset_VFHT)
df_subset_VFHT_df$id <- row.names(df_subset_VFHT_df)
df_subset_VFHT_df

mpgsmall_long <- reshape(df_subset_VFHT_df, varying = c("cty", "hwy"),
                         v.names = "mileage",
                         timevar = "type",
                         times = c("cty", "hwy"),
                         direction = "long")
mpgsmall_long$type <- as.factor(mpgsmall_long$type)
str(mpgsmall_long)
table(mpgsmall_long$type, mpgsmall_long$class)

p <- ggplot(mpgsmall_long, aes(x = class, y = mileage, fill = type)) +
  # 柱子：高度 = 均值，position_dodge 让 cty/hwy 并排
  stat_summary(fun = mean, geom = "bar",
               position = position_dodge(width = 0.9)) +
  # 误差棒：mean ± SE
  stat_summary(fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = 0.9),
               width = 0.2, linewidth = 0.7) +
  # ggthemes 配色与主题（第 8c 题要求，保留轴标签，不用 WSJ）
  scale_fill_few() +
  labs(
    title   = "City and Highway Fuel Economy by Vehicle Class",
    x       = "Vehicle class",
    y       = "Mean mileage (mpg)",
    fill    = "Mileage type",
    caption = "Note. Bars represent mean mileage; error bars represent ±1 standard error."
  ) +
  theme_few(base_size = 13) +
  theme(
    axis.title   = element_text(face = "bold"),
    axis.text.x  = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(face = "bold", size = 15),
    plot.caption = element_text(size = 10, hjust = 0),
    legend.title = element_text(face = "bold")
  )
p


