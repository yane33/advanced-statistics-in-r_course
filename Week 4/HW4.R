### Homework 4 ###
# 01
getwd()
setwd("/Users/heyanyan/Desktop/2025_Master_8,0/2026-2027/2026_FA_B1&B2/26F_R_10/Week 1")
df <- read.csv("parenting.csv")
df

# numeric
install.packages("DescTools")
library(DescTools)
Skew(df_a$Neurotic, method = 1, conf.level = .95) # neurotic 0.32
Skew(df_a$Psycho, method = 1, conf.level = .95) # psycho 0.06
Skew(df_a$Problems, method = 1, conf.level = .95) # problems -0.27

Kurt(df_a$Neurotic, method = 1, conf.level = .95) # neurotic -0.57
Kurt(df_a$Psycho, method = 1, conf.level = .95) # psycho -0.37
Kurt(df_a$Problems, method = 1, conf.level = .95) # problems -0.21

# visual
# single
install.packages("ggplot2")
library(ggplot2)
ggplot(df, aes(sample = Neurotic)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot of Neuroticism") +
  theme_minimal()

# multiple
install.packages("patchwork")
library(patchwork)
p1 <- ggplot(df, aes(sample = Neurotic)) + stat_qq() + stat_qq_line(color="red") + ggtitle("Neurotic")
p2 <- ggplot(df, aes(sample = Psycho)) + stat_qq() + stat_qq_line(color="red") + ggtitle("Psycho")
p3 <- ggplot(df, aes(sample = Problems)) + stat_qq() + stat_qq_line(color="red") + ggtitle("Problems")
p1 + p2 + p3

# 02
Neurotic <- mean(df$Neurotic) + 3*sd(df$Neurotic)
Neurotic #23.71 upper-bond
Neurotic_hout <- which(df$Neurotic >= Neurotic)
Neurotic_hout #0

Neurotic <- mean(df$Neurotic) - 3*sd(df$Neurotic)
Neurotic #-6.299 lower-bond
Neurotic_hout <- which(df$Neurotic <= Neurotic)
Neurotic_hout #0

Psycho <- mean(df$Psycho) + 3*sd(df$Psycho)
Psycho #23.71 upper-bond
Psycho_hout <- which(df$Psycho >= Psycho)
Psycho_hout #317

Psycho <- mean(df$Psycho) - 3*sd(df$Psycho)
Psycho #15.037 lower-bond
Psycho_hout <- which(df$Psycho <= Psycho)
Psycho_hout #0

Problems <- mean(df$Problems) + 3*sd(df$Problems)
Problems #22.85 upper-bond
Problems_hout <- which(df$Problems >= Problems)
Problems_hout #0

Problems <- mean(df$Problems) - 3*sd(df$Problems)
Problems #0.51 lower-bond
Problems_hout <- which(df$Problems <= Problems)
Problems_hout #0

#03 Neurotic, Psycho, Problems
m.neurotic <- mean(df$Neurotic)
m.neurotic #  8.704607
sigma <- sd(df$Neurotic) # 5.0
n <- length(df$Neurotic) #369
conf <- 95
alpha <- (100 - conf)/100
se.neurotic <- sigma/n**.5
moe <- se.neurotic*qnorm(1-alpha/2)
moe # 0.5102914

m.psycho <- mean(df$Psycho)
m.psycho #  8.704607
sigma <- sd(df$Psycho) # 5.0
n <- length(df$Psycho) #369
conf <- 95
alpha <- (100 - conf)/100
se.psycho <- sigma/n**.5
moe <- se.psycho*qnorm(1-alpha/2)
moe # 0.6772845

m.problems <- mean(df$Problems)
m.problems #  8.704607
sigma <- sd(df$Problems) # 5.0
n <- length(df$Problems) #369
conf <- 95
alpha <- (100 - conf)/100
se.problems <- sigma/n**.5
moe <- se.problems*qnorm(1-alpha/2)
moe # 0.3800134

#04 Neurotic, Psycho, Problems
summary(df$Neurotic) # range [0, 23], mean = 8.705
summary(df$Psycho) # range [18, 55], mean = 34.95
summary(df$Problems) # range [2, 22], mean = 11.68

t.test(df$Neurotic, conf.level = 0.95)$conf.int # [8.19,9.22]
t.test(df$Psycho,   conf.level = 0.95)$conf.int # [34.27, 35.63]
t.test(df$Problems, conf.level = 0.95)$conf.int # [11.30, 12.06]

#05-06 Gender, response, demand
head(df)
str(df)

# change the type of these variables from chr to factor to numeric
df$n_Response <- NA
df$n_Response[df$Response == "low"] <- 0
df$n_Response[df$Response == "high"] <- 1

df$n_Demand <- NA
df$n_Demand[df$Demand == "low"] <- 0
df$n_Demand[df$Demand == "high"] <- 1

# a the relationship between parental responsiveness and parental demandingness
cor.test(df$n_Response, df$n_Demand,
         method = "pearson",
         conf.level = 0.95) # p = .94 not significant

# b problem and neurotic problem in adolescent
t.test(df$Problems, df$Neurotic, # problems - neurotic
       paired = TRUE,
       conf.level = 0.95) # p < .001, [2.32, 3.63] significant
# sample estimates = mean difference 2.97561 平均而言，problems比neurotic高2.98
mean(df$Problems) # 11.68 problems is higher than the neurotic
mean(df$Neurotic) # 8.70

# c psycho difference across gender
head(df)
t.test(Psycho ~ Gender, data = df,
       conf.level = 0.95) # p = .65 > .05 CI includes zero => no difference

# d relationship between higher responsiveness and less problems
str(df)
df$f_Response <- as.factor(df$Response)
t.test(Problems ~ f_Response, data = df,
       conf.level = 0.95) # p = .02 < .05 10.94 > 11.99 => low responsiveness = higher problems

# e demanding parents difference across gender
str(df)
table(df$Gender, df$Demand)
chisq.test(table(df$Gender, df$Demand)) # p = .11 < .05 no difference

#08
str(df)
df$f_Demand <- as.factor(df$Demand)
t.test(df$Neurotic ~ f_Demand, data = df, conf.level = 0.95)
aggregate(df$Neurotic ~ f_Demand, data = df, FUN = sd) # SD = high demand = 5.12, low demand = 4.64
# p = 0.007556, high demanding = high neuroticism
install.packages("effectsize")
library(effectsize)
cohens_d(Neurotic ~ f_Demand, data = df) # d = 0.29, 95% CI [0.07, 0.50]

#09
df$Gender <- factor(df$Gender)
t.test(df$Problems ~ df$Gender, data = df, conf.level = 0.95)
# p < .001, significant = problem behaviors show higher in the male group
aggregate(df$Problems ~ df$Gender, data = df, FUN = sd) # SD: female = 3.34, male = 3.53
cohens_d(df$Problems ~ df$Gender, data = df) # d = -0.85

#10
tab <- table(df$Gender, df$f_Response)
tab
chisq.test(tab) # p = .19 > .05 not significant
row_pct <- prop.table(tab, margin = 1) * 100
row_pct["female", "high"] #32.62%
row_pct["male", "high"] #25.82%



