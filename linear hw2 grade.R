# R code for Nathan Neill, linear regression models hw2

# Problem 4
# clear workspace
rm(list=ls())


# Load libraries
library(faraway)
library(broom)
library(dplyr)
library(readr)
library(ggplot2)

# read data
data("Puromycin")
attach(Puromycin)

# visualize distributions of each variable
boxplot(Puromycin$conc, xlab = "Concentration", ylab = "ppm")
hist(Puromycin$conc, xlab = "Concentration (ppm)", ylab = "count")

boxplot(Puromycin$rate, xlab = "Rate", ylab = "counts/min/min")
hist(Puromycin$rate, xlab = "Rate (counts/min/min)", ylab = "count")

#boxplots of state vs conc and state vs rate

ggplot(Puromycin, aes(x = state, y = rate, fill = state)) + geom_boxplot()
ggplot(Puromycin, aes(x = state, y = conc, fill = state)) + geom_boxplot()


# simple linear regression
puroreg <- lm(rate ~ conc)
summary(puroreg)

# scatterplot with line
plot(conc, rate)
abline(lm(rate~conc))


# detach dataset
detach(Puromycin)




# problem 5
# clear workspace
rm(list=ls())


# Load libraries
library(faraway)
library(broom)
library(dplyr)
library(readr)


# read data
Grade <- read_csv("C:/Users/Nathan/Desktop/Linear Regression Models/Grade.csv")
View(Grade)


# creating grade as a data frame to use for prediction later on
grade = read.csv("C:/Users/Nathan/Desktop/Linear Regression Models/Grade.csv")
names(grade) = c("GPA", "ACT")
attach(grade)
lmgrade = lm(GPA~ACT)
summary(lmgrade)





# obtain least squares estimates, reg line
reg_grade <- lm(Grade$GPA~Grade$ACT)
summary(reg_grade)
tidy(reg_grade)

# plot the data and regression line
plot(Grade$ACT, Grade$GPA)
abline(lm(Grade$GPA~Grade$ACT))

# confidence intervals for the slope
# 95%
confint(reg_grade)
# 90%
confint(reg_grade, level = 0.9)

# point estimate and CI for ACT = 30
predict(lmgrade, data.frame(ACT = 30), interval = "predict")
predict(lmgrade, data.frame(ACT = 30), interval = "confidence")






# CI of change in mean response with 5 points difference
coef<-summary(reg_grade)$coefficients[2,1] 
err<-summary(reg_grade)$coefficients[2,2] 
slope_int<-5*coef + c(-1,1)*(5*err)*qt(0.975, 118)
slope_int

# point estimate is average of lower and upper bound of CI





# problem 6
# clear workspace
rm(list=ls())


# Load libraries
library(faraway)
library(broom)
library(dplyr)
library(readr)

# read grocery data 
library(readr)
Grocery <- read_csv("C:/Users/Nathan/Desktop/Linear Regression Models/Grocery.csv")
View(Grocery)
attach(Grocery)


# create individual plots
# cases
plot(Grocery$Cases, Grocery$Labor_hours)
abline(lm(Grocery$Labor_hours~Grocery$Cases))

# indirect cost
plot(Grocery$Indirect_cost, Grocery$Labor_hours)
abline(lm(Grocery$Labor_hours~Grocery$Indirect_cost))

# boxplot for holiday
boxplot(Grocery$Labor_hours~Grocery$Holiday, xlab = "Holiday Status", ylab = "Labor Hours")


# simple linear regressions
# cases slr
casesreg <- lm(Grocery$Labor_hours~Grocery$Cases)
summary(casesreg)
tidy(casesreg)


# indirect cost slr
indcostreg <- lm(Grocery$Labor_hours~Grocery$Indirect_cost)
summary(indcostreg)
tidy(indcostreg)

# holiday slr
holidayreg <- lm(Grocery$Labor_hours~Grocery$Holiday)
summary(holidayreg)
tidy(holidayreg)


# fit multiple linear regression
mlr <- lm(Grocery$Labor_hours~ Grocery$Cases + Grocery$Indirect_cost + Grocery$Holiday)
summary(mlr)


# coefficient of determination 
summary(mlr)$r.squared

# prediction interval question
grocery = read.csv("C:/Users/Nathan/Desktop/Linear Regression Models/Grocery.csv")
names(grocery) = c("Hours", "Cases", "Indirect", "Holiday")
attach(grocery)


grocery.lm = lm(Hours~ 
                  + Cases + Indirect + Holiday)

newframe <- data.frame((Cases = 282),
                       + (Indirect = 7.10),
                       + (Holiday = 0))

# actual interval generation
predict(grocery.lm, newframe, interval="prediction")














