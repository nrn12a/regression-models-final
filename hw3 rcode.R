# Nathan Neill, UNI: nn2401. Homework 3, Linear Regression Models


# problem 1

# clear workspace 
rm(list = ls())

# Load libraries
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(splines)


# read heartdisease dataset
library(readr)
heartdisease <- read_csv("C:/Users/Nathan/Desktop/Linear Regression Models/HeartDisease.csv")
View(heartdisease)
attach(heartdisease)

# start descriptive statistics, totalcost
summary(totalcost)
summary(age)
summary(interventions)
summary(drugs)
summary(ERvisits)
summary(complications)
summary(comorbidities)
summary(duration)

# gender is categorical binary
table(gender)
# significantly more of category 0 than category 1

# 2.a create separate histograms
costplot <- ggplot(heartdisease, aes(x = totalcost)) + geom_histogram() + ggtitle("Frequency histogram of total cost")
costplot


erplot <- ggplot(heartdisease, aes(x = ERvisits)) + geom_histogram() +
  ggtitle("Frequency histogram of ER visits")
erplot


# 2.b create log_tot
range(totalcost)
log_tot <- log(totalcost + .000001)
heartdisease <- cbind(heartdisease, log_tot)
range(log_tot)

# 2.c create comp_bin
comp_bin <- ifelse(complications==0,0,ifelse(complications!=0, 1, NA))
table(comp_bin)
heartdisease <- cbind(heartdisease, comp_bin)

# 3 SLR log_tot on ERvisits
simplelogtot <- lm(log_tot ~ ERvisits)
summary(simplelogtot)


# scatterplot
slrplot <- ggplot(data = heartdisease, aes(y=log_tot, x=ERvisits)) + geom_point() +
   theme_minimal() 
slrplot

# 4 fit mlr

multiregtot <- lm(log_tot ~ ERvisits + comp_bin)
summary(multiregtot)
tidy(multiregtot)



# test for effect measure modification
# subset based on complications
comps <- subset(heartdisease, comp_bin == 1)
nocomps <- subset(heartdisease, comp_bin == 0)

# association for those with complications
compslr <- lm(log_tot ~ ERvisits, data = comps)
summary(compslr)

# association for those without complications
nocompslr <- lm(log_tot ~ ERvisits, data = nocomps)
summary(nocompslr)

# also use anova comparison
anova(simplelogtot, multiregtot)

# I tested for confounding by hand, not in R code. 

# add additional covariates, fit a mlr
admlr <- lm(log_tot ~ ERvisits + comp_bin + duration +age + gender)
summary(admlr)


# partial F tests for different models
added <- lm(log_tot ~ ERvisits + comp_bin + duration + gender)
summary(added)
anova(multiregtot, added)

anova(added, admlr)

# comparison of SLR with my model
anova(simplelogtot, added)

# confidence interval
mean(ERvisits)
mean(comp_bin)
mean(duration)
mean(gender)
df <- data.frame(ERvisits = c(3.425), comp_bin = c(.05457), duration = c(164), gender = c(.2284))

predict(added, df, interval = "confidence")
# problem 2 

# clear workspace 
detach(heartdisease)
rm(list = ls())

# Load libraries
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)
library(splines)
library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)

# read commercial properties dataset
library(readr)
commercial <- read_csv("C:/Users/Nathan/Desktop/Linear Regression Models/CommercialProperties.csv")
View(commercial)
attach(commercial)
objects(commercial)

# 1, fit multiple regression model
firstmodel <- lm(Rental_rate ~ Age + Sq_footage + Taxes + Vacancy_rate)
summary(firstmodel)
tidy(firstmodel)
anova(firstmodel)

# 2, create scatterplots

ggplot2.scatterplot(data=commercial, xName = 'Age', yName = 'Rental_rate')

ggplot2.scatterplot(data=commercial, xName = 'Sq_footage', yName = 'Rental_rate')

ggplot2.scatterplot(data=commercial, xName = 'Taxes', yName = 'Rental_rate')

# I think just remove vacancy_rate, everything else is significant

# 3, mlr with significant predictors
sigmodel <- lm(Rental_rate ~ Age + Sq_footage + Taxes)
summary(sigmodel)
tidy(sigmodel)

# 4.a play with age property, try quadratic out
commercial = mutate(commercial, 
                     Rental_rate = Rental_rate, age.pow2 = Age ^ 2, age.pow3 = Age ^ 3,
                    age.pow4 = Age ^ 4, Sq_footage = Sq_footage,
                    Taxes = Taxes)

age.pow2 <- (Age^2)

sqmodel <- lm(Rental_rate ~ Age  + Sq_footage + Taxes + age.pow2, data = commercial)
summary(sqmodel)
summary(sigmodel)
anova(sigmodel, sqmodel)

cubmodel <- lm(Rental_rate ~ age + age.pow3 + Sq_footage + Taxes, data = commercial)
summary(cubmodel)

quarmodel <- lm(Rental_rate ~ age + age.pow4 + Sq_footage + Taxes, data = commercial)
summary(quarmodel)


mutate(commercial, fitted = fitted(sqmodel)) %>%
  ggplot(., aes(y=y, x=x)) + geom_point() + 
  geom_line(aes(y = fitted), color = "red") + theme_bw()


# 4.b try piecewise
# looking at the data, piecewise clearly does not make sense'
# 4.c recommend model
# quadratic model
# 5, test whether it's better than the mlr
summary(sqmodel)
summary(sigmodel)
anova(sigmodel, sqmodel)






