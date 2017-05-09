# Nathan Neill, UNI: nn2401
# Linear Regression Models Final Project


rm(list=ls())


# load data
library(readr)
suit <- read_csv("C:/Users/Nathan/Desktop/Linear Regression Models/Lawsuit.csv")
View(suit)
attach(suit)
names(suit)
plot(Exper)
plot(Prate)

# other packages
library(ggplot2)

# some other variables might also be adjusted by discrimination,
# for example rank

# make new variable for total salary
combinedsalary <- Sal94 + Sal95
suit <- cbind(suit, combinedsalary)

summary(suit)
names(suit)
cor(suit)
# very high correlation between prate and being research vs. clinical

plot(combinedsalary)
summary(combinedsalary)
hist(combinedsalary)

# subset different departments

biochem <- subset(suit, Dept == '1')
physiology <- subset(suit, Dept == '2')
genetics <- subset(suit, Dept == '3')
pediatrics <- subset(suit, Dept == '4')
medicine <- subset(suit, Dept == '5')
surgery <- subset(suit, Dept == '6')


plot(Prate, combinedsalary)
abline(lm(combinedsalary ~ Prate))

plot(Exper, combinedsalary)
hist(Exper)
# skewed

log_exper <- log(Exper)
hist(log_exper)
plot(log_exper, combinedsalary)

# more normal

hist(Dept)
# a lot more of dept 5

hist(Prate)
# not completely normal, but not obviously skewed in any direction
boxplot(Rank)

# first look at regression
first <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
            + Gender + Dept)
summary(first)


# automatic procedures
automatic <- step(first, direction='backward')

summary(automatic)

AIC(automatic)

# gender does matter, department plays a huge role, so stratify by that
# by department
biochemreg <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
                 + Gender, data=biochem)
summary(biochemreg)
bioauto <- step(biochemreg, direction = 'backward')
summary(bioauto)

# rank, exper, cert, clin
# not significant for biochem

physioreg <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
                + Gender, data=physiology)
summary(physioreg)
physioauto <- step(physioreg, direction = 'backward')
summary(physioauto)

# rank, exper, cert, clin

# not significant for physiology


genereg <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
              + Gender, data=genetics)
summary(genereg)

geneauto <- step(genereg, direction = 'backward')
summary(geneauto)

# just rank, prate, cert!!
# clin NS for genetics


# not significant for genetics

pedreg <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
             + Gender, data=pediatrics)
summary(pedreg)

pedauto <- step(pedreg, direction = 'backward')
summary(pedauto)

# rank, exper, prate, cert
# clinical not sig for peds

# NS for peds

medreg <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
             + Gender, data=medicine)
summary(medreg)

medauto <- step(medreg, direction = 'backward')
summary(medauto)

# rank, exper, cert, clin
#NS for medicine

surgreg <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
              + Gender, data=surgery)
summary(surgreg)

surgauto <- step(surgreg, direction = 'backward')
summary(surgauto)

#rank, exper, cert, clin
#NS for surgery




# tomorrow: look at hw3 for more ideas

