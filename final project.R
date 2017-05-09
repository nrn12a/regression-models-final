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
plot(combinedsalary)
summary(combinedsalary)

# subset different departments

biochem <- subset(suit, Dept == '1')
physiology <- subset(suit, Dept == '2')
genetics <- subset(suit, Dept == '3')
pediatrics <- subset(suit, Dept == '4')
medicine <- subset(suit, Dept == '5')
surgery <- subset(suit, Dept == '6')


plot(Prate, combinedsalary)
abline(lm(combinedsalary ~ Prate))


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
# not significant for biochem

physioreg <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
                + Gender, data=physiology)
summary(physioreg)
physioauto <- step(biochemreg, direction = 'backward')
summary(physioauto)

# not significant for physiology


genereg <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
              + Gender, data=genetics)
summary(genereg)

geneauto <- step(biochemreg, direction = 'backward')
summary(geneauto)
# not significant for genetics

pedreg <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
             + Gender, data=pediatrics)
summary(pedreg)

pedauto <- step(biochemreg, direction = 'backward')
summary(pedauto)
# NS for peds

medreg <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
             + Gender, data=medicine)
summary(medreg)

medauto <- step(biochemreg, direction = 'backward')
summary(medauto)
#NS for medicine

surgreg <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
              + Gender, data=surgery)
summary(surgreg)

surgauto <- step(biochemreg, direction = 'backward')
summary(surgauto)
#NS for surgery

# automatic procedures for all models give rank, exper, cert, clin