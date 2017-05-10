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

# look at everything visually
hist(Dept)
hist(Prate)
hist(Exper)
hist(Rank)


# transform exper
# make new variable for total salary
combinedsalary <- Sal94 + Sal95
suit <- cbind(suit, combinedsalary)
suit <- cbind(suit, logcombined)
suit <- cbind(suit, logexper)
View(suit)

hist(combinedsalary)
# maybe transform combined

logexper <- log(Exper)
hist(logexper)

experhist <- ggplot(suit, aes(x=Exper)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth=.5,
                 colour="black", fill="light blue") +
  geom_density(alpha = .2, fill="#FF6666")
experhist + labs(x = "Experience", title = "Distribution of Experience")

logexperhist <- ggplot(suit, aes(x=logexper)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha = .2, fill="#FF6666")
logexperhist + labs(x = "Log Transformation of Experience",
                    title = "Distribution of Log Transformation")




salaryhist <- ggplot(suit, aes(x=combinedsalary)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth=.5,
                 colour="black", fill="light blue") +
  geom_density(alpha = .2, fill="#FF6666")
salaryhist + labs(x = "Combined Salary", title = "Distribution of Combined Salary")

logsalhist <- ggplot(suit, aes(x=logcombined)) + 
  geom_histogram(aes(y = ..density..),
                 binwidth=.5,
                 colour="black", fill="white") +
  geom_density(alpha = .2, fill="#FF6666")
logsalhist + labs(x = "Log Transformation of Combined Salary",
                    title = "Distribution of Log Transformation")

names(suit)
# that's better

summary(suit)
names(suit)
cor(suit)
# very high correlation between prate and being research vs. clinical

plot(combinedsalary)
summary(combinedsalary)
hist(combinedsalary)

logcombined <- log(combinedsalary)
hist(logcombined)
plot(logcombined)

transfit <- lm(logcombined ~ Dept + Gender + Clin + Cert + Prate + logexper +
                 Rank)

summary(transfit)

# everything but clin is significant

# probably use that
# subset different departments

biochem <- subset(suit, Dept == '1')
physiology <- subset(suit, Dept == '2')
genetics <- subset(suit, Dept == '3')
pediatrics <- subset(suit, Dept == '4')
medicine <- subset(suit, Dept == '5')
surgery <- subset(suit, Dept == '6')


plot(Prate, combinedsalary)
abline(lm(combinedsalary ~ Prate))

plot(logexper, logcombined)
hist(Exper)
# skewed

plot(logexper, combinedsalary)

# more normal
# a lot more of dept 5

hist(Prate)
# not completely normal, but not obviously skewed in any direction
boxplot(Rank)

# first look at regression, don't use because of skew
first <- lm(combinedsalary ~ Rank + Exper + Prate + Cert + Clin
            + Gender + Dept)
summary(first)


# automatic procedures
automatic <- step(transfit, direction='backward')

summary(automatic)

AIC(automatic)

# gender does matter, department plays a huge role, so stratify by that
# by department
biochemreg <- lm(logcombined ~ Rank + logexper + Prate + Cert + Clin
                 + Gender, data=biochem)
summary(biochemreg)
bioauto <- step(biochemreg, direction = 'backward')
summary(bioauto)
AIC(biochemreg)
AIC(bioauto)

# rank, logexper, cert, clin
# G not significant for biochem

physioreg <- lm(logcombined ~ Rank + logexper + Prate + Cert + Clin
                + Gender, data=physiology)
summary(physioreg)
physioauto <- step(physioreg, direction = 'backward')
summary(physioauto)
AIC(physioreg)
AIC(physioauto)

# rank, logexper, cert, clin

# not significant for physiology


genereg <- lm(logcombined ~ Rank + logexper + Prate + Cert + Clin
              + Gender, data=genetics)
summary(genereg)

geneauto <- step(genereg, direction = 'backward')
summary(geneauto)
AIC(genereg)
AIC(geneauto)
# just rank, prate, cert!!
# clin NS for genetics


# not significant for genetics

pedreg <- lm(logcombined ~ Rank + logexper + Prate + Cert + Clin
             + Gender, data=pediatrics)
summary(pedreg)

pedauto <- step(pedreg, direction = 'backward')
summary(pedauto)
AIC(pedreg)
AIC(pedauto)

# rank, logexper, prate, cert
# clinical not sig for peds

# NS for peds

medreg <- lm(logcombined ~ Rank + logexper + Prate + Cert + Clin
             + Gender, data=medicine)
summary(medreg)

medauto <- step(medreg, direction = 'backward')
summary(medauto)
AIC(medreg)
AIC(medauto)

# rank, logexper, clin
#NS for medicine

surgreg <- lm(logcombined ~ Rank + logexper + Prate + Cert + Clin
              + Gender, data=surgery)
summary(surgreg)

surgauto <- step(surgreg, direction = 'backward')
summary(surgauto)
AIC(surgreg)
AIC(surgauto)

#rank, logexper, cert, clin
#NS for surgery




# lasso 1
install.packages("lars")
library(lars)

fitlasso <- lars(X, Y, type="lasso")
summary(fitlasso)

# minimum error
best_step <- fitlasso$df[which.min(fitlasso$RSS)]

# predictions
predictionslasso <- predict(fitlasso, X, s=best_step, type="fit")$fitlasso

mse <- mean((Y - predictionslasso)^2)
print(mse)

# lasso technique 2
install.packages('glmnet')
library('MASS')                           # for lm.ridge()
library('glmnet')
# Try a grid of values for lambda: from 10^-2 to 10^10

grid<-10^seq(10,-2, length=100)

# Matrix of 100X8 containing coefficients for all 100 values of lambda
ridge2 <- lm.ridge(combinedsalary ~., data=suit, lambda=grid)
dim(coef(ridge2))

ans <- cv.glmnet(X[test, ], Y[test])
plot(ans)
coef(ans, s=ans$lambda.1se)

Y = suit$combinedsalary

X = as.matrix(suit[,-9:-11])
View(X)
train<-sample(1:nrow(X),nrow(X)/2)

test<-(-train)

Y.test<-Y[test]

lasso1<- glmnet(X[train ,],Y[train], alpha =1, lambda =grid)
#plot(lasso1)

# Cross-validation
set.seed(1)
cv.out<-cv.glmnet(X[train,],Y[train])
plot(cv.out)
title('log(Lambda) vs. CV curve')

best.lambda<-cv.out$lambda.min
best.lambda

# 290.4429

lasso.pred=predict(lasso1,s=best.lambda,newx=X[test,])
mean((lasso.pred-Y.test)^2)

# 5072219850

# Fit a model with all observations with the best lambda
lasso2<- glmnet(X,Y, alpha =1, lambda=best.lambda)


coef(lasso2)

# estimated coefficients vs loglambda
### Using the entire data, fit Lasso regressions using the lambda grid.
lasso3 <- glmnet(X,Y, alpha=1, lambda=grid)
### Save the estimated 'standardized' coefficients for all  predictors
### without the intercept that is not of interest.
coef_lasso3 <- coef(lasso3)[-1,]
coef_lasso3
### transpose the matrix
coef_lasso3_mat <- t(as.matrix(coef_lasso3))
### rename rows by grid
rownames(coef_lasso3_mat) <- grid
### sort the matrix by ascending grid
coef_lasso3_mat_sort <- coef_lasso3_mat[order(grid),]
### plot the estimated 'standardized' coefficients for all predictors as functions of lambdas
### Use different colors for each variable 
matplot(coef_lasso3_mat_sort,type="l",lty=1,xlim=c(0,50),
        xlab="lambda",ylab="coefficient",col=1:12) 
### add legend
legend('bottomright', inset=.05, legend=colnames(coef_lasso3_mat_sort), 
       pch=20, cex=0.8, col=1:12)
title('Estimated coefficients vs. log(Lambda)')


# elastic net
# load the package
# load data

# fit model
elastic <- glmnet(X, Y, family="gaussian", alpha=0.5, lambda=0.001)
# summarize the fit
summary(elastic)
# make predictions
predictions <- predict(elastic, X, type="link")
# summarize accuracy
mse <- mean((Y - predictions)^2)
print(mse)

# lasso is better than elastic based on mse

cvfit <- glmnet::cv.glmnet(X, Y)
coef(cvfit, s = best.lambda)
best.lambda

