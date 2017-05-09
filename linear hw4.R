# Nathan Neill hw4, UNI: nn2401

rm(list = ls())


# install packages
install.packages('dplyr')
install.packages('HH')                 # For VIF function
install.packages("leaps")              # Stepwise and test-based criteria
install.packages("boot")

library(dplyr)
library(HH)
library("leaps")
library(tidyr)
library(faraway)
library(boot)

# generate data for problem 1

# Code 1
set.seed(1)
x1 <- seq(1,5,length.out = 100)
e1 <- rnorm(100, sd=1.2)
y1 <- 3 - 1.5*x1 + e1
reg1 <- lm(y1~x1)
summary(reg1)

y1



# Code 2
set.seed(1)
sigma <- matrix(c(40,20,20,40), ncol=2)
e2 <- mvtnorm::rmvnorm(100, mean=c(0,0), sigma=sigma)
z <- seq(1,5, length.out = 100)
x2 <- 2*z + e2[,1]
y2 <- 4*x2 + e2[,2]
reg2 <- lm(y2~x2)
summary(reg2)


# two regression models and confidence intervals
summary(reg1)
predict(reg1, newdata=data.frame(x1=mean(x1)), interval="confidence")


summary(reg2)
predict(reg2, newdata=data.frame(x2=mean(x2)), interval="confidence")


# bootstrap to calculate slope estimates
set.seed(1)
mydata <- data.frame(x1=x1, y1=y1)
boot.fn <- function(data, index){
  return(coef(lm(y1~x1, data=mydata, subset=index)))
}

set.seed(1)
boot.fn(mydata,1:100)
boot.fn(mydata, sample(100, 100, replace=T))
boot(mydata, boot.fn, 10000)

data2 <- data.frame(x2 = x2, y2 = y2)
boot.fn2 <- function(data, index){
  return(coef(lm(y2~x2, data=data2, subset=index)))
}
set.seed(1)
boot.fn2(data2, 1:100)
boot.fn2(data2, sample(100,100, replace=T))

boot(data2, boot.fn2, 10000)


# plot sampling distributions
install.packages('dplyr')
library(dplyr)
library(ggplot2)
## define a vector for the bootstrapped estimates
betaHatBS = data.frame(b1.est = rep(NA, 10000))

## use a loop to do the bootstrap
for(i in 1:10000){
  data.cur = sample_frac(mydata, size = 1, replace = TRUE)
  betaHatBS$b1.est[i] = lm(y1 ~ x1, data = data.cur)$coef[2]
}



betaHatBS$b1.null = rnorm(10000, reg1$coef[2], summary(reg1)$coef[2,2])

ggplot(betaHatBS, aes(x = b1.est)) + geom_density(fill = "blue", alpha = .3) + 
  geom_density(aes(x = b1.null), fill = "red", alpha = .3) + theme_bw() +
  ggtitle("Reg1 Slope distribution") + labs(x="Slope Estimate", y="Density")

# plot 2
for(i in 1:10000){
  data.second = sample_frac(data2, size=1, replace=TRUE)
  betaHatBS$b1.est[i] = lm(y2 ~ x2, data = data.second)$coef[2]
}

betaHatBS$b1.null2 = rnorm(10000, reg2$coef[2], summary(reg2)$coef[2,2])

ggplot(betaHatBS, aes(x = b1.est)) + geom_density(fill = "blue", alpha = .3) + 
  geom_density(aes(x = b1.null2), fill = "blue", alpha = .3) + theme_bw() +
  ggtitle("Reg2 Slope distribution") + labs(x="Slope Estimate", y="Density")

# should work



# extra credit problem
# bootstrap t interval
meanboot <- function(data,indices){
  m <- data[indices]
  return(mean(m))
}
output1 <- boot(mydata, meanboot, 10000)

out1 <- boot(mydata, boot.fn, 10000)
out1

boot.ci(out1, index=1) # intercept
boot.ci(out1, index = 2) # slope
?boot.ci


out2 <- boot(data2, boot.fn2, 10000)
boot.ci(out2, index = 2)



# problem 2

# packages
rm(list = ls())

install.packages('dplyr')
install.packages('HH')                 # For VIF function
install.packages("leaps")              # Stepwise and test-based criteria

library(dplyr)
library(HH)
library("leaps")
library(tidyr)
library(leaps)
library(boot)


?state.x77

state = as.data.frame(state.x77)
names(state)
attach(state)

# summary
summary(state)
#sd for each variable
sd(state$Population)
sd(Population)
sd(Income)
sd(Illiteracy)
sd(state$`Life Exp`)
sd(Murder)
sd(state$`HS Grad`)
sd(Frost)
sd(Area)



# alternative way of viewing descriptive stats
install.packages('pastecs')
library(pastecs)
stat.desc(state)


# exploratory scatterplots, correlation
pairs(state)
cor(state)

# histograms, boxplots, etc
par(mfrow=c(1,1))
hist(Population)
hist(Income)
hist(Illiteracy)
hist(state$`Life Exp`)
hist(state$Murder)
hist(state$`HS Grad`)
hist(state$Frost)
hist(state$Area)

boxplot(Population)
boxplot(Income)
boxplot(Illiteracy)
boxplot(state$`Life Exp`)
boxplot(Murder)
boxplot(state$`HS Grad`)
boxplot(Frost)
boxplot(Area)

# transform skewed variables
# Population, Illiteracy, Area



originalfit <- lm(state$`Life Exp` ~ Population + Income + Illiteracy + Murder
                  + state$`HS Grad` + Frost + Area)
anova(originalfit)
plot(fitted(originalfit), resid(originalfit), xlab = "Predicted/Fitted value", ylab = "Residual")
title("(a) Residual Plot for Y (Life Expectancy) ")
abline(0, 0)

# shapiro-wilk for normality

shapiro.test(resid(originalfit))


logPop <- log(Population)
logIll <- log(Illiteracy)
logArea <- log(Area)

transfit <- lm(state$`Life Exp` ~ logPop + Income + logIll + Murder + state$`HS Grad`
               + Frost + logArea)


plot(fitted(transfit), resid(transfit), xlab = "Predicted/Fitted value", ylab = "Residual")
title("Residual Plot for Y (Life Expectancy) with transformed variables ")
abline(0, 0)

# shapiro-wilk for normality

shapiro.test(resid(transfit))



# automatic procedures for best subset
# backwards, forwards, stepwise

#############################################################################
# Backward elimination: take out non-significant variables 'one at a time'  #
# starting with the highest p-value                                         #
#############################################################################

summary(transfit)

# No Income
step1<-update(transfit, . ~ . -Income)
summary(step1)

# No logIll
step2<-update(step1, . ~ . -logIll)
summary(step2)

# No logArea
step3<-update(step2, . ~ . -logArea)
summary(step3)

vif(step3)


# forward elimination
library(tidyr)
library(broom)
names(state)
fit1 <- lm(state$`Life Exp` ~ logPop)
summary(fit1)
tidy(fit1)

fit2 <- lm(state$`Life Exp` ~ Income)
summary(fit2)

fit3 <- lm(state$`Life Exp` ~ logIll)
summary(fit3)

fit4 <- lm(state$`Life Exp` ~ Murder)
summary(fit4)

fit5 <- lm(state$`Life Exp` ~ state$`HS Grad`)
summary(fit5)

fit6 <- lm(state$`Life Exp` ~ Frost)
summary(fit6)

fit7 <- lm(state$`Life Exp` ~ logArea)
summary(fit7)

# Murder = 2.26e-11, logArea = 0.453, Frost = .066, HSGrad = 9.2e-06, logIll = 1.55e-05
# Income = 0.0156, logpop = 0.45
# Order = Murder, HSGrad, logIll, Income, Frost, logPop, logArea

# first forward

forward1 <- lm(state$`Life Exp` ~ Murder)
summary(forward1)

### Step 2: Enter the one with the lowest p-value in the rest 
fit1 <- update(forward1, . ~ . +state$`HS Grad`)
tidy(fit1)
fit2 <- update(forward1, . ~ . +logIll)
tidy(fit2)
fit3 <- update(forward1, . ~ . +Income)
tidy(fit3)
fit4 <- update(forward1, . ~ . +Frost)
tidy(fit4)
fit5 <- update(forward1, . ~ . +logPop)
tidy(fit5)
fit6 <- update(forward1, . ~ . +logArea)
tidy(fit6)

# Enter the one with the lowest p-value: HSGrad
forward2 <- update(forward1, . ~ . + state$`HS Grad`)
tidy(forward2)

fit2 <- update(forward2, . ~ . +logIll)
tidy(fit2)
fit3 <- update(forward2, . ~ . +Income)
tidy(fit3)
fit4 <- update(forward2, . ~ . +Frost)
tidy(fit4)
fit5 <- update(forward2, . ~ . +logPop)
tidy(fit5)
fit6 <- update(forward2, . ~ . +logArea)
tidy(fit6)



# Enter the one with the lowest p-value: Frost
forward3 <- update(forward2, . ~ . + Frost)
tidy(forward3)

### Step 4: Enter the one with the lowest p-value in the rest 
fit2 <- update(forward3, . ~ . +logIll)
tidy(fit2)
fit3 <- update(forward3, . ~ . +Income)
tidy(fit3)
fit5 <- update(forward3, . ~ . +logPop)
tidy(fit5)
fit6 <- update(forward3, . ~ . +logArea)
tidy(fit6)

# Enter the one with the lowest p-value: logPop
forward4 <- update(forward3, . ~ . + logPop)
tidy(forward4)

# Step 5: Enter the one with the lowest p-value in the rest
fit2 <- update(forward4, . ~ . +logIll)
tidy(fit2)
fit3 <- update(forward4, . ~ . +Income)
tidy(fit3)
fit6 <- update(forward4, . ~ . +logArea)
tidy(fit6)

# none of these are significant, so stop. 
summary(forward4)


# do some stepwise stuff
mult.fit <- lm(state$`Life Exp` ~ logPop + Income + logIll + Murder + state$`HS Grad`
                  + Frost + logArea)
step(mult.fit, direction='backward')

summary(forward4)
cor(forward4)
cor(state)

AIC(forward4)




# criterion based procedures

best <- function(model, ...) 
{
  subsets <- regsubsets(formula(model), model.frame(model), ...)
  subsets <- with(summary(subsets),
                  cbind(p = as.numeric(rownames(which)), which, rss, rsq, adjr2, cp, bic))
  
  return(subsets)
} 

# Select the 'best 2 models of all subsets
round(best(mult.fit, nbest = 2), 4)

# Select the 'best 1 model of all subsets
round(best(mult.fit, nbest = 1), 4)

# Checking the model assumptions for the model: Lnsurvival ~ Enzyme + Progindex + Alcheav + Bloodclot
mult.fit <- lm(state$`Life Exp` ~ logPop + Murder + state$`HS Grad` + Frost)
par(mfrow=c(2,2))
plot(mult.fit)



# Residuals vs predictor variables
plot(logPop, mult.fit$residuals)
abline(h=0, lwd=2, col=2)

plot(Murder, mult.fit$residuals)
abline(h=0, lwd=2, col=2)

plot(state$`HS Grad`, mult.fit$residuals)
abline(h=0, lwd=2, col=2)

plot(Frost, mult.fit$residuals)
abline(h=0, lwd=2, col=2)

# remove Hawaii
statenew <- state[-c(11),]
logpopnew <- log(statenew$Population)

mult.fit2 <- lm(statenew$`Life Exp` ~ logpopnew + statenew$Murder + statenew$`HS Grad` + statenew$Frost, data = statenew)
summary(mult.fit2)

# makes frost insignicant
mult.fit3 <- lm(statenew$`Life Exp` ~ logpopnew + statenew$Murder + statenew$`HS Grad`, data = statenew)
summary(mult.fit3)

logIllnew <- log(statenew$Illiteracy)
logAreanew <- log(statenew$Area)

mult.fit4 <- lm(statenew$`Life Exp` ~ logpopnew + statenew$Income
                + logIllnew + statenew$Murder + statenew$`HS Grad`
               + statenew$Frost + logAreanew)
step(mult.fit4, direction='backward')

# adds logAreanew as a significant variable
logareanewreg <- lm(statenew$'Life Exp' ~ logpopnew + statenew$Murder
                    + statenew$'HS Grad' + logAreanew)
summary(logareanewreg)

finalreg <- lm(statenew$'Life Exp' ~ logpopnew + statenew$Murder
                    + statenew$'HS Grad')
summary(finalreg)

# remove frost, don't keep area

# model assumptions
par(mfrow=c(2,2))
plot(finalreg)
title("Final Regression Model Assumptions")



# Cross validation
set.seed(1)
train <- sample(49, 24)

fitnew <- lm(statenew$'Life Exp' ~ logpopnew + statenew$Murder
   + statenew$'HS Grad', subset=train)
summary(fitnew)

# MSPE
mean((statenew$`Life Exp`-predict(fitnew,statenew))[-train]^2)


# k-fold
# Use glm() instead of lm() because of cv.glm() function
glm.fit<-glm(statenew$`Life Exp`~logpopnew + statenew$Murder
             + statenew$'HS Grad', data=statenew)

cv.err<-cv.glm(statenew,glm.fit)
cv.err$delta
warnings()

set.seed(1)
cv.error.10=rep(0,10)
for (i in 1:10){
   glm.fit = glm(statenew$`Life Exp` ~poly(logpopnew + statenew$Murder
                                            + statenew$'HS Grad', i), data=statenew)
   cv.error.10[i] = cv.glm(statenew, glm.fit, K=10)$delta[1]
}
cv.error.10

cv.err<-rep(0,4)  
for (i in 1:4){
  glm.fit<-glm(statenew$`Life Exp`~poly(logpopnew + statenew$Murder
                                        + statenew$'HS Grad',i), data=statenew)
  cv.err[i]=cv.glm(statenew,glm.fit,K=10)$delta[1]
}                      
cv.err

library(MPV)

newsummary <- function(model)
{
  list('coefs'    = round(t(summary(model)$coef[, 1:2]), 4),
       'criteria' = cbind('SSE'   = anova(model)["Residuals", "Sum Sq"],
                          'PRESS' = PRESS(model),
                          'MSE'   = anova(model)["Residuals", "Mean Sq"],
                          'Rsq'   = summary(model)$adj.r.squared))
}

newsummary(lm(statenew$`Life Exp` ~ logpopnew + statenew$Murder + statenew$`HS Grad`, statenew))


# split models
state_train<-statenew[train,]
state_test<-statenew[-train,]

newsummary(lm(statenew$`Life Exp` ~ logpopnew + statenew$Murder + statenew$`HS Grad`, state_train))
newsummary(lm(statenew$`Life Exp` ~ logpopnew + statenew$Murder + statenew$`HS Grad`, state_test))

newsummary(lm(statenew$`Life Exp` ~ ., statenew))
newsummary(lm(statenew$`Life Exp` ~ ., statenew[train,]))
newsummary(lm(statenew$`Life Exp` ~ ., statenew[-train,]))




