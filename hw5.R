# Nathan Neill, UNI: nn2401 
# Homework 5, Linear Regression Models

# clear

rm(list=ls())

# import data

library(readr)
BWT <- read_csv("C:/Users/Nathan/Desktop/Linear Regression Models/BWT.csv")
View(BWT)
attach(BWT)

# packages
install.packages('nlme')               # For data 'Orthodont' and gls()
install.packages('lme4')               # Package for mixed models, newer version of 'nlme'     
install.packages('afex')
install.packages('lattice')    
install.packages('reshape2')
install.packages('dplyr')
install.packages('ggplot2')

library(nlme)
library(lme4)
library(afex)
library(lattice)
library(reshape2)
library(dplyr)
library(ggplot2)
library('MASS')                           # for lm.ridge()
library('glmnet')                         # for glmnet()
library(reshape2)

# linear regression

firstbwt <- lm(bwt ~., data=BWT)
summary(firstbwt)
alias(firstbwt)

cor(BWT)

# drop variables because of collinearity, also they're literally 0 all the way
afinal <- subset(BWT, select = -c(pnumlbw, pnumsga, wtgain, malform, ppbmi))


# linear regression
# full model

finmoda <- lm(bwt ~., data=afinal)
summary(finmoda)


# hw5 from lecture this morning. Don't prune based on p-values, look at the 
# data itself and make decisions. For example, no ned to put pre-birth,
# post-birth and weight change into model. Also height, weight, bmi

# make sure to use new uploaded data for problem 2

# Try a grid of values for lambda: from 10^-2 to 10^10

grid<-10^seq(10,-2, length=100)

# Matrix of 100X8 containing coefficients for all 100 values of lambda
ridge2 <- lm.ridge(bwt ~., data=afinal, lambda=grid)
dim(coef(ridge2))

Y = afinal$bwt

X = as.matrix(afinal[,-4])

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

lasso.pred=predict(lasso1,s=best.lambda,newx=X[test,])
mean((lasso.pred-Y.test)^2)

# Fit a model with all observations with the best lambda
lasso2<- glmnet(X,Y, alpha =1, lambda=best.lambda)

coef(lasso2)

# estimated coefficients vs loglambda
### Using the entire data, fit Lasso regressions using the lambda grid.
lasso3 <- glmnet(X,Y, alpha=1, lambda=grid)
### Save the estimated 'standardized' coefficients for all 7 predictors
### without the intercept that is not of interest.
coef_lasso3 <- coef(lasso3)[-1,]
### transpose the matrix
coef_lasso3_mat <- t(as.matrix(coef_lasso3))
### rename rows by grid
rownames(coef_lasso3_mat) <- grid
### sort the matrix by ascending grid
coef_lasso3_mat_sort <- coef_lasso3_mat[order(grid),]
### plot the estimated 'standardized' coefficients for all 7 predictors as functions of lambdas
### Use different colors for each variable 
matplot(coef_lasso3_mat_sort,type="l",lty=1,xlim=c(0,50),
        xlab="lambda",ylab="coefficient",col=1:12) 
### add legend
legend('bottomright', inset=.05, legend=colnames(coef_lasso3_mat_sort), 
       pch=20, cex=0.8, col=1:12)
title('Estimated coefficients vs. log(Lambda)')



# stepwise model selection, 1.d
model2 <- step(firstbwt, direction='backward')

summary(model2)
# 13 predictor variables


# beginning of question 2
rm(list=ls())

library(readr)
FEV1 <- read_csv("C:/Users/Nathan/Desktop/Linear Regression Models/FEV1.csv")
View(FEV1)

install.packages('nlme')               # For data 'Orthodont' and gls()
install.packages('lme4')               # Package for mixed models, newer version of 'nlme'     
install.packages('afex')
install.packages('lattice')    
install.packages('reshape2')

str(FEV1)
summary(FEV1)

# Individual profiles or spaghetti plots
library(lattice)
xyplot(FEV ~ patient, group=drug, data=fev_long, type='l')

# By gender
# xyplot(distance~age|Sex, group=Subject, data=Orthodont, type='l')




# big plot

par(mar=c(4.5,4.5,2,1))
plot(c(0,9),c(0,5),type="n", xlab=" ",cex.lab=1.2,xaxt="n",cex.axis=1.25, ylab="FEV")

lines(1:9, FEV1[1,2:10], pch=16, col='grey60',lwd=1.5, type="o", cex=1) 

for(i in 2:27){
  lines(1:9, FEV1[i,2:10], col='grey60',lwd=1.5, pch=16, type="o", cex=1) 
}

axis(side=1,at=c(1,2,3,4,5,6,7,8,9),labels=c('Baseline', '1', '2', '3', '4', '5', '6', '7', '8'), cex.axis=1.25)

# Save the mean weight for each timepoint and add an overlaid line

mean_w<-NULL

for (i in 1:9){
  mean_w[i]<-mean(FEV1[,i+1])              
}          

mean_w

# Super-impose the mean profile to the plot
lines(1:9, mean_w, lwd=3.5)
title("FEV vs. Time")
#dev.off()

fev_long <- reshape(FEV1, 
                    idvar="patient",
                    varying = c("fev11", "fev12", "fev13", "fev14", "fev15", "fev16", "fev17", "fev18"), 
                    v.names = "FEV",
                    timevar = "Time", 
                    times = c(1, 2, 3, 4, 5, 6, 7, 8), 
                    new.row.names = 1:576, 
                    direction = "long")

fev_long<-fev_long[order(fev_long$patient),]
fev_long[1:10,]

summary(fev_long)

# 2b

# Random slope and intercept model
library(lme4)
# Random intercept/slope and the other covariates: use ML
lm.randintslopecov<-lmer(FEV~basefev1+drug+Time+drug*Time+(Time|patient), data=fev_long, REML=F)

# Same thing
#lm.randintslopecov<-lmer(distance~age+Sex+age*Sex+(1+age|Subject), data=Orthodont, REML=F)

summary(lm.randintslopecov)

# Get the p-values 
library(afex)
coefs.pKen<-mixed(FEV~basefev1+drug+Time+drug*Time+(Time|patient), data=fev_long)
coefs.pKen

# Use Satterthwaite approximation
anova(lm.randintslopecov)

# CI for fixed effects
CI.est<-confint(lm.randintslopecov, method="Wald")

# Get the variance-covarinace matrix for fixed effects
vcov(lm.randintslopecov)

# Get the variance-covariance matrix for random effects
VarCorr(lm.randintslopecov)







# Compound symmetry
fit.compsym <- gls(FEV~basefev1+factor(drug)+Time+factor(drug)*Time+(Time|patient), data=fev_long, 
                   control = list(singular.ok = TRUE),corr=corCompSymm(, form= ~ 1 | patient))
summary(fit.compsym)
anova(fit.compsym)

# Unstructured
fit.unstr <- gls(FEV~basefev1+factor(drug)+Time+factor(drug)*Time+(Time|patient), data=fev_long, corr=corSymm(, form= ~ 1 | patient), 
                 control = list(singular.ok = TRUE),weights = varIdent(form = ~ 1 | Time))
summary(fit.unstr)
anova(fit.unstr)

# Autoregressive order 1: AR(1)
fit.ar1 <- gls(FEV~basefev1+factor(drug)+Time+factor(drug)*Time+(Time|patient), data=fev_long, 
               control = list(singular.ok = TRUE),corr=corAR1(, form= ~ 1 | patient))
summary(fit.ar1)
anova(fit.ar1)

# Use AIC to choose the 'best' variance-covariance matrix.

# What if we treat time as a continuos predictor and later fit a polynomial order 2?

fit.unspolytime1 <- gls(FEV~factor(drug)*Time, data=fev_long, corr=corSymm(, form= ~ 1 | patient),weights = varIdent(form = ~ 1 | Time), method="ML")
summary(fit.unspolytime1)
anova(fit.unspolytime1)

fit.unspolytime2 <- gls(FEV~factor(drug)*poly(Time,degree=2), data=fev_long, corr=corSymm(, form= ~ 1 | patient),weights = varIdent(form = ~ 1 | Time), method="ML")
summary(fit.unspolytime2)
anova(fit.unspolytime2)

# compare the order 1 and order 2 polynomial models
anova(fit.unspolytime1,fit.unspolytime2)



