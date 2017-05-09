# Nathan Neill homework code 1
# removal
rm(list=ls())

# load data from r
library(readr)
Esoph <- read_csv("C:/Users/Nathan/Desktop/Linear Regression Models/Esoph.csv")
View(Esoph)

# data attached
attach(Esoph)


# packages
library(dplyr)
library(ggplot2)
library(gmodels)
library(gridExtra)

glimpse(Esoph)


# new age categories
Esoph = mutate(Esoph, age_cat = 
                 ifelse(agegp == "25-34" | agegp=="35-44", 1,
                        ifelse(agegp == "45-54" | agegp=="55-64", 2,
                               ifelse(agegp == "65-74" | agegp == "75+", 3, NA))))

# new alcohol categories
Esoph = mutate(Esoph, alcocat = 
                 ifelse(alcgp == "0-39g/day", c(1),
                        ifelse(alcgp == "40-79", c(2),
                               ifelse(alcgp == "80-119", c(3),
                                      ifelse(alcgp == "120+", c(4), NA)))))



#same for tobacco usage

Esoph = mutate(Esoph, tobcat = 
                 ifelse(tobgp == "0-9g/day", c(1),
                        ifelse(tobgp == "10-19/day", c(2),
                               ifelse(tobgp == "20-29", c(3),
                                      ifelse(tobgp == "30+", c(4), NA)))))

# create alcocat vector 4a
unique(Esoph$alcocat)



# create tobacco vector 4b
unique(Esoph$tobcat)



# two new data frames problem 5
data1 <- data.frame(as.vector(Esoph$age_cat), Esoph$ncases, as.vector(Esoph$alcocat))

data2 <- data.frame(as.vector(Esoph$age_cat), Esoph$ncases, as.vector(Esoph$tobcat))



# matrix creation a, use tapply problem 6
matrix1 <- tapply(Esoph$ncases, list(Esoph$alcocat, Esoph$age_cat), sum)

matrix2 <- tapply(Esoph$ncases, list(Esoph$tobcat, Esoph$age_cat), sum)


# plots of the matrices, then adjusting tick marks and placing the graphs on the same row 
x <- c(1,2,3,4)
lab1 <- c("0-39g", "40-79g", "80-119g", "120+" )
lab2 <- c("0-9", "10-19", "20-29", "30+")


#put them on same row
par(mfrow=c(1,2))
par(mar=c(4, 4, 0.5, 0.5))


# actual plots, legend creates legend and removes box
matplot(x, matrix1, type="o", col=c("green", "red", "royalblue"), ylim=c(0,50), xlab = "Alcohol Consumption g/day", ylab="Number of Cases", pch=c(1), xaxt='n')
axis(1,at=x,labels=lab1)
legend("topleft", legend=c("Age 25-44", "Age 45-64", "Age 64+"), pch=c(19,19), col=c("royalblue","red", "green"), lty=c(1,1), lwd=c(1,1), cex=c(.5,.5), bty="n")

# axis in order to changs ticks
                
matplot(x, matrix2, type="o", col=c("green", "red", "royalblue"), ylim=c(0,50), xlab = "Tobacco Consumption g/day", ylab="Number of Cases",pch=c(1), xaxt='n')
axis(1,at=x,labels=lab2)





