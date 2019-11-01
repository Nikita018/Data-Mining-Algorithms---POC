############################################
### This code performs subset selection using different methods
### Nikita Goswami
### Created : 23rd Sept
### Edited : 
############################################

rm(list = ls())

# set working directory
setwd("C:/UB/Sem 1/Stat Data Mining/Lab/subset_selection")

################################################################
## Installing and Loading the Library
################################################################
#install.packages("leaps")
library(MASS)
library(leaps)

### Load Data
data(Boston)

#################################################################
### All possible subsets
#################################################################
regfit.full = regsubsets(medv~.,nvmax=13, nbest = 1 ,data= Boston, method="exhaustive")
exhaustive_summary <- summary(regfit.full)
names(exhaustive_summary)

par(mfrow = c(2,2))
plot(exhaustive_summary$rss,xlab="No of variables", ylab = "RSS", type = "l")
plot(exhaustive_summary$adjr2,xlab="No of variables", ylab = "adjr2", type = "l")
plot(exhaustive_summary$cp,xlab="No of variables", ylab = "cp", type = "l")
plot(exhaustive_summary$bic,xlab="No of variables", ylab = "bic", type = "l")


#They both agree that 11 variables is the best
which(exhaustive_summary$cp == min(exhaustive_summary$cp))
which(exhaustive_summary$bic == min(exhaustive_summary$bic))

x11()
plot(regfit.full, scale = "r2")
x11()
plot(regfit.full, scale = "adjr2")
x11()
plot(regfit.full, scale = "Cp")
x11()
plot(regfit.full, scale = "bic")



#####################################################################
### Forward and Backword subset selection
#####################################################################
regfit.fwd <- regsubsets(medv~., data = Boston, nvmax = 13, method = "forward")
regfit.bwd <- regsubsets(medv~., data = Boston, nvmax = 13, method = "backward")
summary(regfit.fwd)
summary(regfit.bwd)



# Examine the best variables
summary(regfit.full)$outmat[5,]
summary(regfit.fwd)$outmat[5,]
summary(regfit.bwd)$outmat[5,]


# Look at the regression models determined by different methods
coef(regfit.full, 7)
coef(regfit.fwd, 7)
coef(regfit.bwd, 7)