##########################
##Statistical Data Mining 1
##Assignment 1

##Nikita Goswami
##Created : 9/7/2019
##Boston Housing Dataset from MASS Library
##########################

rm(list = ls())

# set working directory
setwd("C:/UB/Sem 1/Stat Data Mining/Assignments/1/BostonHousing")

##########################################################################
### Install and load library
#########################################################################
library(MASS)
library(ggplot2)
library(lattice)
library("DT")
library(corrplot)

###########################################################################
### Data Visualization
###########################################################################

dim(Boston)
#Dim : 506*14. Observations are made in 506 neighbourhood of Boston for 14 predictor attributes
datatable(data=Boston, rownames = FALSE, filter = "top")

###################################################################################
# checking for type of attributes in the dataset
###################################################################################
str(Boston)

### Convert int data to num
chas = as.numeric(Boston$chas)
rad = as.numeric(Boston$rad)

###################################################################################
### a) Make a matrix of pairwise scatter plots
###################################################################################
pairs(Boston, main="Pairwise Scatterplots")
#From the scatter plot, we see some correlated variables.
plot(Boston$age,Boston$knox)

####################################################################################
### b) Are any of the predictors associated with the per capita crime rate
####################################################################################
# Plotting a correlation matrix for per capita crime rate to judge the relationship better
Boston_corr = corrplot.mixed(cor(Boston), lower.col = "black", number.cex = .7) 

# Findings
# High level of rad - index of accessibility to radial highways contain the highest level of cri - per capita crime rate in town.
# High level of tax - full-value property-tax rate per $10,000 has a correlation with the per capita crime rate in town
# lsat, nox and ndus are other attributes having some amount of linear relatinship with per capita crime rate.
# Medv and Black have the max inverse relatinship with per capita crime rate


Boston_corr_crim = Boston_corr[,1]
attributes_Ordered = Boston_corr_crim[order(abs((Boston_corr_crim)), decreasing = T)]
att = attributes_Ordered[2:6]

#####################################################################################
# c) Do any of the suburbs of Boston appear to have particularly high crime rates?
#####################################################################################
summary(Boston$crim)
#Highest crime rate is 88.97
range = range(Boston$crim)
# Looking at the range, we see that there are suburbs with negligible crime rate and some other suburbs with very high crime rate 
crim_low = range[1]
crim_high = range[2]
crim_range = (crim_high-crim_low)

# Plotting a histogram to see how many suburbs have high crime rate
hist(Boston$crim, xlab = "Crime rate", labels = T, main = "Crime Rate in Boston")

# Findings
# There are some neighbourhoods where the crime rate is very high

select = subset(Boston, crim > 25)
nrow(select)/nrow(Boston)
# 2% of suburbs in Boston have crime rate above 25%

select = subset(Boston, crim > 50)
nrow(select)/nrow(Boston)
# 0.8% of suburbs in Boston have crime rate above 50%

#################################################################################
# Analysis of Tax Rate in suburbs of Boston
#################################################################################
summary(Boston$tax)
#Highest tax rate is 711
range = range(Boston$tax)
# Looking at the range, we see that there is good difference between tax rates in suburbs os Boston


# Plotting a histogram to see how many suburbs have high crime rate
hist(Boston$tax, xlab = "Full value property-tax rate per $10,000", labels = T, main = "Tax Rate in Boston")

# Findings
# There are many suburbs with high tax rate in Boston

select = subset(Boston, tax < 500)
nrow(select)/nrow(Boston)
# 72% of suburbs in Boston have tax rate below 500

select = subset(Boston, tax > 500)
nrow(select)/nrow(Boston)
# 27% of suburbs in Boston have tax rate above 500

#######################################################################################
# Analysis of Pupil-Teacher ratios in the suburbs of Boston
#######################################################################################
summary(Boston$ptratio)
#Highest pupil_Teacher ratio is 22:1. Lowest pupil teacher ratio is 13:1
range = range(Boston$ptratio)
# Looking at the range, we see that there is good difference between pupil to teacher ratio in suburbs of Boston


# Plotting a histogram to see how many suburbs have high crime rate
hist(Boston$ptratio, xlab = "Pupil Teacher ratio by town", labels = T, main = "Pupil_Teacher ratio in Boston")

# Findings
# There are many suburbs with higher pupil teacher ratio compared to other suburbs

select = subset(Boston, ptratio < 15)
nrow(select)/nrow(Boston)
# 11% of suburbs in Boston have pupil teacher ratio below 15

select = subset(Boston, ptratio > 15)
nrow(select)/nrow(Boston)
# 88% of suburbs in Boston have pupil teacher ratio above 15

#############################################################################
# d) How many suburbs average more than seven rooms per dwelling
############################################################################
summary(Boston$rm)
hist(Boston$rm, xlab = "Average number of rooms per dwelling", labels = T, main = "Average rooms per dwelling")

# More than seven rooms per dwelling
select = subset(Boston, rm > 7)
nrow(select)
# 64 suburbs have more than 7 rooms per dwelling

# More than eight rooms per dwelling
select = subset(Boston, rm > 8)
nrow(select)
# 13 suburbs have more than 8 rooms per dwelling

############################################################################
# Subsetting data to include only more than 8 rooms
############################################################################
select <- subset(Boston,rm > 8)
summary(select)
nrow(select)
# There are 13 suburbs which have more than 8 rooms.
plot(select$rm, select$crim, ylab = "Per capita crime rate", xlab = "Rooms")

# Crime rate is exceptionally high in one of the suburbs with rooms > 8
plot(select$rm, select$age)

# Proportion of owner occupied units built prior to 1940 is more in suburbs with rooms >8
plot(select$rm, select$age, ylab = "owner occupied units built prior to 1940", xlab = "Rooms")

plot(select$rm, select$medv, ylab = "medv", xlab = "Rooms")
# The value of homes is more for suburbs where rooms > 8 but their is an outlier.

