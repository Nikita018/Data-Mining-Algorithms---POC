##########################
## Statistical Data Mining 1
## Home work 1

## Nikita Goswami
## Created : 9/7/2019
## Finding out the desirable features of a cereal with a high ranking
##########################


rm(list = ls())

# set working directory
setwd("C:/UB/Sem 1/Stat Data Mining/Assignments/1")

################################################################################
# Reading CSV data for analysis 
################################################################################
cereal_data <- read.delim("cereal.csv", sep = ",", header = TRUE)
class(cereal_data)
dim(cereal_data)
head(cereal_data)
colnames(cereal_data)

################################################################################
### Installing and Loading useful libraries for analysis
################################################################################
#Create Report using Data Explorer
library(DataExplorer)
# loading library tidyverse for EDA
library(tidyverse)
# Display cereal data as Data table widget for more clarity
library("DT")
# Library for correlation matrix
library(corrplot)
# Library for xyplot
library("lattice")
library(forecast)
library(reshape)


#####################################################################################
### Exploring and Analyzing the cereal data
#####################################################################################
# Check for any missing data
plot_missing(cereal_data)
# There is no missing data

dim(cereal_data)
# Checking for any NA values in data
sum(is.na(cereal_data))
# No NA values present in the data

#Checking a quick EDA report using Data Explorer
DataExplorer::create_report(cereal_data)
#Report shows that 'Kellogs' and 'General Mills' maufacture the most varieties of cereals. Also 'Cold' type of cereals are significantly greater in number than the 'Hot' type.

# Differentiate type of attributes
classVariables = sapply(cereal_data, function(x) class(x))
classVariables
#Name, Manufacturer and Type columns are factor type (qualitative). All the other variables are integer and numeric type(quantitative)

# check summary of dataset
summary(cereal_data)


# Carbohydrates, sugars and Potassium have some missing observations denoted by negative values. 
# Replace negative values with NA
cereal_data$carbo[cereal_data$carbo < 0] <- NA
cereal_data$sugars[cereal_data$sugars < 0] <- NA
cereal_data$potass[cereal_data$potass < 0] <- NA

# Checking summary
summary(cereal_data)
# Negative values have been ommitted

#################################################################################################
# Nutrition is being defined per serving but manufacturers are defining serving with different weight and cups.
# For comparing nutrition, let's Normalize the data per cup
# Dataset containing nutrition values normalized by cup is stored in cereal_data.cup
#################################################################################################
cereal_data.cup <- data.frame( 
  name = cereal_data$name,
  mfr = cereal_data$mfr,
  type = cereal_data$type,
  calories= round(as.numeric(cereal_data$calories) / as.numeric(cereal_data$cups)),
  protein= round(as.numeric(cereal_data$protein) / as.numeric(cereal_data$cups)),
  fat= round(as.numeric(cereal_data$fat) / as.numeric(cereal_data$cups)),
  sodium = round(as.numeric(cereal_data$sodium) / as.numeric(cereal_data$cups)),
  fiber = round(as.numeric(cereal_data$fiber) / as.numeric(cereal_data$cups)),
  carbo = round(as.numeric(cereal_data$carbo) / as.numeric(cereal_data$cups)),
  sugars = round(as.numeric(cereal_data$sugars) / as.numeric(cereal_data$cups)),
  potass = round(as.numeric(cereal_data$potass) / as.numeric(cereal_data$cups)),
  vitamins = round(as.numeric(cereal_data$vitamins) / as.numeric(cereal_data$cups)),
  shelf = cereal_data$shelf,
  weight = as.numeric(cereal_data$weight) / as.numeric(cereal_data$cups),
  rating = cereal_data$rating)


# Display cereal data as Data table widget for more clarity
datatable(data=cereal_data.cup, rownames = FALSE, filter = "top")

#####################################################################################
### Correlation matrix
#####################################################################################
cereal.correlation <- cereal_data.cup %>% select(calories:rating)
cereal.cor.plot <- cor(cereal.correlation, use="complete.obs")
corrplot.mixed(cor(cereal.cor.plot), lower.col = "black", number.cex = .7) 


# From the correlation matrix it is clear that calories,sugar and carbohydrates have a linear relationship
# and they have an inverse relationship with rating
# Protein and fiber are correlated and affect the rating


# Lets look at the correlation data graphically as a barplot to get a better picture of variables which have a positive relationship to Rating
cereals.correlation.bar <- as.data.frame(cor(cereal.cor.plot))
cereals.correlation.bar$Nutrients <- rownames(cereals.correlation.bar)
df1 <- melt(cereals.correlation.bar,"Nutrients")
g1 <- ggplot(df1[ which(df1$variable=='rating' & df1$Nutrients != 'rating'),], aes(x = Nutrients, y=value)) +
  geom_bar( position = "identity", stat = "identity", alpha = .3 ,aes(fill = Nutrients))
g1


# plotting rating v/s shelf
xyplot(rating~shelf, data = cereal_data.cup,type=c("p", "smooth"))
# One cereal on shelf 3 has exceptionally high rating. SOme cereals on shelf 2 have low ratings. 
# No clear cut relationship between shelf and rating seen.

#######################################################################################
### Analyzing the attributes and transforming them as needed inorder to have a linear like relationship with the response variable
#######################################################################################
# Plotting Protein v/s Rating
xyplot(rating~protein, data = cereal_data.cup,type=c("p", "smooth"),xlab = "Protein per cup", ylab = "Rating")
# The plot is linear when protein>8 but few points in that range are having a lot of say in the linear relationship
# Linear regression assumes linear relatinship between expected and independent values, so lets try with square root of protein and plot again
xyplot(rating~sqrt(protein), data = cereal_data.cup,type=c("p", "smooth"),xlab = "Protein per cup", ylab = "Rating")
# The relationship is positive. I will use the sqrt(protein) value to fit my model

# Plotting Fiber and Rating
xyplot(cereal_data.cup$rating~cereal_data.cup$fiber, data = cereal_data.cup,type=c("p", "smooth"),xlab = "Fiber per cup", ylab = "Rating")
# The plot is linear when fiber>15 but few points in that range are having a lot of say in the linear relationship.
# When fiber is between 5 and 10 range, rating is going down but that should not happen ideally. Lets explore more
# To make data suitable for modelling with linear regression, lets take square root of fiber and plot again
xyplot(cereal_data.cup$rating~sqrt(cereal_data.cup$fiber), data = cereal_data.cup,type=c("p", "smooth"), xlab = "Fiber per cup", ylab = "Rating")
# The relationship is positive but not linear

# Cereals with good fiber, potassium and protein have higher ratings
# Cerelas with higher level of Sugar, fat and calories have lower ratings
# Let's look at these variables individually to access their relationship with Rating
ggplot(cereal_data, aes(cereal_data.cup$calories,cereal_data.cup$rating)) + geom_point() + geom_jitter() + geom_smooth(method = 'lm')
ggplot(cereal_data, aes(cereal_data.cup$sugars,cereal_data.cup$rating)) + geom_point() + geom_jitter() + geom_smooth(method = 'lm')
ggplot(cereal_data, aes(cereal_data.cup$carbo,cereal_data.cup$rating)) + geom_point() + geom_jitter() + geom_smooth(method = 'lm')
# As the sugar content in cereal is increasing, the rating is decreasing
# With calories and carbo,we do not have lot of data points in the higher side for these variables to establish the exact relationship


# Plotting Potass v/s Rating
xyplot(cereal_data.cup$rating~cereal_data.cup$potass, data = cereal_data.cup,type=c("p", "smooth"))
# The plot is linear when potass>400 but few points in that range are having a lot of say in the linear relationship. 
# Also there is a dip in rating when protein ranges between 200 ato 400. This counters our assumption of linear relationship
# Linear regression assumes linear relatinship between expected and independent values, so lets try with square root of potass and plot again
xyplot(cereal_data.cup$rating~sqrt(cereal_data.cup$potass), data = cereal_data.cup,type=c("p", "smooth"))
# The relationship is positive

# Plotting Sugars v/s Rating
xyplot(cereal_data.cup$rating~cereal_data.cup$sugars, data = cereal_data.cup,type=c("p", "smooth"))
# The plot is linear when sugars<12 but is flat after that. This shows that after the threshold, an increase in sugar is not decreasing the rating 
# Linear regression assumes linear relatinship between expected and independent values, so lets try with square root of sugars and plot again
xyplot(cereal_data.cup$rating~sqrt(cereal_data.cup$sugars), data = cereal_data.cup,type=c("p", "smooth"))
# Taking square root is giving too much attention to few cereals which have negligible sugar and high rating.
# I would chose to take raw data for sugar to fit in my model

# Plotting Calories v/s Rating
xyplot(cereal_data.cup$rating~cereal_data.cup$calories, data = cereal_data.cup,type=c("p", "smooth"))
# The plot is linear when calories>250 but is flat after that. This shows that after the threshold, an increase in calories is not decreasing the rating 
# Linear regression assumes linear relatinship between expected and independent values, so lets try with square root of calories and plot again
xyplot(cereal_data.cup$rating~sqrt(cereal_data.cup$calories), data = cereal_data.cup,type=c("p", "smooth"))
# The relationship is negative

# Plotting Vitamins v/s Rating
xyplot(cereal_data.cup$rating~cereal_data.cup$vitamins, data = cereal_data.cup,type=c("p", "smooth"))
# The plot shows a decreasing trend and finally a flat line when vitamins>3 . This shows that after the threshold, an increase in vitamins is not decreasing the rating
# The plot is contradicting as it shows higher rating for no vitamins and less rating for high vitamins
# Linear regression assumes linear relatinship between expected and independent values, so lets try with square root of sugars and plot again
xyplot(cereal_data.cup$rating~sqrt(cereal_data.cup$vitamins), data = cereal_data.cup,type=c("p", "smooth"))
# The relationship is negative


# Plotting Fat v/s Rating
xyplot(cereal_data.cup$rating~cereal_data.cup$fat, data = cereal_data.cup,type=c("p", "smooth"))
# For cereals with no fat rating is higher.
# For cereals with fat values, rating is constant

# Plotting Sodium v/s Rating
xyplot(cereal_data.cup$rating~cereal_data.cup$sodium, data = cereal_data.cup,type=c("p", "smooth"))
# For cereals with no sodium, rating is ranging about 50%
# The plot is not linear and we see 2 cereals with high sodium values dictating the linearity of the plot at the end.
# Using square root transformation for a better plot
xyplot(rating~sqrt(sodium), data = cereal_data.cup,type=c("p", "smooth"))

#########################################################################################
# Adding transformed attributes to the dataset
# Saving the dataset as RData file
#########################################################################################
cereal_data.cup$sqrt_sodium = sqrt(cereal_data.cup$sodium)
cereal_data.cup$sqrt_protein = sqrt(cereal_data.cup$protein)
cereal_data.cup$sqrt_fiber = sqrt(cereal_data.cup$fiber)

# Saving the transformed data as an RData file
save(cereal_data.cup, file = "cereal_data.cup.Rdata")


####################################################################################
# Checking for outliers in Data
####################################################################################
boxplot(cereal_data)

#removing categorical variables, weight and rating from the box plot for more clarity
cereal_data1 = subset(cereal_data.cup, select = -c(name,mfr,type,rating, shelf, weight) )
boxplot(cereal_data1)
ggsave(filename = "box_outlier.png")
#We see outliers present in calories,protein,sodium,fiber,carbo,potass,vitamins

# checking for high risk outlier by plotting separate box plots
boxplot(cereal_data.cup$sodium)
boxplot(cereal_data.cup$potass)
boxplot(cereal_data.cup$calories)
boxplot(cereal_data.cup$vitamins)
# I would not be treating them as they look legitimate values and I would not want to bias the data by removing them from my analysis

############################################################################################
### Fitting a Linear Model using lm() function
############################################################################################

##################
# Fitting model using all nutrients
# Model Name : fit1
# Response Attribute : Rating
# Taking all categorical and quantitative attributes to fit the model
##################
fit1 <- lm(cereal_data.cup$rating ~. , data=cereal_data1)
summary(fit1)
# Adjusted R-squared statistics estimates that 94.63% of the changes in the response can be explained by all set of predictors in data.


##################
# Fitting model using transformed attributes
# Model Name : fit2, fit3
# Response Attribute : Rating
##################

# Converting the attribute 'shelf' to a factor type attribute from numeric.
# Shelf as a variable dosent provide any numerical meaning
cereal_data.cup$shelf = as.factor(cereal_data$shelf)

# Subsetting the data
# Name, mfr and type are qualitative type, keeping them aside to fit the model
# We have normalized the data with cup, so neglecting weight attribute to fit the data
cereal_data2 = subset(cereal_data.cup, select = -c(name,mfr,type, weight) )

# Model : fit2
# Response : Rating
# Excluding : Name, shelf, manufacturer and weight
fit2 <- lm(rating ~ sqrt_fiber + sqrt_protein + sqrt_sodium + sugars + fat + calories + carbo + potass + vitamins, data = cereal_data2, na.action=na.omit)
summary(fit2)
# Adjusted R-squared statistics estimates that 93.05% of the changes in the response can be explained by all set of predictors in data.

# Finding
# Calories and carbo attributes do not have significant values. (P value> 0.05).
# Fitting a model removing calories and carbo
fit3 <- lm(rating ~ sqrt_fiber + sqrt_protein + sqrt_sodium + sugars + fat + potass + vitamins, data = cereal_data2, na.action=na.omit)
summary(fit3)
# Adjusted R-squared value is 92.89
# The adj R-squared estimate is decreased when we removed calories and carbo. So we will go ahead with fit1 model 


#######################################################################################
## Fitting models using interation variables to get better results
## Examining the correlation plot to decide on the interaction attributes.
## Model name : Fit4
#######################################################################################


# Interaction between sugar and fat looks promising
# interaction = sugar:fat
fit4 <- lm(rating ~ . +sugars:fat, data = cereal_data2)

# View summary of model fit
summary(fit4)
# The interaction variable is significant based on the P-value
# Adj R-sqaured estimate is 95.54

### Checking if interaction between protein and fiber is significant
### We have already added protein and fiber attribute separately hence using ":" interaction operator
fit5 <- lm(rating ~ . +fiber:protein, data = cereal_data2)

# View summary of model fit
summary(fit5)
# adjusted R-square estimate is 94.74%

########################################################################################
### Adjusted R-squared statistics estimates that 93.05% of the changes in the response can be explained by all set of predictors in data.
### Fiber and Protein have a positive relationship with Rating and look related.
### Sugar and Fat have a positive relationship with Rating and look related.
### Moel : Fit6
########################################################################################

fit6 <- lm(rating ~ sugars:fat +sqrt_fiber + sqrt_protein + sqrt_sodium + sugars + fat + calories + carbo + potass + vitamins + fiber:protein, data = cereal_data2, na.action=na.omit)
summary(fit6)
# Adjusted R-squared statistics estimates that 94.6% of the changes in the response can be explained by all set of predictors in data.
