######################################################################
### LASSO and RIDGE
### Nikita Goswami
### Created : 23rd sept'19
######################################################################


rm(list = ls())

# set working directory
setwd("C:/UB/Sem 1/Stat Data Mining/Lab/shrinkage")

### Load and Install library
library(ElemStatLearn)
library(glmnet)
library(DataExplorer)

#########################################
## Load prostate data
###################################
data <- data(prostate)
sum(is.na(data))
X <- as.matrix(prostate[,1:8])
Y <- prostate[,9]

##########################################
## Fit a Ridge Regression Model
############################################
ridge.mod <- glmnet(X,Y,alpha=0)
names(ridge.mod)
ridge.mod$offset
# Trying to access Beta values
dim(coef(ridge.mod))
# 9*100 -->  100 

ridge.mod$lambda[10]
coef(ridge.mod)[,10]

# lambda = 365 and the coefficients look pretty small
# lambdas are col and 9 attributes are rows. We should see how small they are but how?
# We can calculate L2 norm for all the coefficients except the intercept
l2_norm <- sqrt(sum(coef(ridge.mod)[2:9,10]^2))
l2_norm
# l2_norm = 0.0069
# Lets look at 50th Lambda

coef(ridge.mod)[,50]
l2_norm <- sqrt(sum(coef(ridge.mod)[2:9,50]^2))
l2_norm
# Important things are shining through on increasing lambda
# Lets check at 100th lambda


##############################################
## Look at different lambdas
#############################################
ridge.mod$lambda[100]
coef(ridge.mod)[,100]
l2_norm <- sqrt(sum(coef(ridge.mod)[2:9,100]^2))
l2_norm
# None of the attributes are set to zero. We are only shrinking

#########################################
## Predict the model for "new value" of lambda
## Predict corfficients for a particular value of Lambda
###########################################
predict.glmnet(ridge.mod, s= 0.0005, type = "coefficient")
# Looks exactly like the least square solution. This is one way of checking
# but it may not always be same as the least square solution

# check for another s value
predict.glmnet(ridge.mod, s= 0.75, type = "coefficient")


##############################################
## Model Selection
###############################################
?cv.glmnet
# Dividing data into k-fold 
# set seed
set.seed(12345)
# set aside training side
train <- sample(1:nrow(X), round(nrow(X)/2))

cv.out <- cv.glmnet(X[train,], Y[train], alpha=0)
# plotting results of CV
plot(cv.out)

names(cv.out) 
# lambda.min is the best lambda based on cross validation
 bestlam <- cv.out$lambda.min
# bestlam = 0.096
 
# Now lets go back to prediction function and predict for this min lambda
 ridge.pred <- predict(ridge.mod, s=bestlam, type = "coefficients")
 
# Predict response for test data
 ridge.pred2 <- predict(ridge.mod, s=bestlam,newx = X[-train,], type = "response")
 
 y_hat <- ridge.pred2
 y_true <- Y[-train]
 test_error <- sum((y_hat - y_true)^2)
 # error = 20.027 (Racheal's was 27. Why is it different every time? Is it because train data is random?)
 
 ###########################################################
 ##
 ## The LASSO
 ###########################################################
 
 lasso.mod <- glmnet(X[train,], Y[train], alpha=1)
 names(lasso.mod)
 plot(lasso.mod)

 
 # Let's look at coefficents
 lasso.mod$lambda[10]
 coef(lasso.mod)[,10]
 # Lasso has eliminated coefficients
 
 
## Best lambda
 cv.out <- cv.glmnet(X[train,], Y[train], alpha=1)
 bestlam <- cv.out$lambda.min
 
 lasso.pred <- predict(lasso.mod, s=bestlam, type = "coefficients")
 
 
 lasso.pred2 <- predict(lasso.mod, s=bestlam, newx = X[-train,], type = "response")
 y_hat <- lasso.pred2
 y_true <- Y[-train]
 test_error_lasso <- sum((y_hat - y_true)^2)
 # test error = 31.32
 # Ridge was doing better in case of performance
 