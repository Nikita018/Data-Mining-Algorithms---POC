##########################
## Statistical Data Mining 1
## Resampling methods Lab

## Nikita Goswami
## Created : 10/31/2019
## This code performs in sample methods, resampling methods for estimation of error and model selection.
##########################
set.seed(1)
rm(list = ls())
library(ISLR)
library(bootstrap)
library(boot)  
install.packages("bootstrap")
library(leaps)

###########################
# load the data
###########################
data(Auto)
auto <- Auto #392 x 9
dim(auto)
head(auto)


#############################################
#
# The hold out method
# Y variable : mpg
# X variable : horsepower and weight
#
#############################################
### Dividing data into Test and Train

train <- sample(1:nrow(auto), 0.80*nrow(auto))
Y.train <- auto$mpg[train] # 313
X.train <- auto[train,4:5] # 313*2
Y.test <- auto$mpg[-train] # 79
X.test <- auto[-train,4:5] # 79*2

# Fitting Linear model
fit <- lm(mpg ~ horsepower + weight, data = auto[train,])
pred.test <- predict(fit, newdata = X.test)
pred.train <- predict(fit, newdata = X.train)

test.error = (sum((pred.test-Y.test)^2))/(length(Y.test)) # 21.9%
train.error = (sum((pred.train-Y.train)^2))/(length(Y.train)) # 16.8%
test.error
train.error

#############################################
# Utilize the hold-method over a spectrum
# of complexity paramters
#
#############################################
training <- auto[train,1:7] # 79*7
testing <- auto[-train,1:7] # 313*7

reg.fit <- regsubsets(mpg~.,data = training,Y.train,method = "exhaustive")
my_summary <- summary(reg.fit)
names(my_summary)
my_summary$cp
my_summary$bic

which.min(my_summary$cp) #Cp says 2 variables is best
which.min(my_summary$bic) #BIC says 2 variables is best

# Do the selection based on the "hold out method"
select = summary(reg.fit)$outmat
select
class(select) # select is a matrix
train.error.store <- c()
test.error.store <- c()
for (i in 1:6){
  temp <- which(select[i,] == "*")
  temp <- temp + 1
  
  red.training <- training[, c(1,temp)]
  red.testing <- testing[, c(1,temp)]
  
  # Fitting linear models
  red.fit <- lm(mpg~., data = red.training)
  
  pred.train = predict(red.fit, newdata = red.training)
  pred.test = predict(red.fit, newdata = red.testing)
  
  test.error <- (1/length(Y.test))*sum((pred.test - Y.test)^2)
  train.error <- (1/length(Y.train))*sum((pred.train - Y.train)^2)
  
  train.error.store <- c(train.error.store, train.error)
  test.error.store <- c(test.error.store, test.error)}

### Plot the results
# limits for Y
upper = max(train.error.store, test.error.store)
lower = min(train.error.store, test.error.store)


plot(train.error.store, type = "o", lty = 2, col = "blue", ylim = c(lower -1, upper +1) , xlab = "k", ylab = "error", main = "Model Selection")
lines(test.error.store, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue", "red"))

#############################
## Calculate bootstrap predition error 
## for the best models of size "k"
###########################################

# create functions that feed into "bootpred"
theta.fit <- function(X,Y){
  lsfit(X,Y)	
}

theta.predict <- function(fit,X){
  cbind(1,X)%*%fit$coef
}

err.meas <- function(Y,Yhat){
  (Y-Yhat)^2
}

# Create X and Y
X <- auto[,2:7]
Y <- auto[,1]

# Practice, WLOG lets look at a single model
select = summary(reg.fit)$outmat
#temp <- which(select[3,] == "*") # best 3 variable model

#res1 <- bootpred(X[,temp], Y, nboot = 500, theta.fit, theta.predict , err.meas) 

# Generalize it, and search over the best possible subsets of size "k"
error_store <- c()
for (i in 1:6){
  # Pull out the model
  temp <- which(select[i,] == "*")
  
  res <- bootpred(X[,temp], Y, nboot = 50, theta.fit , theta.predict, err.meas) 
  error_store <- c(error_store, res[[3]])
  
}

plot(train.error.store, type = "o", lty = 2, col = "blue", ylim = c(lower -1, upper +1) , xlab = "k", ylab = "error", main = "Model Selection")
lines(test.error.store, type = "o", lty = 1, col = "red")
lines(error_store, type = "o", lty = 3, col = "green")
legend("topright", c("training", "test", "bootstrap .632"), lty = c(2,1), col = c("blue", "red", "green"))


# Model with 2 attributes - weight and year give the best model.Bootstrap method and hold out method both give similar results.
