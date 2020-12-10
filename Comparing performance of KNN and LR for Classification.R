##########################
##Statistical Data Mining 1
##Assignment 1

##Nikita Goswami
##Created : 9/7/2019
##Comparing the classification of Linear regression and k-nearest neighbour classification on zipcode data
##########################

rm(list = ls())

# set working directory
setwd("C:/UB/Sem 1/Stat Data Mining/Assignments/1/ZipCode")

################################################################
## Installing and Loading the Library
################################################################
#install.packages("ElemStatLearn")
#install.packages("class")
library(ElemStatLearn)
library("DT")
library(mclust)
library("ggplot2")
library(lattice)
library(class)


##############################################################
## Exploring data in column format
##############################################################
datatable(data=zip.test, rownames = FALSE, filter = "top")

############################################################
## Checking type of Data
############################################################
class(zip.test)

### Load data into data frame
zip_train<- as.data.frame(zip.train)
zip_test <- as.data.frame(zip.test)

### Checking the data
colnames(zip_test)
head(zip_test)
nrow(zip_test)

### Checking for any NA values in the data
sum(is.na(zip_test))

##############################################################
### Subsetting the data
### Keeping only '2' and '3'
### Separating response attribute data and predictor attribute data for train and test
##############################################################
zip_train_subset <- subset(zip_train,V1 == 2 | V1 ==3)
zip_train_subset_x <- zip_train_subset[,c(-1)]
zip_train_subset_y <- zip_train_subset$V1
zip_test_subset <- subset(zip_test,V1 == 2 | V1 ==3)
zip_test_subset_x <- zip_test_subset[,c(-1)]
zip_test_subset_y <- zip_test_subset$V1


##############################################################
### Fitting KNN model
##############################################################

##############################################################
###KNN function
### Input variables :
###         Train : Data to train the model 
###         Test: Data to test the model
###         Label : Actual value of response variable
###           k : Value of k used
### Output variable:
###         Accuracy : accuracy of prediction
##############################################################
knnplot <- function(train,test,label,k){
KNN <- knn(train, test, zip_train_subset_y, k)
KNN_num = as.numeric(as.character(KNN))
error <- abs(KNN_num - label)
total = length(error)
count <- 0
for (val in error){
  if(val == 0)
    count = count+1
}
error_rate <- (total-count)/total
accuracy <- (1 - error_rate)
return (error_rate)
}

###################################
### Calling the KNN function for different values of K
### K = 1,3,5,7,9,11,13,15
##################################
k_values <- c(1,3,5,7,9,11,13,15)

### Creating vectors for storing accuracy values for train and test dataset
Knn_Test_error = c()
knn_Train_error = c()
num <- 1

### Executing knn model for each value of k for test and train data
for (i in k_values ){
  
  Knn_Test_error[num] <- knnplot(zip_train_subset_x,zip_test_subset_x,zip_test_subset_y,i)
  knn_Train_error[num] <- knnplot(zip_train_subset_x,zip_train_subset_x,zip_train_subset_y,i)
  num <- num +1
}

### Combining K, Test accuracy and Train accuracy values in a data frame
knn_result = cbind.data.frame(k_values,Knn_Test_error,knn_Train_error)
knn_result

##########################################
### Linear Regression
##########################################

### Using lm function to train the linear model
linReg_Train <- lm(V1 ~ ., data = zip_train_subset)
summary(linReg_Train)


###############################################################################
### Predicting values for response variable in test and train dataset using the linear model
### The output of linear regression is a continuous number. Taking all the numbers above 2.5 to be 3
###############################################################################
category_f <- function(x) { if (x > 2.5) 3 else 2 }
predictions_test_lm <- as.character( sapply(predict(linReg_Train, zip_test_subset_x),category_f))
head(predictions_test_lm)
predictions_train_lm <- as.character(sapply(predict(linReg_Train, zip_train_subset_x),category_f))
head(predictions_train_lm )



### Calculating the error and accuracy for train and test dataset
length(as.numeric(predictions_train_lm) )
length(zip_train_subset_y)
Error_train_lm <- abs(zip_train_subset_y - (as.numeric(predictions_train_lm) ))
Error_test_lm <- abs(zip_test_subset_y - (as.numeric(predictions_test_lm) ))


### Function to calculate accuracy  
calculate_error_rate <- function(Data){
total = length(Data)
count = 0
for (val in Data){
    if(val == 0)
      count = count+1
  }
error_rate <- (total-count)/total
accuracy <- (1 - error_rate)
return(error_rate)
}

### FUnction call to calculate error rate for Linear Model
Lr_Train_error <-  calculate_error_rate(Error_train_lm)
Lr_Test_error <- calculate_error_rate(Error_test_lm)

###################################################################################
### Taking error values for KNN and Linear model in a dataframe for comparison
### using ggplot to plot all error rates for comparison
###################################################################################
errors <- cbind.data.frame(knn_result,Lr_Train_error,Lr_Test_error)



ggplot(errors) + geom_line(aes(x=k_values,y=knn_Train_error,color = "knn_Train_error"), show.legend = TRUE)+ 
                 geom_line(aes(x=k_values,y=Knn_Test_error,color = "Knn_Test_error")) + 
                 geom_hline(aes(yintercept=Lr_Train_error, color = "Lr_Train_error")) +
                 geom_hline(aes(yintercept=Lr_Test_error, color = "Lr_Test_error")) + 
                 ylab("Error Rate") 
                                                                                                                                                