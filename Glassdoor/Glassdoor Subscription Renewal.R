rm(list = ls())
#############################################################################
##  Customer Retention Analysis for GlassDoor's Job Slot Product
##
## Nikita Goswami 
## Created: February 23, 2020
## 
#############################################################################


setwd("~/R workingDir/Glassdoor")

#install.packages("corrplot")
library(rpart)

Glassdoor <- read.csv("DS_Intern_Assignment_-_2020.csv", 
                 header = TRUE)

# Divide data into Test and Train
set.seed(123)   # set seed to ensure you always have same random numbers generated
smp_size = floor(0.75*nrow(Glassdoor)) 
train_gd = sample(seq_len(nrow(Glassdoor)),size = smp_siz)  # Randomly identifies therows equal to sample size ( defined in previous instruction) from  all the rows of Smarket dataset and stores the row number in train_ind
train =Glassdoor[train_gd,] #creates the training dataset with row numbers stored in train_ind
test=df[-train_gd,]  # creates the test dataset excluding the row numbers mentioned in train_ind



# Isolating Predictors and Response
x_train<- train[6:12]
x_test <- test[6:12]


### Fitting Logistic Regression
model <- glm(Renewal_Flag ~.,family=binomial,data=Glassdoor)
summary(model)
# Removing Insignificant variable Job Slot

model <- glm(Renewal_Flag ~ Total_Contract_Value+Applications+Click_Market_Value+Job_Listings,family=binomial,data=x_train)
summary(model)
# Click_Market_Value, Job_Listing, Total_Contract_value, Applications are significant predictors of Renewal
# Intercept and Click_Market_Value are the only predictors with positive beta estimate.
# We can infer that Click_Market_rate affects the Subscription Renewal the most. 

# Let's test the predictive ability of our model
fitted.results <- predict(model,newdata=x_test,type='response')
fitted.results <- round(fitted.results)

misClasificError <- mean(fitted.results != x_test$Renewal_Flag)
print(paste('Accuracy',1-misClasificError))
# Accuracy = 70%

# A classification tree can help us see what are the important variables affecting Subscription Renewal and by how much
Glassdoor$Renewal_Flag <- as.factor(Glassdoor$Renewal_Flag)
model.control <- rpart.control(minsplit = 50, xval = 10, cp = 0.01)
tree <- rpart(Renewal_Flag~., data = Glassdoor, method = "class", control = model.control)

### plotting the tree
x11()
plot(tree, branch = .4, uniform = T, compress = T)
text(tree, use.n = T, all = T, cex = 0.55)

# Let's check the Error
my_pred <- predict(tree, newdata = x_test, type = "class")
misclass_tree <- mean(my_pred != x_test$Renewal_Flag)
misclass_tree # 0.26

### Using Complete Data to fit Classification Tree
GlassD <- Glassdoor[6:12]
model.control <- rpart.control(minsplit = 200, xval = 10, cp = 0.01)
t <- rpart(Renewal_Flag~., data = GlassD, method = "class", control = model.control)

### plotting the tree
x11()
plot(t, branch = 0.5, uniform = T, compress = T)
text(t, use.n = T, all = T, cex = 0.675)

# Let's check the Error
my_pred <- predict(tree, newdata = GlassD, type = "class")
misclass_tree <- mean(my_pred != x_test$Renewal_Flag)
misclass_tree # 0.37


