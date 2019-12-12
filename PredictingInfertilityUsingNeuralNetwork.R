##########################
## Statistical Data Mining 1
## Neural Networks Lab

## Nikita Goswami
## Created : 12/12/2019
## Predicting Infertility after spontaneous and Induced Abortion using Neural Networks
##########################

rm(list = ls())

# set working directory
setwd("C:/UB/Sem 1/Stat Data Mining/Lab/NeuralNetworks")

#install.packages("neuralnet")
library(neuralnet)

data(infert)

nn_fit <- neuralnet(case~age+parity+induced+spontaneous, data=infert, hidden=1, err.fct = 'ce', linear.output = FALSE)

names(nn_fit)
nn_fit$result.matrix

out <- cbind(as.matrix(nn_fit$covariate), nn_fit$net.result[[1]])
colnames(out) <- c("age", "parity", "induced", "spontaneous", "nn_output")

true_class <- infert$case
pred_class <- round(nn_fit$net.result[[1]])
error <- sum(abs(true_class-pred_class))/length(true_class) #0.23

x11()
plot(nn_fit)


#######################################################################
## More than one layer
#######################################################################

nn_2 <- neuralnet(case~age+parity+induced+spontaneous, data=infert, hidden=2, err.fct = 'ce', linear.output = FALSE)
nn_3 <- neuralnet(case~age+parity+induced+spontaneous, data=infert, hidden=c(2,2), err.fct = 'ce', linear.output = FALSE)

X11()
plot(nn_2)

X11()
plot(nn_3)



### Predict
new.output <- compute(nn_2, covariate = matrix(c(22,1,0,0,  
                                                22, 1, 1, 0,
                                                22, 1, 0, 1,
                                                22, 1, 1, 1),
                                              byrow = TRUE, ncol = 4))
predicted_class_new_data <- round(new.output$net.result)
predicted_class_new_data


# Prediction is that 4th person may have infertility.


