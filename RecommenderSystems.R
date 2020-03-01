######################################################
### Recommender Lab
### Created by : Nikita Goswami
### Created: 3/1/2020
###
######################################################
rm(list = ls())

setwd("~/R workingDir/Recommender Systems")

#install.packages("recommenderlab")
library(recommenderlab)
citation("recommenderlab")
ls("package:recommenderlab")

# Read in the MovieLens data
Ratings <- read.delim("user_ratedmovies.dat", sep = "\t")
head(Ratings) # 855598 x 9
dim(Ratings)
length(unique(Ratings$userID)) #2113 distinct users
length(unique(Ratings$movieID)) #10109 distinct movies

# Create a "realRatingMatrix
R <- as(Ratings, "realRatingMatrix")

# Get the Rating Matrix
?getRatingMatrix
dim(getRatingMatrix(R))
getRatingMatrix(R)[1:10, 1:10]

# Normalization of the ratings matrix
R_Normalize <- normalize(R)
R_Normalize

#################################################
## Visualize raw ratings and normalized ratings
#################################################
quartz()
image(R_Normalize[1:100,1:100], main = "Normalized ratings")

quartz()
image(R[1:100, 1:100], main = "Raw Ratings")

getRatingMatrix(R_Normalize)[1:10, 1:10]

#de-normalize
R_denormalize <- denormalize(R_Normalize)

# Create a Binary Matrix
R_binarize <- binarize(R_denormalize, minRating = 4)
getRatingMatrix(R_binarize)

quartz()
image(R_binarize[1:100,1:100], main = "Binarized ratings")

# Visualize the ratings in the form of a histogram
quartz()
hist(getRatings(R_Normalize), breaks = 100, main = "Histogram of normalized ratings")

quartz()
hist(rowCounts(R_Normalize), breaks = 100, main = "ratings given by users")

quartz()
hist(colCounts(R_Normalize), breaks = 100, main = "count of ratings per movie")

######################################
## Create a recommender system
######################################
?recommenderRegistry
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

recommender_popularity <- Recommender(R[1000:1], method = "POPULAR")
names(getModel(recommender_popularity))
getModel(recommender_popularity)$topN

# Create top 10 recommendations for 3 users
recom <- predict(recommender_popularity, R[1000:1002], n=10)
recom
as(recom, "list")

# extract sublists
Recom3 <- bestN(recom, n = 3)
Recom3
as(Recom3, "list")

# Predict the ratings for three users
user_ratings <- predict(recommender_popularity, R[1000:1003], type = "ratings")
user_ratings
as(user_ratings, "matrix")[,1:10]

# another way to get to traings
predict_ratings <- predict(recommender_popularity, R[1000:1003], type = "ratingMatrix")
predict_ratings
as(predict_ratings, "matrix")[,1:10]


###########################################################
#### Now, evaluation of the predicted ratings
##### splitting the data into train and test
###########################################################
dim(R)
data_sample <- sample(R, 1000)
dim(data_sample)
eval<- evaluationScheme(data_sample,method = "split", given = 15, train=0.5, goodRating=4)
eval

##########################################################
### Building a recommender model using user based collaborative filtering
#######################################################
userbased_model<- Recommender(getData(eval,"train"), "UBCF")
userbased_model

################################################################
### Building a model using item based collaborative filtering
###ITEM based collaborative filtering is computationally intensive on my system due to low ram but however i am mentioning the way to do IBCF, which is similar to UBCF.
#######################################################################
# itembased_model<-Recommender(getData(eval, "train"),"IBCF")
# itembased_model
###############################################################################
### predicted ratings using model developed by UBCF
###############################################################################
?predict
P1<- predict(userbased_model, getData(eval, "known"), type="ratings")

################################################################################
### calculating the error between prediction and the unknown part of test set
################################################################################
?calcPredictionAccuracy
ERROR<- rbind(UBCF = calcPredictionAccuracy(P1, getData(eval,"unknown")))
ERROR

#################################################################################
### evaluation of top-N recommender algorithm using the Given-3 protocol
###i.e, for the test users all but 3 are withheld for evaluation.
#################################################################################
scheme<- evaluationScheme(data_sample, method="cross",k=4, given=3, goodRating=4) ##?
scheme

###################################################################
### using the created evaluating scheme to evaluate the recommender method popular
#### evaluating the top 1, 3, 5, 10,15,20 recommendation lists
###################################################################################
results<- evaluate(scheme, method = "POPULAR", type="topNList", n=c(1,3,5,10,15,20))
results
getConfusionMatrix(results)[[1]]

#############################################################
### plotting the results, by default we will get ROC curve
####################################################################
x11()
plot(results, annotate=TRUE)
graphics.off()

##################################################################
###precision and recall plot
#####################################################################
x11()
plot(results, "prec/rec", annotate=TRUE)
graphics.off()

##############################################################
## The main function of recommender lab is to compare different recommender algorithms
## now, we shall do that by taking a list of algorithms
#### it is computaionally intensive to allocate vectors for all of these different algorithms
### therefore I am comparing Random and Popular  by commenting the rest.
### you can just uncomment for the user-based, item based, SVD algorithms to work
### this memory problem is caused because I have already so many objects in the global environment with different sizes on top of that my system has less RAM.
##################################################################
set.seed(2018)
scheme_1<- evaluationScheme(data_sample, method="split", train=0.5, k=1, given=-5, goodRating=4)
scheme_1
algorithms<- list("random items" = list(name= "RANDOM", param=NULL),
"popular items"= list(name = "POPULAR", param =NULL)
##"user-based CF"= list(name = "UBCF", param = list(nn=50)),
##"item-based CF"= list(name = "IBCF", param = list(k=50)),
## "SVD approximation" = list(name = "SVD", param= list(k=50)) )
)
results_1<- evaluate(scheme_1, algorithms, type="topNList", n=c(1,3,5,10,15,20))
####################################################################################
## Accessing the results individually  can be done
#####################################################################################
names(results_1)
x11()
plot(results_1, annotate=c(1,3), legend="topleft")
x11()
plot(results_1, "prec/rec", annotate = TRUE)

#############################################################
### what will happen when we use 0-1 data
### using given-3 scheme
#############################################################
R_binarize<- R_binarize[rowCounts(R_binarize)>20]
R_binarize
scheme_binarydata<- evaluationScheme(R_binarize[1:1000], method = "split", train=0.5, k=1, given=3)
scheme_binarydata
results_binarydata<- evaluate(scheme_binarydata, algorithms, type="topNList", n=c(1,3,5,10,15,20))

###################################################################################################
#### recommender "random items" has failed and has been removed from the results
#### The reason according to me is may be due to a problem in memory allocation
#########################################################################################
x11()
plot(results_binarydata, annotate =c(1,3))
graphics.off()










