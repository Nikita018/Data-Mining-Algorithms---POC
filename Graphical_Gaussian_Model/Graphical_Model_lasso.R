#####################################################################
## This code demonstrates Gaussian Graphical Model (GGM) inference ##
## in R using the glasso package                                   ##
## Author: Nikita Goswami                                                       ##
## Created:  4 May 2020                                             ##
#####################################################################
rm(list = ls())
graphics.off()
setwd("~/R workingDir/BayesianNetwork/")

######################################################
## See: http://cran.r-project.org/web/views/gR.html
##################################################
library(gRbase) #CRAN
library(gRim) #CRAN
library(gRain) #CRAN
library(glasso) #CRAN
library(graph) #Bioconductor
library(corrplot) #CRAN

data(BodyFat)
head(BodyFat)
# Graphical Lasso Assumption : Data is continous and Gaussian. We see that this data is continous.


M <- cor(BodyFat)
X11()
corrplot(M)
# Density is negatively correlated with BodyFat which cannot be true so taking it out
# Look at the distribution of each variable and ascertain that they are fairly gaussian
dats <- BodyFat[ , -1]

# Fit a PCA model
fit.pca <- prcomp(scale(dats))
# Let's look at the biplot
xlim_1 <- min(fit.pca$x[,1])-1
xlim_2 <- max(fit.pca$x[,1])+1
ylim_1 <- min(fit.pca$x[,2])-1
ylim_2 <- max(fit.pca$x[,2])+1

X11()
biplot(fit.pca, choices = c(1,2), scale = 0, xlim = c(xlim_1, xlim_2), ylim = c(ylim_1, ylim_2))

# Figured out 2 outliers using biplot, let's remove it from the dataset now
new_dats <- dats[-c(39,42), ]

# Look at partial correlation
# Step 1: Create weighted covariance matrix
S.body <- cov.wt(new_dats, method = "ML")
# convert covariance to correlation
PC.body <- cov2pcor(S.body$cov)
diag(PC.body) <- 0

X11()
heatmap(PC.body)

# Use the graphical lasso package to learn GGMs
install.packages("glasso")
library(glasso)

# Estimate a single graph
S <- S.body$cov
#Eglasso function : estimates a sparse inverse covariance matrix using a lasso (L1) penalty
m0.lasso <- glasso(S, rho = 5) # fit the model
names(m0.lasso)
my.edges <- m0.lasso$wi != 0 # grab the non-zero edges
diag(my.edges) <- 0 # kill the diagonal
g.lasso <- as(my.edges, "graphNEL") # converting to a graphNEL object for plotting
nodes(g.lasso) <- names(new_dats)

X11()
plot(g.lasso)

# Take this "single graph" and put it into a loop, to iterate over different rhos.
install.packages("geneplotter")
library(geneplotter)
graphics.off()
my_rhos <- c(2,5,10,15,25,50)
m0.lasso <- glassopath(S, rho = my_rhos)
for (i in 1:length(my_rhos)){
    my.edges <- m0.lasso$wi[ , , i] != 0 # grab the non-zero edges of the ith object
    diag(my.edges) <- 0 # kill the diagonal
    g.lasso <- as(my.edges, "graphNEL") # converting to a graphNEL object for plotting
    nodes(g.lasso) <- names(new_dats)

    X11()
    plot(g.lasso)
    savepdf(paste("myplot", i, sep = "_"))
 }

 





































































