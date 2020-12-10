#####################################################################
## This code demonstrates Bayesian Networks in R                   ## 
## We specify CPD, reason, evidence based prediction               ##
## Creator : Nikita Goswami                                                  ##
## Created: April 21, 2020                                          ##                                           ##
#####################################################################
setwd("~/R workingDir/BayesianNetwork/")
rm(list = ls())
graphics.off()

######################################################
## See: http://cran.r-project.org/web/views/gR.html
##################################################
# This worked for installing packages
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install(version = "3.10")
# source("http://www.bioconductor.org/biocLite.R")
# BiocManager::install(c("gRain", "ggm"))
# biocLite(c("graph", "RBGL", "Rgraphviz"))
# This did not work - install.packages("gRbase")
library(gRain)
#library(RHugin)
library(Rgraphviz)
library(gRbase)
library(ggm)

##################################################
## Specify the DAG
##################################################
g <- list(~asia, ~tub|asia, ~smoke, ~lung|smoke, ~bronx|smoke, ~either|lung:tub, 
	~xray|either, ~dysp|bronc:either)
chestdag <- dagList(g)

##################################################
## Inquire about d-separation
##################################################
dSep(as(chestdag, "matrix"), "tub", "smoke", c("dysp", "xray"))
dSep(as(chestdag, "matrix"), "tub", "lung", "smoke")

##################################################
## Specify the CPD tables
##################################################
yn <- c("yes", "no")
a <- cptable(~asia, values = c(1, 99), levels =yn)
t.a <- cptable(~tub|asia, values = c(5, 95, 1, 99), levels = yn )
s <- cptable(~smoke, values = c(5, 5), levels = yn )
l.s <- cptable(~lung|smoke, values = c(1,9,1,99), levels = yn )
b.s <- cptable(~bronc|smoke, values = c(6,4,3,7), levels = yn)
e.lt <- ortable(~either|lung:tub, levels = yn)
x.e <- cptable(~xray + either, values = c(98, 2, 5, 95), levels = yn)
d.be <- cptable(~dysp|bronc:either, values = c(9,1,7,3,8,2,1,9), levels = yn)

##################################################
## Build the network
##################################################
plist <- compileCPT(list(a, t.a, s, l.s, b.s, e.lt, x.e, d.be))
grn1 <- grain(plist)
summary(grn1)

quartz()
plot(grn1)

##################################################
## Compile the network 
## DAG is created, moralized, and triangularized.
##################################################
grn1c <- compile(grn1)
summary(grn1c)

# if interested in "haulting" the compilation process
g <- grn1$dag
plot(g)
mg <- moralize(g)
plot(mg)
tmg <- triangulate(mg)
plot(tmg)
rip(tmg)

# plot the junction tree
quartz()
plot(grn1c)
plot(grn1c, type="jt")
summary(grn1c)
names(grn1c)

##################################################
## Propagate the the network 
## 
##################################################
grn1c <- propagate(grn1c)

##################################################
## Make Queries
## 
##################################################
# Suppose we have some information, we want to "absorb it"
grn1c.ev <- setFinding(grn1c, nodes = c("asia", "dysp"), states = c("yes", "yes"))

# probabilistic query, given evidence
abs <- querygrain(grn1c.ev, nodes = c("lung", "bronc"), type = "marginal")

# probabilistic query, given NO evidence
not_abs <- querygrain(grn1c, nodes = c("lung", "bronc"), type = "marginal")

# Calculate the probabilty of observing evidence
getFinding(grn1c.ev)

# probabilistic queries, given evidence, joint distribution
querygrain(grn1c.ev, nodes = c("lung", "bronc"), type = "joint")

# probabilistic queries, given evidence, conditional distribution
querygrain(grn1c.ev, nodes = c("lung", "bronc"), type = "conditional")

##################################################
## Note: you can add evidence incremently as well
## 
##################################################
grn1c.ev2 <- setFinding(grn1c, nodes = c("asia"), states = c("yes"))
grn1c.ev2 <- setFinding(grn1c, nodes = c("xray"), states = c("yes"))
querygrain(grn1c.ev2, nodes = c("lung", "bronc"), type = "joint")






