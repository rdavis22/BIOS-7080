####BIOS 7080: Hw Ch. 6####
if(!require(tidyverse))
  install.packages("tidyverse")

####Problem 6.1####
###Data Input###
#type of alcohol used in reaction
alcohol<-factor(c(rep(1, 8), rep(2, 8), rep(3, 8)))
#type of base used in reaction
base<-factor(c(rep(1, 4), rep(2, 4), rep(1, 4), rep(2, 4), rep(1, 4), rep(2, 4)))
#percent yield of reaction
y<-c(91.3, 89.9, 90.7, 91.4, 87.3, 89.4, 91.5, 88.3, 89.3, 88.1, 90.4, 91.4, 92.3,
     91.5, 90.6, 94.7, 89.5, 87.6, 88.3, 90.3, 93.1, 90.7, 91.5, 89.8)

#dataframe for the analysis
prb6_1.tibble<-tibble(alcohol, base, y)

###Part a)###
##ANOVA##
prb6_1.aov<-aov(y~base*alcohol, data = prb6_1.tibble)

###Part b)###
##cell means##
cellmu.prb6_1<-c(mean(y[1:4]), mean(y[5:8]), mean(y[9:12]), mean(y[13:16]),
                 mean(y[17:20]), mean(y[21:24]))
##marginal means##
#base means
basemu.prb6_1<-c(mean(y[base=="1"]), mean(y[base=="2"]))
#number of levels of base
n.base<-length(basemu.prb6_1)
#alcohol means
alcoholmu.prb6_1<-c(mean(y[alcohol=="1"]), mean(y[alcohol=="2"]),
                    mean(y[alcohol=="3"]))
#number of levels of alcohol
n.alcohol<-length(alcoholmu.prb6_1)
#total mean
mu.prb6_1<-mean(y)
#total number of cells
N.prb6_1<-length(prb6_1.tibble$y)
#number of replications
r.prb6_1<-4 #from the no. of observations per cell

##standard errors##
#MSE of the model
MSE.prb6_1<-summary(prb6_1.aov)[[1]][['Mean Sq']][4]
#standard error of the base
sebase.prb6_1<-sqrt(MSE.prb6_1/(r.prb6_1*n.alcohol))
#standard error of the alcohol
sealcohol.prb6_1<-sqrt(MSE.prb6_1/(r.prb6_1*n.base))