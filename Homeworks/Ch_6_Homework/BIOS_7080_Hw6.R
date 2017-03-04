####BIOS 7080: Hw Ch. 6####
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(car))
  install.packages("car")
if(!require(MASS))
  install.packages("MASS")

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
#MS of base
MSbase.prb6_1<-summary(prb6_1.aov)[[1]][['Mean Sq']][1]
#MS of alcohol
MSalcohol.prb6_1<-summary(prb6_1.aov)[[1]][['Mean Sq']][2]
#MS of interaction
MSintrct.prb6_1<-summary(prb6_1.aov)[[1]][['Mean Sq']][3]
#standard error of the base
sebase.prb6_1<-sqrt(MSE.prb6_1/(r.prb6_1*n.alcohol))
#standard error of the alcohol
sealcohol.prb6_1<-sqrt(MSE.prb6_1/(r.prb6_1*n.base))
#standard error of the cells
secells.prb6_1<-sqrt(MSE.prb6_1/r.prb6_1)

###Part c)###
#F statistic for interaction
F0.prb6_1<-MSintrct.prb6_1/MSE.prb6_1
#critical F-value (alpha, (n.base-1)*(n.alcohol-1), n.base*n.alcohol*(r.prb6_1-1))
Fcrit.prb6_1<-qf(0.95, (n.base-1)*(n.alcohol-1), n.base*n.alcohol*(r.prb6_1-1))
#p-value
pval.prb6_1<-pf(F0.prb6_1, (n.base-1)*(n.alcohol-1),
                n.base*n.alcohol*(r.prb6_1-1), lower.tail = F)

###Part d)###
#standard error for multiple contrasts (mu1-mu2)
sc.prb6_1<-sqrt((MSE.prb6_1/r.prb6_1)*(1^2+(-1)^2))
#Bonf. t-value for base effects (alpha/2=0.975, k=3, v=ab(r-1)=18)
tval.prb6_1b<-2.64
#Bonf. t-value for alcohol effects (alpha/2=0.975, k=6, v=ab(r-1)=18)
tval.prb6_1a<-2.96

##mulitple contrasts for simple effects
#base effects
b12alc1<-cellmu.prb6_1[1]-cellmu.prb6_1[2]
b12alc2<-cellmu.prb6_1[3]-cellmu.prb6_1[4]
b12alc3<-cellmu.prb6_1[5]-cellmu.prb6_1[6]

#alcohol effects
alc12b1<-cellmu.prb6_1[1]-cellmu.prb6_1[3]
alc13b1<-cellmu.prb6_1[1]-cellmu.prb6_1[5]
alc23b1<-cellmu.prb6_1[3]-cellmu.prb6_1[5]
alc12b2<-cellmu.prb6_1[2]-cellmu.prb6_1[4]
alc13b2<-cellmu.prb6_1[2]-cellmu.prb6_1[6]
alc23b2<-cellmu.prb6_1[4]-cellmu.prb6_1[6]

##95% SCI
#base effects
SCI95_b12alc1<-c(b12alc1-tval.prb6_1b*sc.prb6_1, b12alc1+tval.prb6_1b*sc.prb6_1)
SCI95_b12alc2<-c(b12alc2-tval.prb6_1b*sc.prb6_1, b12alc2+tval.prb6_1b*sc.prb6_1)
SCI95_b12alc3<-c(b12alc3-tval.prb6_1b*sc.prb6_1, b12alc3+tval.prb6_1b*sc.prb6_1)

#alcohol effects
SCI95_alc12b1<-c(alc12b1-tval.prb6_1a*sc.prb6_1, alc12b1+tval.prb6_1a*sc.prb6_1)
SCI95_alc13b1<-c(alc13b1-tval.prb6_1a*sc.prb6_1, alc13b1+tval.prb6_1a*sc.prb6_1)
SCI95_alc23b1<-c(alc23b1-tval.prb6_1a*sc.prb6_1, alc23b1+tval.prb6_1a*sc.prb6_1)
SCI95_alc12b2<-c(alc12b2-tval.prb6_1a*sc.prb6_1, alc12b2+tval.prb6_1a*sc.prb6_1)
SCI95_alc13b2<-c(alc13b2-tval.prb6_1a*sc.prb6_1, alc13b2+tval.prb6_1a*sc.prb6_1)
SCI95_alc23b2<-c(alc23b2-tval.prb6_1a*sc.prb6_1, alc23b2+tval.prb6_1a*sc.prb6_1)

###Part e)###
##Residual analysis
#Normal plot
pqq.prb6_1<-plot(prb6_1.aov, 2)
#Spread-Location plot
psl.prb6_1<-plot(prb6_1.aov, 3)
#Levene's Test
#Levene Test (Median)
lvne_test.prb6_1<-leveneTest(prb6_1.aov, center=median)


####Problem 6.2####
###Data Input###
#method for determining serumn glucose
method<-factor(c(rep("Method 1", 9), rep("Method 2", 9)))