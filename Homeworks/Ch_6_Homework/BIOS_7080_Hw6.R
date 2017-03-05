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
#pool of serum of glucose
serum<-factor(c(rep(1, 3), rep(2, 3), rep(3, 3), rep(1, 3), rep(2, 3),
                rep(3, 3)))
#glucose level (mg/dL)
glucose<-c(42.5, 43.3, 42.9, 138.4, 144.4, 142.7, 180.9, 180.5, 183.0, 39.8,
           40.3, 41.2, 132.4, 132.4, 130.3, 176.8, 173.6, 174.9)

#dataframe for problem 6.2
prb6_2.tibble<-tibble(method, serum, glucose)

###Part a)###
#Anova
prb6_2.aov<-aov(glucose~method*serum, data=prb6_2.tibble)

#residuals
prb6_2.res<-prb6_2.aov$residuals

#spread-level plot
prb6_2.slp<-spreadLevelPlot(prb6_2.aov)

#levene Test
lvne_test.prb6_2<-leveneTest(prb6_2.aov, center=median)

###Part B)###
#no transformation necessary (Levene's test for medians was not significant)

###Part C)###
##Hypothesis test for interaction effect
#degrees of freedom of method
dfmeth.prb6_2<-summary(prb6_2.aov)[[1]][['Df']][1]
#degrees of freedom of serum
dfserum.prb6_2<-summary(prb6_2.aov)[[1]][['Df']][2]
#degrees of freedom of Residuals
dfresiduals.prb6_2<-summary(prb6_2.aov)[[1]][['Df']][4]
#MSE for this ANOVA
MSE.prb6_2<-summary(prb6_2.aov)[[1]][['Mean Sq']][4]
#MS interaction for this ANOVA
MSintrct.prb6_2<-summary(prb6_2.aov)[[1]][['Mean Sq']][3]
#F statistic for interaction 
FAB.prb6_2<-MSintrct.prb6_2/MSE.prb6_2
#critical F value
Fcrit.prb6_2<-qf(0.95, dfmeth.prb6_2*dfserum.prb6_2, dfresiduals.prb6_2)
#p-value
pval.prb6_2<-pf(FAB.prb6_2, dfmeth.prb6_2*dfserum.prb6_2,
                dfresiduals.prb6_2, lower.tail = F)

###Part D)###
##Cell means
cellmu.prb6_2<-c(mean(glucose[1:3]), mean(glucose[4:6]), mean(glucose[7:9]),
                 mean(glucose[10:12]), mean(glucose[13:15]), mean(glucose[16:18]))
##marginal means
#methods
methmu.prb6_2<-c(mean(glucose[1:9]), mean(glucose[10:18]))
#glucose serum
serummu.prb6_2<-c(mean(c(glucose[1:3], glucose[10:12])), mean(c(glucose[4:6], glucose[13:15])),
                  mean(c(glucose[7:9], glucose[16:18])))
#overall mean
mu.prb6_2<-mean(glucose)

#number of replications
r.prb6_2<-length(glucose[1:3])
#number of methods
n.methods<-length(levels(method))
#number of serums
n.serum<-length(levels(serum))

#standard error of the method
semethod.prb6_2<-sqrt(MSE.prb6_2/(r.prb6_2*n.serum))
#standard error of the serum
seserum.prb6_2<-sqrt(MSE.prb6_2/(r.prb6_2*n.methods))
#standard error of the cells
secells.prb6_2<-sqrt(MSE.prb6_2/r.prb6_2)

###Part E)###
##Difference between method means for each level of glucose
#glucose=1 (method=1-method=2)
meth12glc1.prb6_2<-cellmu.prb6_2[1]-cellmu.prb6_2[4]
#glucose=2
meth12glc2.prb6_2<-cellmu.prb6_2[2]-cellmu.prb6_2[5]
#glucose=3
meth12glc3.prb6_2<-cellmu.prb6_2[3]-cellmu.prb6_2[6]

##Multiple contrasts
#standard error for multiple contrasts (mu1-mu2)
sc.prb6_2<-sqrt((MSE.prb6_2/r.prb6_2)*(1^2+(-1)^2))
#Bonferroni t-value for the standard error (alpha/2=0.975, k=3, v=ab(r-1)=12)
tval.prb6_2<-2.56 #from table V in Kuehl

#multiple contrast tests
SCI95_meth12glc1<-c(meth12glc1.prb6_2-tval.prb6_2*sc.prb6_2, meth12glc1.prb6_2+tval.prb6_2*sc.prb6_2)
SCI95_meth12glc2<-c(meth12glc2.prb6_2-tval.prb6_2*sc.prb6_2, meth12glc2.prb6_2+tval.prb6_2*sc.prb6_2)
SCI95_meth12glc3<-c(meth12glc3.prb6_2-tval.prb6_2*sc.prb6_2, meth12glc3.prb6_2+tval.prb6_2*sc.prb6_2)


####Problem 6.3####
###Data Input###
#Fabric
fabric<-factor(c(1, 1, 2, 2, 3, 3, 4, 4, 1, 1, 2, 2, 3, 3, 4, 4, 1, 1, 2, 2,
                 3, 3, 4, 4, 1, 1, 2, 2, 3, 3, 4, 4))
#temperature in Farenheit
temperature<-factor(c(rep("210", 8), rep("215", 8), rep("220", 8),
                      rep("225", 8)))
#percent shrinkage in dying fabrics
shrinkage<-c(1.8, 2.1, 2.2, 2.4, 2.8, 3.2, 3.2, 3.6, 2.0, 2.1, 4.2, 4.0, 4.4,
             4.8, 3.3, 3.5, 4.6, 5.0, 5.4, 5.6, 8.7, 8.4, 5.7, 5.8, 7.5, 7.9,
             9.8, 9.2, 13.2, 13.0, 10.9, 11.1)
#data frame for prob. 3 data
prb6_3.tibble<-tibble<-tibble(shrinkage, temperature, fabric)

###Part a)###
#Anova
prb6_3.aov<-aov(shrinkage~fabric*temperature, data=prb6_3.tibble)

###Part b)###
##Hypothesis test for interaction effect between fabric*temperature
#degrees of freedom of fabric
dffabric.prb6_3<-summary(prb6_3.aov)[[1]][['Df']][1]
#degrees of freedom of temperature
dftemp.prb6_3<-summary(prb6_3.aov)[[1]][['Df']][2]
#degrees of freedom of Residuals
dfresiduals.prb6_3<-summary(prb6_3.aov)[[1]][['Df']][4]
#MSE for this ANOVA
MSE.prb6_3<-summary(prb6_3.aov)[[1]][['Mean Sq']][4]
#MS interaction for this ANOVA
MSintrct.prb6_3<-summary(prb6_3.aov)[[1]][['Mean Sq']][3]
#F statistic for interaction 
Ffabtemp.prb6_3<-MSintrct.prb6_3/MSE.prb6_3
#critical F value
Fcrit.prb6_3<-qf(0.95, dffabric.prb6_3*dftemp.prb6_3, dfresiduals.prb6_3)
#p-value
pval.prb6_3<-pf(Ffabtemp.prb6_3, dffabric.prb6_3*dftemp.prb6_3,
                dfresiduals.prb6_3, lower.tail = F)

###Part c)###
##partition sum of squares ANOVA 
#Fabic (contrast)
fabric.contr<-factor(c(1, 1, 2, 2, 3, 3, 4, 4, 1, 1, 2, 2, 3, 3, 4, 4, 1, 1, 2, 2,
                 3, 3, 4, 4, 1, 1, 2, 2, 3, 3, 4, 4))
#temperature in Farenheit (contrast)
temperature.contr<-factor(c(rep("210", 8), rep("215", 8), rep("220", 8),
                      rep("225", 8)))
#percent shrinkage in dying fabrics (contrast)
shrinkage.contr<-c(1.8, 2.1, 2.2, 2.4, 2.8, 3.2, 3.2, 3.6, 2.0, 2.1, 4.2, 4.0, 4.4,
             4.8, 3.3, 3.5, 4.6, 5.0, 5.4, 5.6, 8.7, 8.4, 5.7, 5.8, 7.5, 7.9,
             9.8, 9.2, 13.2, 13.0, 10.9, 11.1)
#dataframe for problem 6.3 (contrasts)
prb6_3_contr.tibble<-tibble<-tibble(shrinkage.contr, temperature.contr, fabric.contr)
#ANOVA (w/ contrasts)
prb6_3_contr.aov<-aov(shrinkage.contr~fabric.contr*temperature.contr, data=prb6_3_ord.tibble)
#display ANOVA table with linear and quadratic contrasts
summary.aov(prb6_3_ord.aov, split=list(temperature=list("Linear"=1, "Quadratic" = 2)))