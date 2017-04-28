####BIOS 7080: Ch. 15 Homework####
###Load Design of Experiments Libraries###
if(!require(tidyverse)){
  install.packages("tidyverse"); library(tidyverse)}
if(!require(nlme)){
  install.packages("nlme"); library(nlme)}
if(!require(car)){
  install.packages("car"); library(car)}
if(!require(MASS)){
  install.packages("MASS"); library(MASS)}
if(!require(lme4)){
  install.packages("lme4"); library(lme4)}
if(!require(lmerTest)){
  install.packages("lmerTest"); library(lmerTest)}
if(!require(lsmeans)){
  install.packages("lsmeans"); library(lsmeans)}
if(!require(daewr)){
  install.packages("daewr"); library(daewr)}
if(!require(multcomp)){
  install.packages("multcomp"); library(multcomp)}
if(!require(ez)){
  install.packages("ez"); library(ez)}

####Problem 15.1####
###Data Input###
#serum glucose levels
glucose<-c(28, 34, 32, 15, 29, 27, 12, 33, 28, 21, 44, 39, 22, 18, 12, 23, 22,
           10, 18, 16, 9, 25, 24, 15, 31, 30, 39, 28, 27, 36, 24, 26, 36, 21,
           26, 32)
#diet
diet<-factor(c(rep(1, 12), rep(2, 12), rep(3, 12)))
#subject
subject.prb1<-factor(c(rep(1,3), rep(2,3), rep(3,3), rep(4,3), rep(5,3),
                       rep(6,3), rep(7,3), rep(8,3), rep(9,3), rep(10,3),
                       rep(11,3), rep(12,3)))
#Time in minutes
time<-factor(rep(c(15, 30, 45), 12))

##Dataframe for Problem 15.1
prb15_1.tibble<-tibble(glucose, diet, subject.prb1, time)

###Part b)###
#means of the diets
mudiet.list<-list(
  mudiet1t15<-mean(glucose[diet==1&time==15]),
  mudiet2515<-mean(glucose[diet==2&time==15]),
  mudiet3t15<-mean(glucose[diet==3&time==15]),
  mudiet1t30<-mean(glucose[diet==1&time==30]),
  mudiet2t30<-mean(glucose[diet==2&time==30]),
  mudiet3t30<-mean(glucose[diet==3&time==30]),
  mudiet1t45<-mean(glucose[diet==1&time==45]),
  mudiet2t45<-mean(glucose[diet==2&time==45]),
  mudiet3t45<-mean(glucose[diet==3&time==45])
  )
#vector of diet means
mudiet.vec<-unlist(mudiet.list)
#vector of diets
diet.vec<-factor(rep(as.numeric(as.list(levels(diet))), 3))
#vector of 
time.vec<-factor(c(rep(15, 3), rep(30,3), rep(45,3)))

##profile plot
#Dataframe for the profile plot
profplottib.prb15_1<-tibble(mudiet.vec, diet.vec, time.vec)

#profile plot
profplt.prb15_1<-ggplot(data = profplottib.prb15_1,
                        aes(x=time.vec , y=mudiet.vec,
                             colour=diet.vec, group=diet.vec))+
  geom_point()+
  geom_line()+
  labs(y="Mean serum glucose", x="Time (minutes)", title="Mean serum glucose vs. Time (minutes)",
       legend="Time")

###Part d)###
#split plot-repeated measures anova with "subjects" nested in time
prb15_1.aov<-aov(glucose~diet*time+Error(subject.prb1/(time)), data = prb15_1.tibble)

#add contrasts
contrasts(prb15_1.tibble$time)<-contr.poly(3)
prb15_1contr.aov<-aov(glucose~diet*time+Error(subject.prb1/(time)), data = prb15_1.tibble)
#view the linear and quadratic contrasts for time and time:diet
summary(prb15_1contr.aov, split=list(time=list("Linear"=1, "Quadratic"=2)))

##marignal means##
#marginal means of diet
marmudiet1.prb15_1<-mean(glucose[diet==1])
marmudiet2.prb15_1<-mean(glucose[diet==2])
marmudiet3.prb15_1<-mean(glucose[diet==3])
#marginal means of time
marmutime15.prb15_1<-mean(glucose[time==15])
marmutime30.prb15_1<-mean(glucose[time==30])
marmutime45.prb15_1<-mean(glucose[time==45])

##standard errors##
#number of replications per diet
r.prb15_1<-length(glucose[diet==1&time==15])
#number of repeated time measures
p.prb15_1<-length(levels(time))
#MSE of the "whole-plot"
MSE1.prb15_1<-summary(prb15_1.aov)[[1]][[1]][['Mean Sq']][2]
#Df E1
DfE1.prb15_1<-summary(prb15_1.aov)[[1]][[1]][['Df']][2]
#MSE of the "sub-plots"
MSE2.prb15_1<-summary(prb15_1.aov)[[2]][[1]][['Mean Sq']][3]
#Df E2
DfE2.prb15_1<-summary(prb15_1.aov)[[2]][[1]][['Df']][3]

#cell means standard errors
secelmu.prb15_1<-sqrt(MSE2.prb15_1/r.prb15_1)
#marginal mean standard errors for diet
semarmudiet.prb15_1<-sqrt(MSE1.prb15_1/(r.prb15_1*p.prb15_1))
#marginal mean standard errors for 'time'
semarmutime.prb15_1<-sqrt(MSE2.prb15_1/(r.prb15_1*p.prb15_1))

###Part e)###
##Residual Plot analysis##
#get the model again with 'lme' function in 'nlme' package
prb15_1.lme<-lme(glucose~diet*time, random=~1|subject.prb1/time, data = prb15_1.tibble)
#create space for both Q-Q and homoscedasticity plots
par(mfrow=c(1,2))
#add the residuals to the dataframe
prb15_1.tibble$resi<-residuals(prb15_1.lme)
#Q-Q plot
qqnorm(prb15_1.tibble$resi, main="Normal Q-Q")
#add the q-q line
qqline(prb15_1.tibble$resi)
boxplot(resi ~ interaction(diet,time), main="Homoscedasticity", 
        xlab = "Code Categories", ylab = "Residuals", border = "white", 
        data=prb15_1.tibble)
points(resi ~ interaction(diet,time), pch = 1, 
       main="Homoscedasticity",  data=prb15_1.tibble)

###Part g)###
#get the matrix of values from the design
prb15_1.matrix<-matrix(glucose, byrow = T, ncol = 3)
#dataframe defining the intra-subject model for multivariate repeated-measures data
dietfac<-factor(c(1, 2, 3))
timefac<-factor(c(15, 30, 45))
prb15_1.idata<-data.frame(timefac)
options(contrasts = c("contr.sum", "contr.poly"))
#linear regression model
prb15_1.lm<-lm(prb15_1.matrix~1, data=prb15_1.tibble)

##Sphericity test is contained under 'Anova' results
# prb15_1.results<-Anova(prb15_1.lm, idata = prb15_1.idata,
#                        idesign=~timefac*dietfac, type = "III")
# ##alternate method for sphericity
# ezANOVA(prb15_1.tibble,
#                  within=.(time, diet),
#                  wid=.(subject.prb1),
#                  dv=.(glucose))
