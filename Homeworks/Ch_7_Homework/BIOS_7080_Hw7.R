####BIOS 7080: Ch. 7 Homework####
###Load Libraries###
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

####Problem 7.1####
###Data Input###
#cholesterol observations in serum samples (mg/dL)
cholesterol<-c(167.3, 186.7, 100.0, 214.5, 148.5, 166.7, 184.2, 107.9, 215.3,
               148.5, 179.6, 193.8, 111.6, 228.9, 158.6, 175.3, 198.9, 114.4,
               220.4, 154.7, 169.4, 179.4, 105.9, 108.2, 144.7, 165.0, 177.6,
               104.1, 207.1, 145.9, 177.7, 190.4, 113.4, 221.0, 156.1, 177.1,
               192.4, 114.6, 219.7, 151.0)
#patients
patients<-factor(rep(c(1, 2, 3, 4, 5), 8))
#run number
runs<-factor(c(rep(1, 10), rep(2, 10), rep(3, 10), rep(4, 10)))
#dataframe for problem 1
prb7_1.tibble<-tibble(cholesterol, patients, runs)

###Part a)###
##Various methods for ANOVA for random effects
#model for problem 1
prb7_1.mod<-cholesterol~1+(1|runs)+(1|patients)
prb7_1.lmer<-lmer(prb7_1.mod, data=prb7_1.tibble)
#anova(prb7_1.lmer)
prb7_1.lme<-lme(cholesterol~1, random=~1|runs/patients)
#summary(prb7_1.lme)
prb7_1.aov<-aov(formula = cholesterol ~ Error(runs * patients))
#summary(prb7_1.aov)