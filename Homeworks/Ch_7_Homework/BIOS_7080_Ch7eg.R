####E.g. 7.5####
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

#glucose concentration (mg/dL)
glucose<-c(41.2, 41.2, 39.8, 41.5, 41.9, 45.5, 42.6, 41.4, 40.3, 43.0, 42.7, 44.7,
          135.7, 143.0, 132.4, 134.4, 137.4, 141.1, 136.8, 143.3, 130.3, 130.0,
          135.2, 139.1, 163.2, 181.4, 173.6, 174.9, 166.6, 175.0, 163.3, 180.3,
          173.9, 175.6, 165.5, 172.0)
#standard concentration samples
C<-factor(c(rep(1, 12), rep(2, 12), rep(3, 12)))
#Days
D<-factor(c(rep(c("Day 1", "Day 1", "Day 2", "Day 2", "Day 3", "Day 3"), 6)))
#runs
R<-factor(c(rep(c("Run 1", "Run 2", "Run 3", "Run 4", "Run 5", "Run 6"), 6)))

##ANOVA##
prb7_5eg.aov<-aov(glucose~C*D*R%in%D)
