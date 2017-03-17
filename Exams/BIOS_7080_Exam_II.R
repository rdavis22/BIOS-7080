####BIOS 7080: Exam 2####
###Load Libraries###
if(!require(tidyverse)){
  install.packages("tidyverse"); library(tidyverse)}
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

####Problem 1####
###Data Input###
#wafer position (this is the "batch" variable)
wafer<-factor(c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3)))
#uniformity
uniformity<-c(2.76, 5.67, 4.49, 1.43, 1.70, 2.19, 2.34, 1.97, 1.47, 0.94, 1.36,
                1.65)
#tibble for the data
prb1.tibble<-tibble(wafer, uniformity)

##ANOVA##
prb1.aov<-aov(uniformity~Error(wafer), data=prb1.tibble)

###Part b)###
##model for the analysis##
prb1.model<-uniformity~wafer
##linear regression of the model
prb1.lm<-lm(prb1.model, data=prb1.tibble)
##Levene's Test for homogeneity of variances##
prb1.levene<-leveneTest(prb1.lm, center=median)

###Part c)###
#Number of treatments
t.prb1<-length(levels(wafer))
#number of replications in each treatment
r.prb1<-length(wafer[wafer=='1'])
#total number of observations
N.prb1<-nrow(prb1.tibble)

#Sum of Squares and Mean Squares for the model
SSA.prb1<-16.2198
SSW.prb1<-5.2175
MSA.prb1<-5.4066
MSW.prb1<-0.6522

#F-value for the ANOVA
F0.prb1<-MSA.prb1/MSW.prb1

##point estimate and 90% CI for variance of error
#point estimate for variance for progeny within uniformity observations
sigmae2.prb1<-MSW.prb1
#point estimate
#chi-square variables for building 90% CI for sigma_error^2
A.prb1<-qchisq(.95, N.prb1-t.prb1)
B.prb1<-qchisq(.05, N.prb1-t.prb1)
#90% CI for sigma_error^2
CI90_sigmae2.prb1<-c(SSW.prb1/A.prb1, SSW.prb1/B.prb1)

##point estimate and 90% CI for variance of treatment
#point estimate for variance for wafers
sigmatau2.prb1<-(MSA.prb1-MSW.prb1)/r.prb1
#critical values for sigma_tau^2
#F upper critical value (F(0.05, t-1=3, t(r-1)=8)) for 90% CI for sigma_tau^2
Fu.prb1<-qf(0.975, t.prb1-1, N.prb1-t.prb1)
#F lower critical value (F(0.05, t-1=3, t(r-1)=8)) for 90% CI for sigma_tau^2
Fl.prb1<-qf(0.025, t.prb1-1, N.prb1-t.prb1)
##chi-square variables for building 90% CI for sigma_tau^2
C.prb1<-qchisq(0.975, t.prb1-1)
D.prb1<-qchisq(0.025, t.prb1-1)

#90% CI for sigma_tau^2
CI90_sigmatau2.prb1<-c(SSA.prb1*(1-Fu.prb1/F0.prb1)/(r.prb1*C.prb1),
                         SSA.prb1*(1-Fl.prb1/F0.prb1)/(r.prb1*D.prb1))

###Part d)###
##Intraclass Correlation Coefficient
#point estimate
rhoI.prb1<-sigmatau2.prb1/(sigmatau2.prb1+sigmae2.prb1)
#get new critical F values for rho based on alpha=.10
Fu.prb1d<-qf(0.95, t.prb1-1, N.prb1-t.prb1)
Fl.prb1d<-qf(0.05, t.prb1-1, N.prb1-t.prb1)

#90% CI
CI90_rhoI.prb1<-c((F0.prb1-Fu.prb1d)/(F0.prb1+(r.prb1-1)*Fu.prb1d),
                    (F0.prb1-Fl.prb1d)/(F0.prb1+(r.prb1-1)*Fl.prb1d))

###Part e)###
pval.prb1<-pf(F0.prb1, t.prb1-1, N.prb1-t.prb1, lower.tail = F)

####Problem 2####