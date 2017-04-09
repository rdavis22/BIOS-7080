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
               220.4, 154.7, 169.4, 179.4, 105.9, 208.2, 144.7, 165.9, 177.6,
               104.1, 207.1, 145.9, 177.7, 190.4, 113.4, 221.0, 156.1, 177.1,
               192.4, 114.6, 219.7, 151.0)
#patients
patients<-factor(rep(c(1, 2, 3, 4, 5), 8))
#run number
runs<-factor(c(rep(1, 10), rep(2, 10), rep(3, 10), rep(4, 10)))
#dataframe for problem 1
prb7_1.tibble<-tibble(cholesterol, patients, runs)

###Parts a) and b)###
##Various methods for ANOVA for random effects
#model for problem 1
prb7_1.mod<-cholesterol~1+(1|runs)+(1|patients)+(1|patients:runs)
#Classical ANOVA method
prb7_1.aov<-aov(formula = cholesterol ~ Error(runs * patients))
#summary(prb7_1.aov)

#lmer ANOVA method
prb7_1.lmer<-lmer(prb7_1.mod, data=prb7_1.tibble)
#summary(prb7_1.lmer)

#lme ANOVA method
prb7_1.lme<-lme(cholesterol~1, random=~1|runs/patients)
#summary(prb7_1.lme)

##Mean Squares
#MSruns
MSruns<-summary(prb7_1.aov$runs)[[1]][['Mean Sq']]
#MSpatients
MSpatients<-summary(prb7_1.aov$patients)[[1]][['Mean Sq']]
#MSrp (interaction)
MSrp<-summary(prb7_1.aov$`runs:patients`)[[1]][['Mean Sq']]
#MSError
MSError.prb1<-summary(prb7_1.aov$Within)[[1]][['Mean Sq']]

###Parts c)###
#number of runs
r.prb1<-2
#number of levels of runs
a.prb1<-length(levels(runs))
#number of levels of patients
b.prb1<-length(levels(patients))

##Estimated components of variances (can also get main effects from 'lmer' output)
#estimated variance component of runs
sigma_runs<-(MSruns-MSrp)/(r.prb1*b.prb1)
#estimated variance component of patients
sigma_patients<-(MSpatients-MSrp)/(r.prb1*a.prb1)
#estimated variance component of interaction
sigma_rp<-(MSrp-MSError.prb1)/(r.prb1)

###part d)###
##F test for interaction
#F statistic
F0_rp<-MSrp/MSError.prb1
#critical F value (F(0.05, df(rp), df(error)))
Fcrit_rp<-qf(0.95, summary(prb7_1.aov$`runs:patients`)[[1]][['Df']],
             summary(prb7_1.aov$Within)[[1]][['Df']])
#p_value
pF0_rp<-pf(F0_rp, summary(prb7_1.aov$`runs:patients`)[[1]][['Df']],
   summary(prb7_1.aov$Within)[[1]][['Df']],lower.tail = F)
##F test for runs
#F statistic
F0_runs<-MSruns/MSrp
#critical F value (F(0.05, df(rp), df(error)))
Fcrit_runs<-qf(0.95, summary(prb7_1.aov$runs)[[1]][['Df']],
             summary(prb7_1.aov$`runs:patients`)[[1]][['Df']])
#p_value
pF0_runs<-pf(F0_runs, summary(prb7_1.aov$runs)[[1]][['Df']],
           summary(prb7_1.aov$`runs:patients`)[[1]][['Df']],lower.tail = F)
##F test for patients
#F statistic
F0_patients<-MSpatients/MSrp
#critical F value (F(0.05, df(rp), df(error)))
Fcrit_patients<-qf(0.95, summary(prb7_1.aov$patients)[[1]][['Df']],
             summary(prb7_1.aov$`runs:patients`)[[1]][['Df']])
#p_value
pF0_patients<-pf(F0_patients, summary(prb7_1.aov$patients)[[1]][['Df']],
           summary(prb7_1.aov$`runs:patients`)[[1]][['Df']],lower.tail = F)

####Problem 7.2####
###Data Input###
#steer calf performance
performance<-c(2.65, 2.46, 2.56, 2.43, 2.53, 2.36, 2.38, 2.50, 2.25, 1.95, 2.01,
              2.14, 2.20, 2.25, 1.98, 2.37)
#water treatment combinations (N=normal, S=saline)
treatment<-factor(rep(c("NN", "NS", "SN", "SS"), 4))
#which summer (1 or 2)
summer<-factor(c(rep(1, 8), rep(2, 8)))

#dataframe for all the data
prb7_2.tibble<-tibble(performance, treatment, summer)

###Part a)###
##Various methods for ANOVA for random effects
#Classical ANOVA method
prb7_2.aov<-aov(performance~treatment+Error(summer), data=prb7_2.tibble)
###Note: the sum of squares and the mean squares are the same for mixed...
###effects model and a completely randomized model. For example, you can use...
###"aov(performance~treatment*summer)" to obtain the Anova table; however,
###you CANNOT use the F tests and P values if doing so for a mixed model!!!
###They are invalid for a mixed or random effects model.
#summary(prb7_2.aov)

#lmer ANOVA method
prb7_2.lmer<-lmer(performance~treatment+(1|summer), data=prb7_2.tibble)
#summary(prb7_2.lmer)

#lme ANOVA method
prb7_2.lme<-lme(performance~treatment, random=~1|summer)
#summary(prb7_2.lme)