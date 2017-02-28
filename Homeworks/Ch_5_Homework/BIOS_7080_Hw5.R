####BIOS 7080: Hw Ch. 5####
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(lme4))
  install.packages("lme4")
if(!require(lmerTest))
  install.packages("lmerTest")
if(!require(lsmeans))
  install.packages("lsmeans")

####Problem E.g. 5.1####
###Data Input###
#indepent variable
casting<-factor(c(rep(1, 10), rep(2, 10), rep(3, 10)))
#casting variable (dependent variable)
tens_stren<-c(88.0, 88.0, 94.8, 90.0, 93.0, 89.0, 86.0, 92.9, 89.0, 93.0, 85.9,
              88.6, 90.0, 87.1, 85.6, 86.0, 91.0, 89.6, 93.0, 87.5, 94.2, 91.5,
              92.0, 96.5, 95.6, 93.8, 92.5, 93.2, 96.2, 92.5)
#dataframe for analysis
prb5_1eg.tibble<-tibble(casting, tens_stren)

###ANOVA###
prb5_1eg.aov<-aov(tens_stren~Error(casting))

prb5_1eg.lmer<-lmer(tens_stren~1+(1|casting), data = prb1.tibble)
prb5_1eg.anova<-anova(prb5_1eg.lmer)


####Problem 5.1####
###Data input###
#beef animal sires that produced the offspring
sires<-factor(c(rep(177, 8), rep(200, 8), rep(201, 8), rep(202, 8), rep(203, 8)))
#birthweights of offspring
bweights<-c(61, 100, 56, 113, 99, 103, 75, 62, 75, 102, 95, 103, 98, 115, 98, 
            94, 58, 60, 60, 57, 57, 59, 54, 100, 57, 56, 67, 59, 58, 121, 101,
            101, 59, 46, 120, 115, 115, 93, 105, 75)
#dataframe for the data
prb5_1.tibble<-tibble(sires, bweights)

###Part a)###
#classical method for random effects model
prb5_1.aov<-aov(bweights~Error(sires), data=prb5_1.tibble)

#mixed effects method for random effects model
prb5_1.lmer<-lmer(bweights~1+(1|sires))
prb5_1.anova<-anova(prb5_1.lmer)
SSA.prb5_1<-5591
SSW.prb5_1<-16233
MSA.prb5_1<-1398
MSW.prb5_1<-463.8

###Part b)###
#get the estimate for the F value
Fval.prb5_1<-MSA.prb5_1/MSW.prb5_1

#Number of treatments
t.prb5_1<-length(levels(sires))
#number of replications in each treatment
r.prb5_1<-length(sires[sires=='177'])
#total number of observations
N.prb5_1<-length(sires)

##point estimate and 90% CI for variance of error
#point estimate for variance for progeny within sires
sigmae2.prb5_1<-MSW.prb5_1
#point estimate
#chi-square variables for building 90% CI for sigma_error^2
A.prb5_1<-qchisq(.95, N.prb5_1-t.prb5_1)
B.prb5_1<-qchisq(.05, N.prb5_1-t.prb5_1)
#90% CI for sigma_error^2
CI90_sigmae2.prb5_1<-c(SSW.prb5_1/A.prb5_1, SSW.prb5_1/B.prb5_1)

##point estimate and 90% CI for variance of treatment
#point estimate for variance for sires
sigmatau2.prb5_1<-(MSA.prb5_1-MSW.prb5_1)/r.prb5_1
#critical values for sigma_tau^2
#F upper critical value (F(0.05, t-1=4, t(r-1)=35)) for 90% CI for sigma_tau^2
Fu.prb5_1<-qf(0.975, t.prb5_1-1, N.prb5_1-t.prb5_1)
#F lower critical value (F(0.05, t-1=4, t(r-1)=35)) for 90% CI for sigma_tau^2
Fl.prb5_1<-qf(0.025, t.prb5_1-1, N.prb5_1-t.prb5_1)
##chi-square variables for building 90% CI for sigma_tau^2
C.prb5_1<-qchisq(0.975, t.prb5_1-1)
D.prb5_1<-qchisq(0.025, t.prb5_1-1)

#90% CI for sigma_tau^2
CI90_sigmatau2.prb5_1<-c(SSA.prb5_1*(1-Fu.prb5_1/Fval.prb5_1)/(r.prb5_1*C.prb5_1),
                         SSA.prb5_1*(1-Fl.prb5_1/Fval.prb5_1)/(r.prb5_1*D.prb5_1))

###Part c)###
#Test the null hypothesis that sigma_tau^2=0
#get the p-value
pval.prb5_1<-pf(Fval.prb5_1, t.prb5_1-1, N.prb5_1-t.prb5_1, lower.tail = F)

###Part d)###
##Intraclass correlation coefficient with 90% CI
#point estimate
rhoI.prb5_1<-sigmatau2.prb5_1/(sigmatau2.prb5_1+sigmae2.prb5_1)
#get new critical F values for rho based on alpha=.10
Fu.prb5_1d<-qf(0.95, t.prb5_1-1, N.prb5_1-t.prb5_1)
Fl.prb5_1d<-qf(0.05, t.prb5_1-1, N.prb5_1-t.prb5_1)

#90% CI
CI90_rhoI.prb5_1<-c((Fval.prb5_1-Fu.prb5_1d)/(Fval.prb5_1+(r.prb5_1-1)*Fu.prb5_1d),
                    (Fval.prb5_1-Fl.prb5_1d)/(Fval.prb5_1+(r.prb5_1-1)*Fl.prb5_1d))


####Problem 5.2####
###Data Input###
#The patient number of the samples
patient<-factor(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8))
#Amount of cholesterol
cholesterol<-c(167.3, 166.7, 186.7, 184.2, 100.0, 107.9, 214.5, 215.3, 148.5, 
               149.5, 171.5, 167.3, 161.5, 159.4, 243.6, 245.5)
#dataframe for the data
prb5_2.tibble<-tibble(patient, cholesterol)

###Part a)###
#classical method for random effects model
prb5_2.aov<-aov(cholesterol~Error(patient), data=prb5_2.tibble)

#mixed effects method for random effects model
prb5_2.lmer<-lmer(cholesterol~1+(1|patient))
prb5_2.anova<-anova(prb5_2.lmer)
SSA.prb5_2<-25132
SSW.prb5_2<-48.16
MSA.prb5_2<-3590
MSW.prb5_2<-6.02

###Part b)###
#get the estimate for the F value
Fval.prb5_2<-MSA.prb5_2/MSW.prb5_2

#Number of treatments
t.prb5_2<-length(levels(patient))
#number of replications in each treatment
r.prb5_2<-length(patient[patient=='1'])
#total number of observations
N.prb5_2<-length(patient)

##point estimate and 90% CI for variance of error
#point estimate for variance for samples
sigmae2.prb5_2<-MSW.prb5_2
#point estimate
#chi-square variables for building 90% CI for sigma_error^2
A.prb5_2<-qchisq(.95, N.prb5_2-t.prb5_2)
B.prb5_2<-qchisq(.05, N.prb5_2-t.prb5_2)
#90% CI for sigma_error^2
CI90_sigmae2.prb5_2<-c(SSW.prb5_2/A.prb5_2, SSW.prb5_2/B.prb5_2)

##point estimate and 90% CI for variance of treatment
#point estimate for variance for patients
sigmatau2.prb5_2<-(MSA.prb5_2-MSW.prb5_2)/r.prb5_2
#critical values for sigma_tau^2
#F upper critical value (F(0.05, t-1=7, t(r-1)=8)) for 90% CI for sigma_tau^2
Fu.prb5_2<-qf(0.975, t.prb5_2-1, N.prb5_2-t.prb5_2)
#F lower critical value (F(0.05, t-1=7, t(r-1)=8)) for 90% CI for sigma_tau^2
Fl.prb5_2<-qf(0.025, t.prb5_2-1, N.prb5_2-t.prb5_2)
##chi-square variables for building 90% CI for sigma_tau^2
C.prb5_2<-qchisq(0.975, t.prb5_2-1)
D.prb5_2<-qchisq(0.025, t.prb5_2-1)

#90% CI for sigma_tau^2
CI90_sigmatau2.prb5_2<-c(SSA.prb5_2*(1-Fu.prb5_2/Fval.prb5_2)/(r.prb5_2*C.prb5_2),
                         SSA.prb5_2*(1-Fl.prb5_2/Fval.prb5_2)/(r.prb5_2*D.prb5_2))

#Test the null hypothesis that sigma_tau^2=0
#get the p-value
pval.prb5_2<-pf(Fval.prb5_2, t.prb5_2-1, N.prb5_2-t.prb5_2 , lower.tail = F)

###Part c)###
##Intraclass correlation coefficient with 90% CI
#point estimate
rhoI.prb5_2<-sigmatau2.prb5_2/(sigmatau2.prb5_2+sigmae2.prb5_2)
#get new critical F values for rho based on alpha=.10
Fu.prb5_2c<-qf(0.95, t.prb5_2-1, N.prb5_2-t.prb5_2)
Fl.prb5_2c<-qf(0.05, t.prb5_2-1, N.prb5_2-t.prb5_2)

#90% CI
CI90_rhoI.prb5_2<-c((Fval.prb5_2-Fu.prb5_2c)/(Fval.prb5_2+(r.prb5_2-1)*Fu.prb5_2c),
                    (Fval.prb5_2-Fl.prb5_2c)/(Fval.prb5_2+(r.prb5_2-1)*Fl.prb5_2c))


####Problem 5.3 Exmple####
# batch<-factor(c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6))
# method<-factor(c(rep("A", 6), rep("B", 6)))
# residue<-c(120, 110, 120, 100, 140, 130, 71, 71, 70, 76, 63, 68)
# summary(aov(residue~batch+Error(method/batch)))