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
#which summer (1 or 2)
summer<-factor(c(rep(1, 8), rep(2, 8)))
#water treatment combinations (N=normal, S=saline)
treatment<-factor(rep(c("NN", "NS", "SN", "SS"), 4))

#dataframe for all the data
prb7_2.tibble<-tibble(performance, treatment, summer)

###Part a)###
##Various methods for ANOVA for random effects
#Classical ANOVA method
prb7_2.aov<-aov(performance~summer*treatment, data=prb7_2.tibble)
###Note: the sum of squares and the mean squares are the same for mixed...
###effects model and a completely randomized model. For example, you can use...
###"aov(performance~treatment*summer)" to obtain the Anova table; however,
###you CANNOT use the F tests and P values if doing so for a mixed model!!!
###They are invalid for a mixed or random effects model.
#summary(prb7_2.aov)

##Mean Squares
#summer
MSsummer<-summary(prb7_2.aov)[[1]][['Mean Sq']][1]
#treatment
MStreatment<-summary(prb7_2.aov)[[1]][['Mean Sq']][2]
#interaction
MSst<-summary(prb7_2.aov)[[1]][['Mean Sq']][3]
MSError.prb2<-summary(prb7_2.aov)[[1]][['Mean Sq']][4]

##dimensions of the experiment
#number of replications
r.prb2<-length(performance[treatment=="NN" & summer==1])
#number of levels of summer
a.prb2<-length(levels(summer))
#number of levels of treatment
b.prb2<-length(levels(treatment))

#lmer ANOVA method
prb7_2.lmer<-lmer(performance~treatment+(1|summer), data=prb7_2.tibble)
#summary(prb7_2.lmer)

#lme ANOVA method
prb7_2.lme<-lme(performance~treatment, random=~1|summer)
#summary(prb7_2.lme)

###Part b)###
##grand mean
grandmu<-mean(performance)

##cell means
#mean of treatment(==NN) and summer(==1) (and as follows)
cellmu.list<-list(
  mu_NN1<-mean(performance[treatment=="NN" & summer==1]),
  mu_NS1<-mean(performance[treatment=="NS" & summer==1]),
  mu_SN1<-mean(performance[treatment=="SN" & summer==1]),
  mu_SS1<-mean(performance[treatment=="SS" & summer==1]),
  mu_NN2<-mean(performance[treatment=="NN" & summer==2]),
  mu_NS2<-mean(performance[treatment=="NS" & summer==2]),
  mu_SN2<-mean(performance[treatment=="SN" & summer==2]),
  mu_SS2<-mean(performance[treatment=="SS" & summer==2])  
)
#standard error of cell means
secell.prb2<-sqrt(MSError.prb2/r.prb2)

#marginal means for summers
summermu.list<-list(
  mu_summer1<-mean(performance[summer=="1"]),
  mu_summer2<-mean(performance[summer=="2"])
)
#standard error of marginal means (summer)
sesummer.prb2<-sqrt(MSError.prb2/(r.prb2*b.prb2))

#marginal means for water treatment levels)
treatmu.list<-list(
  mu_NN<-mean(performance[treatment=="NN"]),
  mu_NS<-mean(performance[treatment=="NS"]),
  mu_SN<-mean(performance[treatment=="SN"]),
  mu_SS<-mean(performance[treatment=="SS"])
)
#standard error of marginal means (treatment)
setreat.prb2<-sqrt(MSError.prb2/(r.prb2*a.prb2))

###Part d)###
##F test for interaction
#F statistic
F0_st<-MSst/MSError.prb2
#critical F value (F(0.05, df(st), df(error)))
Fcrit_st<-qf(0.95, summary(prb7_2.aov)[[1]][['Df']][3],
             summary(prb7_2.aov)[[1]][['Df']][4])
#p_value
pF0_st<-pf(F0_st, summary(prb7_2.aov)[[1]][['Df']][3],
           summary(prb7_2.aov)[[1]][['Df']][4],lower.tail = F)
##F test for summer
#F statistic
F0_summer<-MSsummer/MSst
#critical F value (F(0.05, df(summer), df(st)))
Fcrit_summer<-qf(0.95, summary(prb7_2.aov)[[1]][['Df']][1],
               summary(prb7_2.aov)[[1]][['Df']][3])
#p_value
pF0_summer<-pf(F0_summer, summary(prb7_2.aov)[[1]][['Df']][1],
               summary(prb7_2.aov)[[1]][['Df']][3],lower.tail = F)
##F test for patients
#F statistic
F0_treatment<-MStreatment/MSst
#critical F value (F(0.05, df(summer), df(st)))
Fcrit_treatment<-qf(0.95, summary(prb7_2.aov)[[1]][['Df']][2],
                 summary(prb7_2.aov)[[1]][['Df']][3])
#p_value
pF0_treatment<-pf(F0_treatment, summary(prb7_2.aov)[[1]][['Df']][2],
               summary(prb7_2.aov)[[1]][['Df']][3],lower.tail = F)

####Problem 7.2 Part e)####
###Data Input###
performance<-c(2.65, 2.46, 2.56, 2.43, 2.53, 2.36, 2.38, 2.50, 2.25, 1.95, 2.01,
               2.14, 2.20, 2.25, 1.98, 2.37)
#water treatment combinations during the first 56-days (N=normal, S=saline)
treatmentA<-factor(rep(c("N", "N", "S", "S"), 4))
#water treatment combinations during the first 56-days (N=normal, S=saline)
treatmentB<-factor(rep(c("N", "S", "N", "S"), 4))
#which summer (1 or 2)
summer<-factor(c(rep(1, 8), rep(2, 8)))

#dataframe for all the data
prb7_2e.tibble<-tibble(performance, treatmentA, treatmentB, summer)

###Part i)###
##Various methods for ANOVA for random effects
#Classical ANOVA method
prb7_2e.aov<-aov(performance~treatmentA*treatmentB*summer, data=prb7_2e.tibble)
#summary(prb7_2e.aov)

###Part ii)###
##Mean Squares
#treatment A
MSA<-summary(prb7_2e.aov)[[1]][['Mean Sq']][1]
#treatment B
MSB<-summary(prb7_2e.aov)[[1]][['Mean Sq']][2]
#summer
MSC<-summary(prb7_2e.aov)[[1]][['Mean Sq']][3]
#treatA:treatB
MSAB<-summary(prb7_2e.aov)[[1]][['Mean Sq']][4]
#treatA:summer
MSAC<-summary(prb7_2e.aov)[[1]][['Mean Sq']][5]
#treatB:summer
MSBC<-summary(prb7_2e.aov)[[1]][['Mean Sq']][6]
#treatA:treatB:summer
MSABC<-summary(prb7_2e.aov)[[1]][['Mean Sq']][7]
#Error
MSError.prb2<-summary(prb7_2e.aov)[[1]][['Mean Sq']][8]

##dimensions of the experiment
#number of replications
r.prb2<-length(performance[treatmentA=="N" & treatmentB=="N" & summer==1])
#number of levels of treatmentA
a.prb2<-length(levels(treatmentA))
#number of levels of treatmentB
b.prb2<-length(levels(treatmentB))
#number of levels of summer
c.prb2<-length(levels(summer))

##grand mean
grandmu<-mean(performance)

##cell and marginal means (are the same as the two factor experiment)
#mean of treatment(==NN) and summer(==1) (and as follows)
cellmu.list<-list(
  mu_NN1<-mean(performance[treatmentA=="N" & treatmentB=="N" & summer==1]),
  mu_NS1<-mean(performance[treatmentA=="N" & treatmentB=="S" & summer==1]),
  mu_SN1<-mean(performance[treatmentA=="S" & treatmentB=="N" & summer==1]),
  mu_SS1<-mean(performance[treatmentA=="S" & treatmentB=="S" & summer==1]),
  mu_NN2<-mean(performance[treatmentA=="N" & treatmentB=="N" & summer==2]),
  mu_NS2<-mean(performance[treatmentA=="N" & treatmentB=="S" & summer==2]),
  mu_SN2<-mean(performance[treatmentA=="S" & treatmentB=="N" & summer==2]),
  mu_SS2<-mean(performance[treatmentA=="S" & treatmentB=="S" & summer==2])  
)
#standard error of cell means
secell.prb2<-sqrt(MSError.prb2/r.prb2)

#standard error of marginal means (summer)
sesummer.prb2<-sqrt(MSError.prb2/(r.prb2*a.prb2*b.prb2))

#standard error of marginal means (treatmentA)
setreatA.prb2<-sqrt(MSError.prb2/(r.prb2*b.prb2*c.prb2))

#standard error of marginal means (treatmentB)
setreatB.prb2<-sqrt(MSError.prb2/(r.prb2*a.prb2*c.prb2))

###Part iv)###
##degrees of Freedom
dfA<-summary(prb7_2e.aov)[[1]][['Df']][1]
dfB<-summary(prb7_2e.aov)[[1]][['Df']][2]
dfC<-summary(prb7_2e.aov)[[1]][['Df']][3]
dfAB<-summary(prb7_2e.aov)[[1]][['Df']][4]
dfAC<-summary(prb7_2e.aov)[[1]][['Df']][5]
dfBC<-summary(prb7_2e.aov)[[1]][['Df']][6]
dfABC<-summary(prb7_2e.aov)[[1]][['Df']][7]
dfError<-summary(prb7_2e.aov)[[1]][['Df']][8]
##Hypothesis testing for each effect
##Three-Way Interaction
F0_ABC<-MSABC/MSError.prb2
Fcrit_ABC<-qf(0.95, dfABC, dfError)
pABC<-pf(F0_ABC, dfABC, dfError,lower.tail = F)
##Two-Way Interactions
#AB
F0_AB<-MSAB/MSABC
Fcrit_AB<-qf(0.95, dfAB, dfABC)
pAB<-pf(F0_AB, dfAC, dfABC,lower.tail = F)
#AC
F0_AC<-MSAC/MSABC
Fcrit_AC<-qf(0.95, dfAC, dfABC)
pAC<-pf(F0_AC, dfAC, dfABC,lower.tail = F)
#BC
F0_BC<-MSBC/MSABC
Fcrit_BC<-qf(0.95, dfBC, dfABC)
pBC<-pf(F0_BC, dfBC, dfABC,lower.tail = F)
##Main Effects
#A
F0_A<-MSA/(MSAB+MSAC-MSABC)
Fcrit_A<-qf(0.95, dfA, dfAB+dfAC-dfABC)
pA<-pf(F0_A, dfA, dfAB+dfAC-dfABC,lower.tail = F)
#B
F0_B<-MSB/(MSAB+MSBC-MSABC)
Fcrit_B<-qf(0.95, dfB, dfAB+dfBC-dfABC)
pB<-pf(F0_B, dfB, dfAB+dfBC-dfABC,lower.tail = F)
#C
F0_C<-MSC/(MSAC+MSBC-MSABC)
Fcrit_C<-qf(0.95, dfC, dfAC+dfBC-dfABC)
pC<-pf(F0_C, dfC, dfAC+dfBC-dfABC,lower.tail = F)


####Problem 7.3####
###Data Input###
#tensile strength of the bars
tensile<-c(13.2, 15.2, 14.8, 14.6, 15.5, 15.0, 14.2, 15.1, 17.1, 16.5, 16.1,
           17.4, 16.7, 17.3, 15.4, 16.8, 14.1, 13.2, 14.5, 13.8, 14.8, 13.9,
           14.7, 13.5)
#alloy type
alloys<-factor(c(rep("A", 8), rep("B", 8), rep("C", 8)))
#Castings
castings<-factor(c(rep(c(1, 2, 3, 4), 6)))
#Bars
bars<-factor(rep(c(1, 1, 1, 1, 2, 2, 2, 2), 3))

#Datframe for the alloy-casting experiment
prb7_3.tibble<-tibble(tensile, alloys, castings, bars)

###Part a)###
##ANOVA##
#alloys is fixed; castings is nested in alloys, bars is nested in castings
prb7_3.aov<-aov(tensile~alloys+Error(alloys/castings/bars))

###Part c)###
##Means Squares
MSalloys<-14.69
MScastings<-0.5231
MSbars<-0.3625
##F test for alloys
F0_alloys<-MSalloys/MScastings
#critical F-Value: alloys df=2, castings df=9
Fcrit_alloys<-qf(0.95, 2, 9)
#p value
p_alloys<-pf(F0_alloys, 2, 9, lower.tail = F)

###Part d)###
##means and standard errors of alloys
muA<-mean(tensile[alloys=="A"])
muB<-mean(tensile[alloys=="B"])
muC<-mean(tensile[alloys=="C"])
sderrorA<-sd(tensile[alloys=="A"])/sqrt(length(tensile[alloys=="A"]))
sderrorB<-sd(tensile[alloys=="B"])/sqrt(length(tensile[alloys=="B"]))
sderrorC<-sd(tensile[alloys=="C"])/sqrt(length(tensile[alloys=="C"]))
##95% CI
E<-qt(0.975, df=length(tensile[alloys=="A"])-1)
CI95_A<-c(muA-E*sderrorA, muA+E*sderrorA)
CI95_B<-c(muB-E*sderrorB, muB+E*sderrorB)
CI95_C<-c(muC-E*sderrorC, muC+E*sderrorC)


####Problem 7.4####
###Data Input###
#traffic delay
delay<-c(61.7, 57.4, 53.1, 36.5, 35.8, 18.5, 35.5, 15.9, 20.0, 24.6, 17.0, 21.0,
         2.7, 3.1, 1.5, 1.1, 35.7, 26.8, 35.4, 20.7, 24.3, 25.9, 27.5, 23.3)
#signal type
signal<-factor(c(rep("Pretimed", 8), rep("Semi-actuated", 8),
                 rep("Fully actuated", 8)))
#intersection
intersection<-factor(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4), rep(5, 4),
                       rep(6, 4)))
#methods estimating time stopped at stop light
method<-factor(c(rep(c("Point-Sample", "Point-Sample", "Path-Trace",
                       "Path-Trace"), 6)))
#time of day (rush hour or nonrush hour)
time<-factor(rep(c("Rush", "Nonrush"), 12))

###Dataframe for problem 7.4###
prb7_4.tibble<-tibble(delay, signal, intersection, method, time)

###Part d)###