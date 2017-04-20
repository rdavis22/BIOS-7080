####BIOS 7080: Ch. 8 Homework####
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
if(!require(multcomp)){
  install.packages("multcomp"); library(multcomp)}

####Problem 8.1####
###Data Input###
#weight in pounds of harvested fruit
weight<-c(450, 469, 249, 125, 280, 352, 221, 251, 358, 512, 281, 58, 352, 293,
          283, 186, 331, 402, 183, 70, 258, 281, 219, 46, 317, 423, 379, 63, 289,
          239, 269, 357, 479, 341, 404, 115, 182, 349, 276, 182, 245, 380, 263,
          62, 336, 282, 171, 98)
#method of irrgation treatment
method<-factor(c(rep("Trickle", 8), rep("Basin", 8), rep("Spray", 8),
                 rep("Sprinkler", 8), rep("Sprinkler+Spray", 8), rep("Flood", 8)))
#block
block.prb8_1<-factor(rep(c(1, 2, 3, 4, 5, 6, 7, 8), 6))

##Dataframe for prb 8.1##
prb8_1.tibble<-tibble(weight, method, block.prb8_1)

###Part a)###
prb8_1.aov<-aov(weight~method+block.prb8_1, data=prb8_1.tibble)

###Part c)###
#MSE
MSE.prb8_1<-summary(prb8_1.aov)[[1]][['Mean Sq']][3]
#number of blocks
r.prb8_1<-length(levels(block.prb8_1))
#number of treatments
t.prb8_1<-length(levels(method))
#standard error of the treatment means
se_mu.prb8_1<-sqrt(MSE.prb8_1/r.prb8_1)
#standard error of the difference between two treatment means
se_delta.prb8_1<-sqrt(2*MSE.prb8_1/r.prb8_1)

###Part d)###
##means of irrigation methods##
mu_basin<-mean(weight[method=="Basin"])
mu_flood<-mean(weight[method=="Flood"])
mu_spray<-mean(weight[method=="Spray"])
mu_sprinkler<-mean(weight[method=="Sprinkler"])
mu_sprkspry<-mean(weight[method=="Sprinkler+Spray"])
mu_trickle<-mean(weight[method=="Trickle"])
##Dunnett Method for multiple comparisons##
#use 'contrMat' function to set 'Flood' as reference group in 'method'
refgrp<-contrMat(table(prb8_1.tibble$method), base=2) #"Flood" is 2nd level
#get the generalized linear hypothesis test model for the ANOVA
prb8_1.glht<-glht(prb8_1.aov, linfct=mcp(method=refgrp))
#95% Simultaneous Confidence Intervals (SCI)
SCI_95.prb8_1<-confint(prb8_1.glht)

###Part e)###
##Relative efficiency of RCBD vs. CRD
#Sum of squares of blocks
SSblock.prb8_1<-summary(prb8_1.aov)[[1]][['Sum Sq']][2]
#Residual Df for RCB
Dfrcb.prb8_1<-summary(prb8_1.aov)[[1]][['Df']][3]
#Residual Df for CR
Dfcr.prb8_1<-summary(aov(weight~method))[[1]][['Df']][2]
#estimated variance of complete randomized design
s2_cr.prb8_1<-(SSblock.prb8_1+r.prb8_1*(t.prb8_1-1)*MSE.prb8_1)/(r.prb8_1*t.prb8_1-1)

#Uncorrected RE
uncorrRE.prb8_1<-s2_cr.prb8_1/MSE.prb8_1

#Corrected RE
corrfactor.prb8_1<-((Dfrcb.prb8_1+1)*(Dfcr.prb8_1+3))/((Dfrcb.prb8_1+3)*(Dfcr.prb8_1+1)) #correction factor
corrRE.prb8_1<-corrfactor.prb8_1*uncorrRE.prb8_1

###Part f)###
#Residuals vs. Fitted values
plot(prb8_1.aov, 1)
#Normal (Q-Q) Plot
plot(prb8_1.aov, 2)
#Spread-Location Plot
plot(prb8_1.aov, 3)


####Problem 8.2####
###Data Input###
#100X%phosphorus in a plant tissue sample
phosphorus<-c(7.6, 8.1, 7.3, 7.9, 9.4, 7.3, 7.7, 7.7, 7.7, 8.2, 6.9, 6.0, 5.6, 
              7.4, 7.0, 10.8, 11.2, 9.0, 12.9, 11.6, 9.6, 9.3, 12.0, 10.6, 10.4)
#fertilizer treatment (make sure levels are in same order as book for contrasts)
treatment.prb8_2<-factor(c(rep("No fertilizer", 5), rep("50 lb. N2", 5), 
                    rep("100 lb. N2", 5), rep("50 lb. N2 + 75 lb. P2O5", 5),
                    rep("100 lb. N2 + 75 lb. P2O5", 5)), levels = c(
                      "No fertilizer", "50 lb. N2", "100 lb. N2",
                      "50 lb. N2 + 75 lb. P2O5", "100 lb. N2 + 75 lb. P2O5"))
#Block
block.prb8_2<-factor(rep(c(1, 2, 3, 4, 5), 5))

##DataFrame for problem 8.2
prb8_2.tibble<-tibble(phosphorus, treatment.prb8_2, block.prb8_2)

###Part a)###
##ANOVA##
prb8_2.aov<-aov(phosphorus~treatment.prb8_2+block.prb8_2, data = prb8_2.tibble)
#Mean Square
MSE.prb8_2<-summary(prb8_2.aov)[[1]][['Mean Sq']][3]
#number of replications (i.e. "blocks")
r.prb8_2<-length(levels(block.prb8_2))
#number of treatment groups
t.prb8_2<-length(levels(treatment.prb8_2))
#degrees of freedom of residual
DfRes.prb8_2<-summary(prb8_2.aov)[[1]][['Df']][3]
#Critical F Value
fc_crit.prb8_2<-(qf(0.95, 1, DfRes.prb8_2))

###Parts b) and c)###
##contrast matrix for 1 Df SS##
# #contrast 1: "No fertilizer" vs. all other treatments<-c(1, -1/4, -1/4, -1/4, -1/4)
# #contrast 2: main effect of nitrogen<-c(0, -1, 1, -1, 1)
# #contrast 3: main effect of phosphorus<-c(0, -1, -1, 1, 1)
# #contrast 4: interaction<-c(0, 1, -1, -1, 1)
mat<-matrix(c(c(1/5, 1/5, 1/5, 1/5, 1/5), c(1, -1/4, -1/4, -1/4, -1/4),
             c(0, -1, 1, -1, 1), c(0, -1, -1, 1, 1), c(0, 1, -1, -1, 1)), ncol=5)
#transpose and get the inverse
mymat=solve(t(mat))
# remove the intercept
my.contrasts<-mymat[,2:5]
#apply the contrasts to the fertilizer treatment variable
contrasts(prb8_2.tibble$treatment.prb8_2)=my.contrasts
#get a new anova with the contrasts
mod.prb8_2<-aov(phosphorus~treatment.prb8_2+block.prb8_2, data=prb8_2.tibble)
#display the contrasts by calling the following in the console:
summary(mod.prb8_2, split=list(treatment.prb8_2=list(NoTrt=1, N2Main=2, PhosMain=3, Intrct=4)))

##Alternate Method##
#contrast 1: "No fertilizer" vs. all other treatments<-c(1, -1/4, -1/4, -1/4, -1/4
C_1<-mean(phosphorus[treatment.prb8_2=="No fertilizer"])-
  0.25*mean(phosphorus[treatment.prb8_2=="50 lb. N2"])-
  0.25*mean(phosphorus[treatment.prb8_2=="100 lb. N2"])-
  0.25*mean(phosphorus[treatment.prb8_2=="50 lb. N2 + 75 lb. P2O5"])-
  0.25*mean(phosphorus[treatment.prb8_2=="100 lb. N2 + 75 lb. P2O5"])
#sum of squares for 'C_1'
SSC_1<-r.prb8_2*C_1^2/(sum(1^2+(-0.25)^2+(-0.25)^2)+(-0.25)^2+(-0.25)^2)
#F value for 'C_1'
f0_C_1<-SSC_1/MSE.prb8_2
#standard error of the contrast
se_C_1<-sqrt(MSE.prb8_2/r.prb8_2*sum(1^2, 4*(-0.25)^2))

#contrast 2: main effect of nitrogen<-c(0, -1, 1, -1, 1)
C_2<-0*mean(phosphorus[treatment.prb8_2=="No fertilizer"])-
  1*mean(phosphorus[treatment.prb8_2=="50 lb. N2"])+
  1*mean(phosphorus[treatment.prb8_2=="100 lb. N2"])-
  1*mean(phosphorus[treatment.prb8_2=="50 lb. N2 + 75 lb. P2O5"])+
  1*mean(phosphorus[treatment.prb8_2=="100 lb. N2 + 75 lb. P2O5"])
#sum of squares for 'C_2'
SSC_2<-r.prb8_2*C_2^2/(sum(0^2+(-1)^2+1^2+(-1)^2+1^2))
#F value for 'C_2'
f0_C_2<-SSC_2/MSE.prb8_2
#standard error of the contrast
se_C_2<-sqrt(MSE.prb8_2/r.prb8_2*((0^2+(-1)^2+1^2+(-1)^2+1^2)))

#contrast 3: main effect of phosphorus<-c(0, -1, -1, 1, 1)
C_3<-0*mean(phosphorus[treatment.prb8_2=="No fertilizer"])-
  1*mean(phosphorus[treatment.prb8_2=="50 lb. N2"])-
  1*mean(phosphorus[treatment.prb8_2=="100 lb. N2"])+
  1*mean(phosphorus[treatment.prb8_2=="50 lb. N2 + 75 lb. P2O5"])+
  1*mean(phosphorus[treatment.prb8_2=="100 lb. N2 + 75 lb. P2O5"])
#sum of squares for 'C_3'
SSC_3<-r.prb8_2*C_3^2/((0^2+(-1)^2+(-1)^2+1^2+1^2))
#F value for 'C_3'
f0_C_3<-SSC_3/MSE.prb8_2
#standard error of the contrast
se_C_3<-sqrt(MSE.prb8_2/r.prb8_2*((0^2+(-1)^2+(-1)^2+1^2+1^2)))

#contrast 4: interaction<-c(0, 1, -1, -1, 1)
C_4<-mean(0*phosphorus[treatment.prb8_2=="No fertilizer"])+
  1*mean(phosphorus[treatment.prb8_2=="50 lb. N2"])-
  1*mean(phosphorus[treatment.prb8_2=="100 lb. N2"])-
  1*mean(phosphorus[treatment.prb8_2=="50 lb. N2 + 75 lb. P2O5"])+
  1*mean(phosphorus[treatment.prb8_2=="100 lb. N2 + 75 lb. P2O5"])
#sum of squares for 'C_4'
SSC_4<-r.prb8_2*C_4^2/((0^2+1^2+(-1)^2+(-1)^2+1^2))
#F value for 'C_4'
f0_C_4<-SSC_4/MSE.prb8_2
#standard error of the contrast
se_C_4<-sqrt(MSE.prb8_2/r.prb8_2*((0^2+1^2+(-1)^2+(-1)^2+1^2)))

###Part d)###
##Relative efficiency of RCBD vs. CRD
#Sum of squares of blocks
SSblock.prb8_2<-summary(prb8_2.aov)[[1]][['Sum Sq']][2]
#Residual Df for RCB
Dfrcb.prb8_2<-summary(prb8_2.aov)[[1]][['Df']][3]
#Residual Df for CR
Dfcr.prb8_2<-summary(aov(phosphorus~treatment.prb8_2))[[1]][['Df']][2]
#estimated variance of complete randomized design
s2_cr.prb8_2<-(SSblock.prb8_2+r.prb8_2*(t.prb8_2-1)*MSE.prb8_2)/(r.prb8_2*t.prb8_2-1)

#Uncorrected RE
uncorrRE.prb8_2<-s2_cr.prb8_2/MSE.prb8_2

#Corrected RE
corrfactor.prb8_2<-((Dfrcb.prb8_2+1)*(Dfcr.prb8_2+3))/((Dfrcb.prb8_2+3)*(Dfcr.prb8_2+1)) #correction factor
corrRE.prb8_2<-corrfactor.prb8_2*uncorrRE.prb8_2

###Part e)###
##Residual Plots
#Residuals vs. Fitted values
plot(prb8_2.aov, 1)
#Normal (Q-Q) Plot
plot(prb8_2.aov, 2)
#Spread-Location Plot
plot(prb8_2.aov, 3)


####Problem 8.4####
###Data Input###
#total unused redlight time
URLtime<-c(15.2, 33.8, 13.5, 27.4, 29.1, 16.5, 26.5, 19.2, 25.8, 22.7, 12.1,
           31.4, 17.0, 31.5, 30.2, 10.7, 34.2, 19.5, 27.2, 21.6, 14.6, 31.7,
           16.7, 26.3, 23.8)
#intersection
intersection<-factor(c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5), rep(5, 5)))
#time period
period<-factor(c(rep(c(1, 2, 3, 4, 5), 5)))
#signal sequence treatments for the red lights
treatment.prb8_4<-factor(c("A", "B", "C", "D", "E", "B", "C", "D", "E", "A",
                           "C", "D", "E", "A", "B", "D", "E", "A", "B", "C",
                           "E", "A", "B", "C", "D"))

#DataFrame for problem 8.4
prb8_4.tibble<-tibble(URLtime, intersection, period, treatment.prb8_4)

###Part a)###
##ANOVA##
prb8_4.aov<-aov(URLtime~intersection+period+treatment.prb8_4, data = prb8_4.tibble)
#MSE
MSE.prb8_4<-summary(prb8_4.aov)[[1]][['Mean Sq']][4]
#number of replications
r.prb8_4<-length(levels(period))
#number of treatments
t.prb8_4<-length(levels(treatment.prb8_4))

###Part b)###
##Standard Errors
#Standard error for signal sequence treatment mean
se_y_k<-sqrt(MSE.prb8_4/t.prb8_4)
#Standard error for difference between signal sequence treatment mean
se_diffy_k<-sqrt(2*MSE.prb8_4/t.prb8_4)

###Part c)###
##Multiple Comparison Test with Best (i.e. "MCB")
##mean unusued red light time for each treatment
mu_vec<-c(
  #A
  mu_A.prb8_4<-mean(URLtime[treatment.prb8_4=="A"]),
  #B
  mu_B.prb8_4<-mean(URLtime[treatment.prb8_4=="B"]),
  #C
  mu_C.prb8_4<-mean(URLtime[treatment.prb8_4=="C"]),
  #D
  mu_D.prb8_4<-mean(URLtime[treatment.prb8_4=="D"]),
  #E
  mu_E.prb8_4<-mean(URLtime[treatment.prb8_4=="E"])
)
#get the minimum sample mean (in this case, treatment B is maximal)

##Method 1##
#The mean of group B is the maximum value
D_AC<-mu_A.prb8_4-mu_C.prb8_4
D_BC<-mu_B.prb8_4-mu_C.prb8_4
D_DC<-mu_D.prb8_4-mu_C.prb8_4
D_EC<-mu_E.prb8_4-mu_C.prb8_4
D_CD<-mu_C.prb8_4-mu_D.prb8_4

#d(alpha=0.05, k=4 (t-1), v=12, one-sided)=2.41 #Appendix VI
d.prb8_4<-2.41
#get the 'M' value for MCB procedure
M.prb8_4<-d.prb8_4*sqrt(2*MSE.prb8_4/r.prb8_4)

#95%CI for treatment means
CI95_A<-c(D_AC-M.prb8_4, D_AC+M.prb8_4)
CI95_B<-c(D_BC-M.prb8_4, D_BC+M.prb8_4)
CI95_C<-c(D_CD-M.prb8_4, D_CD+M.prb8_4)
CI95_D<-c(D_DC-M.prb8_4, D_DC+M.prb8_4)
CI95_E<-c(D_EC-M.prb8_4, D_EC+M.prb8_4)

#95% Simultaneous CI for treatment means
SCI95_A<-c(0, D_AC+M.prb8_4)
SCI95_B<-c(0, D_BC+M.prb8_4)
SCI95_C<-c(D_CD-M.prb8_4, 0)
SCI95_D<-c(D_DC-M.prb8_4, 0)
SCI95_E<-c(D_EC-M.prb8_4, 0)

###Part d)###
##Relative efficiency of "Time period" (i.e. period) blocking
#MS of "period" column block
MSperiod<-summary(prb8_4.aov)[[1]][['Mean Sq']][2]
#Residual Df for LS Design
Dfls.prb8_4<-summary(prb8_4.aov)[[1]][['Df']][4]
#Residual Df for RCB Design
Dfrcb.prb8_4<-summary(aov(URLtime~treatment.prb8_4+intersection))[[1]][['Df']][3]
#estimated variance of complete randomized design
s2_rcb.prb8_4<-(MSperiod+(t.prb8_4-1)*MSE.prb8_4)/(t.prb8_4)

#Uncorrected RE
uncorrRE.prb8_4<-s2_rcb.prb8_4/MSE.prb8_4

#Corrected RE
corrfactor.prb8_4<-((Dfls.prb8_4+1)*(Dfrcb.prb8_4+3))/((Dfls.prb8_4+3)*(Dfrcb.prb8_4+1)) #correction factor
corrRE.prb8_4<-corrfactor.prb8_4*uncorrRE.prb8_4

###Part e)###
#Residuals vs. Fitted values
plot(prb8_4.aov, 1)
#Normal (Q-Q) Plot
plot(prb8_4.aov, 2)
#Spread-Location Plot
plot(prb8_4.aov, 3)


####Problem 8.5####
###Data Input###
#construction times in minutes
minutes<-c(90, 96, 84, 88, 90, 91, 96, 88, 89, 97, 98, 98, 104, 100, 104, 106)
#time period
period.prb8_5<-factor(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)))
#technician used in construction of electronic component
technician<-factor(rep(c(1, 2, 3, 4), 4))
#four construction methods
method.prb8_5<-factor(c("C", "D", "A", "B", "B", "C", "D", "A", "A", "B", "C",
                        "D", "D", "A", "B", "C"))

##Dataframe
prb8_5.tibble<-tibble(minutes, period.prb8_5, technician, method.prb8_5)

###Part a)###
prb8_5.aov<-aov(minutes~period.prb8_5+technician+method.prb8_5)

#MSE
MSE.prb8_5<-summary(prb8_5.aov)[[1]][['Mean Sq']][4]
#number of replications
r.prb8_5<-length(levels(technician))
#number of treatments
t.prb8_5<-length(levels(method.prb8_5))

###Part b)###
##Standard Errors
#Standard error for construction method mean
se_y_k.prb8_5<-sqrt(MSE.prb8_5/t.prb8_5)
#Standard error for difference between two construction method means
se_diffy_k.prb8_5<-sqrt(2*MSE.prb8_5/t.prb8_5)

###Part c)###
##Multiple Comparison test using Tukey Test
#Method 1): "TukeyHSD" function
TukHSD.prb8_5<-TukeyHSD(prb8_5.aov)$method.prb8_5

#Method 2): "glht" method
prb8_5.glht<-glht(prb8_5.aov, linfct = mcp(method.prb8_5 ="Tukey"))
SCI95_glht.prb8_5<-confint(prb8_5.glht)

###Part d)###
##Relative efficiency of "Time period" (i.e. period) blocking
#MS of "period" column block
MSperiod.prb8_5<-summary(prb8_5.aov)[[1]][['Mean Sq']][1]
#Residual Df for LS Design
Dfls.prb8_5<-summary(prb8_5.aov)[[1]][['Df']][4]
#Residual Df for RCB Design w/o "Time period" block
Dfrcb.prb8_5<-summary(aov(minutes~technician+method.prb8_5))[[1]][['Df']][3]
#estimated variance of complete randomized design
s2_rcb.prb8_5<-(MSperiod.prb8_5+(t.prb8_5-1)*MSE.prb8_5)/(t.prb8_5)

#Uncorrected RE
uncorrRE.prb8_5<-s2_rcb.prb8_5/MSE.prb8_5

#Corrected RE
corrfactor.prb8_5<-((Dfls.prb8_5+1)*(Dfrcb.prb8_5+3))/((Dfls.prb8_5+3)*(Dfrcb.prb8_5+1)) #correction factor
corrRE.prb8_5<-corrfactor.prb8_5*uncorrRE.prb8_5


####Problem 8.8####
###Data input###
#%percent 15N taken up by sweet corn plants
N15<-c(21.4, 50.8, 53.2, 54.8, 56.9, 57.7, 11.3, 42.7, 44.8, 47.9, 46.8,
          54.0, 34.9, 61.8, 57.8, 40.1, 57.9, 62.0)
#timing of nitrogen application
timing<-factor(rep(c("Early", "Optimum", "Late"), 6))
#nitrogen inhibitor
inhibitor<-factor(rep(c("None", "None", "None", ".5lb/acre", ".5lb/acre",
                        ".5lb/acre"), 3))
#block
block.prb8_8<-factor(c(rep(1, 6), rep(2, 6), rep(3, 6)))
#Dataframe for problem 8.8
prb8_8.tibble<-tibble(N15, block.prb8_8, timing, inhibitor)

###Part a)###
##ANOVA##
prb8_8.aov<-aov(N15~block.prb8_8+timing*inhibitor, data=prb8_8.tibble)

##Other important values
#MSE for problem 8.8
MSE.prb8_8<-summary(prb8_8.aov)[[1]][['Mean Sq']][5]
#Mean Square timing
MStiming.prb8_8<-summary(prb8_8.aov)[[1]][['Mean Sq']][2]
#Mean Square inhibitor
MSinhibitor.prb8_8<-summary(prb8_8.aov)[[1]][['Mean Sq']][3]
#Mean Square interaction
MSintrct.prb8_8<-summary(prb8_8.aov)[[1]][['Mean Sq']][4]
#Degrees of Freedom
DFtiming<-summary(prb8_8.aov)[[1]][['Df']][2]
DFinhibitor<-summary(prb8_8.aov)[[1]][['Df']][3]
DFintrct<-summary(prb8_8.aov)[[1]][['Df']][4]
DFError<-summary(prb8_8.aov)[[1]][['Df']][5]
#number of replications
r.prb8_8<-length(levels(block.prb8_8))
#number of timing treatments
a.prb8_8<-length(levels(timing))
#number of inihbitor treatments
b.prb8_8<-length(levels(inhibitor))
#number of treatments for the corn plost
t.prb8_8<-a.prb8_8*b.prb8_8

###Part b)###
##Standard Errors
#Standard error for marginal means for inhibitor 
se_inhibitor.prb8_8<-sqrt(MSE.prb8_8/r.prb8_8*a.prb8_8)
#Standard error for marginal means for timing
se_timing.prb8_8<-sqrt(2*MSE.prb8_8/r.prb8_8*b.prb8_8)

##cell means for N15 for each nitrogen inhibitor and timing combination
cellmu_i1t1<-mean(N15[inhibitor=="None"& timing=="Early"])
cellmu_i1t2<-mean(N15[inhibitor=="None"& timing=="Optimum"])
cellmu_i1t3<-mean(N15[inhibitor=="None"& timing=="Late"])
cellmu_i2t1<-mean(N15[inhibitor==".5lb/acre"& timing=="Early"])
cellmu_i2t2<-mean(N15[inhibitor==".5lb/acre"& timing=="Optimum"])
cellmu_i2t3<-mean(N15[inhibitor==".5lb/acre"& timing=="Late"])

###Part c)###
##Hypothesis testing##
#timing factor
F0_timing<-MStiming.prb8_8/MSE.prb8_8
Fcrit_timing<-qf(0.95, DFtiming, DFError)
pval_timing<-pf(F0_timing, DFtiming, DFError, lower.tail = F)

#inhibitor factor
F0_inhibitor<-MSinhibitor.prb8_8/MSE.prb8_8
Fcrit_inhibitor<-qf(0.95, DFinhibitor, DFError)
pval_inhibitor<-pf(F0_inhibitor, DFinhibitor, DFError, lower.tail = F)

#interaction factor
F0_intrct<-MSintrct.prb8_8/MSE.prb8_8
Fcrit_intrct<-qf(0.95, DFintrct, DFError)
pval_intrct<-pf(F0_intrct, DFintrct, DFError, lower.tail = F)

###Part d)###
#sum of square of the block design
SSblock.prb8_8<-summary(prb8_8.aov)[[1]][['Sum Sq']][1]
#Residual Df for RCB
Dfrcb.prb8_8<-summary(prb8_8.aov)[[1]][['Df']][5]
#Residual Df for CR
Dfcr.prb8_8<-summary(aov(N15~timing+inhibitor+timing:inhibitor))[[1]][['Df']][4]
#estimated variance of complete randomized design
s2_cr.prb8_8<-(SSblock.prb8_8+r.prb8_8*(t.prb8_8-1)*MSE.prb8_8)/(r.prb8_8*t.prb8_8-1)

#Uncorrected RE
uncorrRE.prb8_8<-s2_cr.prb8_8/MSE.prb8_8

#Corrected RE
corrfactor.prb8_8<-((Dfrcb.prb8_8+1)*(Dfcr.prb8_8+3))/((Dfrcb.prb8_8+3)*(Dfcr.prb8_8+1)) #correction factor
corrRE.prb8_8<-corrfactor.prb8_8*uncorrRE.prb8_8

###Part e)###
##Residual Plots
plot(prb8_8.aov, 1)
plot(prb8_8.aov, 2)
plot(prb8_8.aov, 3)