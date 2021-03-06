####BIOS 7080: Exam 3####
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
if(!require(agricolae)){
  install.packages("agricolae"); library(agricolae)}

####Problem 1####
###Data Input###
#'sales' in thousands of dollars (response variable)
sales<-c(1.2, 1.5, 1.0, 1.7, 1.4, 1.9, 1.6, 1.5, 2.8, 2.1, 2.7, 2.0, 3.4, 2.5,
         2.9, 2.7)
#sales volume (row blocking factor)
volume<-factor(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)))
#geographic location (column blocking factor)
location<-factor(rep(c("Northeast", "Northwest", "Southeast", "Southwest"), 4))
#price level (treatment factor)
price_level<-factor(c("B", "C", "A", "D", "A", "D", "B", "C", "C", "B", "D",
                      "A", "D", "A", "C", "B"))

##Dataframe for Problem 1##
prb1.tibble<-tibble(sales, volume, location, price_level)

###Part b)###
##ANOVA##
prb1.aov<-aov(sales~volume+location+price_level, data = prb1.tibble)
#total sum of squares
TSS.prb1<-sum(summary(prb1.aov)[[1]][['Sum Sq']])

###Part c)###
##variables needed to calculate relative efficiencies of rows and columns##
#number of treatment levels in the "price_level" treatment factor
t.prb1<-length(levels(price_level))
#Mean Squares of Rows ('Volume' variable)
MSRows.prb1<-summary(prb1.aov)[[1]][['Mean Sq']][1]
#Mean Squares Columns ('location' variable)
MSColumns.prb1<-summary(prb1.aov)[[1]][['Mean Sq']][2]
#MSE
MSE.prb1<-summary(prb1.aov)[[1]][['Mean Sq']][4]
#Degrees of freedom for the Error for the LAtin Squares design
Dfls.prb1<-summary(prb1.aov)[[1]][['Df']][4]
#Degrees of freedom for the Error for RCBD (will be same for rows and cols in LS design)
Dfrcb.prb1<-summary(aov(sales~location+price_level))[[1]][['Df']][3]

##relative efficiency of row blocking ('volume' variable)
#estimated MSE for row blocking only
s2_rcbrow.prb1<-(MSRows.prb1+(t.prb1-1)*MSE.prb1)/t.prb1
#correction factor for estimating the variance for the RCBD
K.prb1<-((Dfls.prb1+1)*(Dfrcb.prb1+3))/((Dfls.prb1+3)*(Dfrcb.prb1+1))
#Relative efficiency of Row blocking
RErow.prb1<-s2_rcbrow.prb1/MSE.prb1
#corrected RE of Row blocking 
corrRErow.prb1<-K.prb1*RErow.prb1

##relative efficiency of row blocking ('Column' variable)
#estimated MSE for column blocking only
s2_rcbcol.prb1<-(MSColumns.prb1+(t.prb1-1)*MSE.prb1)/t.prb1
#Relative efficiency of Row blocking
REcol.prb1<-s2_rcbcol.prb1/MSE.prb1
#corrected RE of Row blocking 
corrREcol.prb1<-K.prb1*REcol.prb1

###Part d)###
##Multiple comparions using Tukey##
#multiple comparison method using TukeyHSD
prb1.TukeyHSD<-TukeyHSD(prb1.aov, conf.level=0.90)$price_level
#multiple comparison method using Tukey (GLHT)
prb1.glht<-glht(prb1.aov, linfct = mcp(price_level ="Tukey"))
SCI90prb1<-confint(prb1.glht, level = 0.90)


####Problem 2####
###Data Input###
#strength of the cloth
cloth<-c(73, 68, 74, 71, 67, 73, 67, 75, 72, 70, 75, 68, 78, 73, 68, 73, 71,
         75, 75, 79)
#"bolt" block
bolt<-factor(rep(c(1, 2, 3, 4, 5), 4))
#"Chemical" treatment factor
chemical<-factor(c(rep(1, 5), rep(2, 5), rep(3, 5), rep(4, 5)))

##Dataframe for problem 2##
prb2.tibble<-tibble(cloth, bolt, chemical)

###Parts b and c)###
##ANOVA##
prb2.aov<-aov(cloth~bolt+chemical, data = prb2.tibble)

##Levene test## (homogeneity of variances
#Levene test for treatment
lvnetrt.prb2<-leveneTest(cloth~chemical, center=median)
#Levene test for block
lvneblk.prb2<-leveneTest(cloth~bolt, center=median)

##Plots##
#Normality assumption (Q-Q Plot)
plot(prb2.aov, 2)
#Homoscedasticity assumption (Scale-Location Plot)
plot(prb2.aov, 3)

##Hypothesis Testing
#MSChemical
MSChemical.prb2<-summary(prb2.aov)[[1]][['Mean Sq']][2]
#MSE
MSE.prb2<-summary(prb2.aov)[[1]][['Mean Sq']][3]
#DFChemical
DFChemical<-summary(prb2.aov)[[1]][['Df']][2]
#DFError
DFError.prb2<-summary(prb2.aov)[[1]][['Df']][3]
#F0
F0_chemical<-MSChemical.prb2/MSE.prb2
#Critical value for Chemical treatment
Fcrit_chemical<-qf(0.95, DFChemical, DFError.prb2)
#pval for the F test
pval_chemical<-pf(F0_chemical, DFChemical, DFError.prb2, lower.tail = F)

###Part d)###
#get the p-values for the Fisher LSD test
Fisherpvals<-with(prb2.tibble, pairwise.t.test(x=cloth, g=chemical,
                                               p.adjust="none"))
#from the 'agricolae' package (contains t value and LSD critical value)
flsd.prb2<-LSD.test(cloth, chemical, DFerror = DFError.prb2, MSerror = MSE.prb2)
#means of the chemicals
chem1mu<-mean(cloth[chemical==1])
chem2mu<-mean(cloth[chemical==2])
chem3mu<-mean(cloth[chemical==3])
chem4mu<-mean(cloth[chemical==4])

#All Fisher pairwise differences
pwcchem43<-chem4mu-chem3mu
pwcchem42<-chem4mu-chem2mu
pwcchem41<-chem4mu-chem1mu
pwcchem32<-chem3mu-chem2mu
pwcchem31<-chem3mu-chem1mu
pwcchem21<-chem2mu-chem1mu


####Problem 3####
###Data Input###
#grapefruit sales response variable
sales.prb3<-c(62.1, 61.3, 60.8, 58.2, 57.9, 55.1, 51.6, 49.2, 46.2, 53.7, 51.5,
              48.3, 61.4, 58.7, 56.6, 58.5, 57.2, 54.3, 46.8, 43.2, 41.5, 51.2,
              49.8, 47.9)
#store blocking factor
store<-factor(c(rep(1,3), rep(2,3), rep(3,3), rep(4,3), rep(5,3), rep(6,3), 
                rep(7,3), rep(8,3)))
#'price_level' treatment factor
price_level.prb3<-factor(rep(c(1, 2, 3), 8))

##Dataframe for problem 3
prb3.tibble<-tibble(sales.prb3, store, price_level.prb3)

###Part b)###
##ANOVA##
prb3.aov<-aov(sales.prb3~store+price_level.prb3, data = prb3.tibble)

##hypothesis testing##
#Mean Square 'price_level.prb3' treatment
MSpl.prb3<-summary(prb3.aov)[[1]][['Mean Sq']][2]
#Mean Square Error
MSE.prb3<-summary(prb3.aov)[[1]][['Mean Sq']][3]
#DF price_level.prb3' treatment
DFpl.prb3<-summary(prb3.aov)[[1]][['Df']][2]
#Mean Square Error
DFE.prb3<-summary(prb3.aov)[[1]][['Df']][3]

#FTest
F0_price_level.prb3<-MSpl.prb3/MSE.prb3
Fcrit_pl.prb3<-qf(0.95, DFpl.prb3, DFE.prb3)
pval_pl.prb3<-pf(F0_price_level.prb3, DFpl.prb3, DFE.prb3, lower.tail = F)

###Part c)###
##contrasts##
#1 vs. 2
C1<-c(1, -1, 0)
#1 vs. 3
C2<-c(1, 0, -1)
#2 vs. 3
C3<-c(0, 1, -1)
#Contrast Matrix
contmat.prb3<-matrix(c(C1, C2, C3), ncol=3, byrow=T)

##Test for orthogonality##
orthoC1C2<-sum(C1*C2)
orthoC1C3<-sum(C1*C3)
orthoC2C3<-sum(C2*C3)

#see if the matrix is orthogonal
orthomat.prb3<-det(contmat.prb3)

###Part d)###
##Relative efficiency of RCBD vs. CRD
#number of levels of 'store' block
r.prb3<-length(levels(store))
#number of levels of 'price level' treatment
t.prb3<-length(levels(price_level.prb3))
#Sum of squares of blocks (i.e. 'store' blocking factor)
SSstore.prb3<-summary(prb3.aov)[[1]][['Sum Sq']][1]
#Residual Df for RCBD
Dfrcb.prb3<-summary(prb3.aov)[[1]][['Df']][3]
#Residual Df for CRD
Dfcr.prb3<-summary(aov(sales.prb3~price_level.prb3))[[1]][['Df']][2]
#estimated variance of complete randomized design
s2_cr.prb3<-(SSstore.prb3+r.prb3*(t.prb3-1)*MSE.prb3)/(r.prb3*t.prb3-1)

#Uncorrected RE
uncorrRE.prb3<-s2_cr.prb3/MSE.prb3

#Corrected RE
corrfactor.prb3<-((Dfrcb.prb3+1)*(Dfcr.prb3+3))/((Dfrcb.prb3+3)*(Dfcr.prb3+1)) #correction factor
corrRE.prb3<-corrfactor.prb3*uncorrRE.prb3

####Problem 4####
###Data Input###
#percentage reflectance of pigment
reflectance<-c(64.5, 66.3, 74.1, 66.5, 68.3, 69.5, 73.8, 70.0, 70.3, 73.1, 78.0,
               72.3, 65.2, 65.0, 73.8, 64.8, 69.2, 70.3, 74.5, 68.3, 71.2, 72.8,
               79.1, 71.5, 66.2, 66.5, 72.3, 67.7, 69.0, 69.0, 75.4, 68.6, 70.8,
               74.2, 80.1, 72.4)
#application method of paint (1=brushing, 2=spraying, roling=3) (sub-plot factor)
application<-factor(rep(c(rep(1, 4), rep(2, 4), rep(3, 4)), 3))
#paint mixing method (whole-plot effect)
mix<-factor(rep(c(1, 2, 3, 4), 9))
#'Day' block effect
day<-factor(c(rep(1, 12), rep(2, 12), rep(3, 12)))

##Dataframe for Problem 4##
prb4.tibble<-tibble(reflectance, application, mix, day)

###Part b)###
##ANOVA##
prb4.aov<-aov(reflectance~mix*application+Error(day/mix), data = prb4.tibble)

##terms for hypothesis testing##
#number of 'days' (i.e. replications)
r.prb4<-length(levels(day))
#number of 'mix' whole-plot treatments
a.prb4<-length(levels(mix))
#number of 'application' sub-plot treatments
b.prb4<-length(levels(application))
#MSE of the "whole-plot"
MSE1.prb4<-summary(prb4.aov)[[2]][[1]][['Mean Sq']][2]
#Df E1
DfE1.prb4<-summary(prb4.aov)[[2]][[1]][['Df']][2]
#MSE of the "sub-plots"
MSE2.prb4<-summary(prb4.aov)[[3]][[1]][['Mean Sq']][3]
#Df E1
DfE2.prb4<-summary(prb4.aov)[[3]][[1]][['Df']][3]

##Mean Squares##
#Mean Square of mix
MSmix.prb4<-summary(prb4.aov)[[2]][[1]][['Mean Sq']][1]
#Mean Square application
MSapplication.prb4<-summary(prb4.aov)[[3]][[1]][['Mean Sq']][1]
#Mean Square mix:application
MSintrct.prb4<-summary(prb4.aov)[[3]][[1]][['Mean Sq']][2]

##Degrees of Freedom##
#Df mix
Dfmix.prb4<-summary(prb4.aov)[[2]][[1]][['Df']][1]
#Df application
Dfapplication.prb4<-summary(prb4.aov)[[3]][[1]][['Df']][1]
#Df mix:application
Dfintrct.prb4<-summary(prb4.aov)[[3]][[1]][['Df']][2]

##F Tests##
#F Test for Interaction
F0_intrct.prb4<-MSintrct.prb4/MSE2.prb4
#critical F value (F(0.05, Dfintrct.prb4, DfE2.prb4)
Fcrit_intrct<-qf(0.95, Dfintrct.prb4, DfE2.prb4)
#p_value
pF0_intrct<-pf(F0_intrct.prb4, Dfintrct.prb4, DfE2.prb4,lower.tail = F)

#F Test for 'mix'
F0_mix.prb4<-MSmix.prb4/MSE1.prb4
#critical F value (F(0.05, Dfmix.prb4, DfE1.prb4)
Fcrit_mix<-qf(0.95, Dfmix.prb4, DfE1.prb4)
#p_value
pF0_mix<-pf(F0_mix.prb4, Dfmix.prb4, DfE1.prb4,lower.tail = F)

#F Test for 'application'
F0_application.prb4<-MSapplication.prb4/MSE2.prb4
#critical F value (F(0.05, Dfapplication.prb4, DfE1.prb4)
Fcrit_application<-qf(0.95, Dfapplication.prb4, DfE2.prb4)
#p_value
pF0_application<-pf(F0_application.prb4, Dfapplication.prb4, DfE2.prb4,lower.tail = F)

###Part c)###
#error degrees of freedom for the RCBD
f2.prb4<-(a.prb4*b.prb4-1)*(r.prb4-1)
#"K" Factors for subplot and whole plot design
Ksub.prb4<-((DfE2.prb4+1)*(f2.prb4+3))/((DfE2.prb4+3)*(f2.prb4+1))
Kwhol.prb4<-((DfE1.prb4+1)*(f2.prb4+3))/((DfE1.prb4+3)*(f2.prb4+1))

##Relative Efficiency for subplot comparisons##
REsub.prb4<-Ksub.prb4*((a.prb4*(b.prb4-1)*MSE2.prb4)
                             +(a.prb4-1)*MSE1.prb4)/((a.prb4*b.prb4-1)*MSE2.prb4)

##Relative Efficiency for whole plot comparisons##
REwhol.prb4<-Kwhol.prb4*((a.prb4*(b.prb4-1)*MSE2.prb4)
                               +(a.prb4-1)*MSE1.prb4)/((a.prb4*b.prb4-1)*MSE1.prb4)

###Part d)###
##grand mean of 'reflectance'
mureflectance<-mean(reflectance)
##Cell means##
celmu.list<-list(
  #mean for mix==1, application==1
  celmu_11<-mean(reflectance[mix==1 & application==1]),
  #mean for mix==2, application==1
  celmu_21<-mean(reflectance[mix==2 & application==1]),
  #mean for mix==3, application==1
  celmu_31<-mean(reflectance[mix==3 & application==1]),
  #mean for mix==4, application==1
  celmu_41<-mean(reflectance[mix==4 & application==1]),
  #mean for mix==1, application==2
  celmu_12<-mean(reflectance[mix==1 & application==2]),
  #mean for mix==2, application==2
  celmu_22<-mean(reflectance[mix==2 & application==2]),
  #mean for mix==3, application==2
  celmu_32<-mean(reflectance[mix==3 & application==2]),
  #mean for mix==4, application==2
  celmu_42<-mean(reflectance[mix==4 & application==2]),
  #mean for mix==1, application==3
  celmu_13<-mean(reflectance[mix==1 & application==3]),
  #mean for mix==2, application==3
  celmu_23<-mean(reflectance[mix==2 & application==3]),
  #mean for mix==3, application==3
  celmu_33<-mean(reflectance[mix==3 & application==3]),
  #mean for mix==4, application==3
  celmu_43<-mean(reflectance[mix==4 & application==3])
)
##Marginal Means for 'application'
marmuapplication.list<-list(
  marmu_app1<-mean(reflectance[application==1]),
  marmu_app2<-mean(reflectance[application==2]),
  marmu_app3<-mean(reflectance[application==3])
)
##Marginal Means for 'mix'
marmumix.list<-list(
  marmu_mix1<-mean(reflectance[mix==1]),
  marmu_mix2<-mean(reflectance[mix==2]),
  marmu_mix3<-mean(reflectance[mix==3]),
  marmu_mix4<-mean(reflectance[mix==4])
)

#cell means standard errors
secelmu.prb4<-sqrt(MSE2.prb4/r.prb4)
#marginal mean standard errors for 'mix'
semarmumix.prb4<-sqrt(MSE1.prb4/(r.prb4*b.prb4))
#marginal mean standard errors for 'application'
semarmuapp.prb4<-sqrt(MSE2.prb4/(r.prb4*a.prb4))


####Problem 5####
###Data Input###
#strength ofaluminum alloy
strength<-c(1.230, 1.346, 1.235, 1.301, 1.346, 1.315, 1.247, 1.275, 1.324,
            1.259, 1.400, 1.206, 1.263, 1.392, 1.320, 1.296, 1.268, 1.315,
            1.316, 1.329, 1.250, 1.274, 1.384, 1.346, 1.273, 1.260, 1.392,
            1.300, 1.362, 1.239, 1.268, 1.375, 1.357, 1.264, 1.265, 1.364,
            1.287, 1.346, 1.273, 1.247, 1.362, 1.336, 1.301, 1.280, 1.319,
            1.292, 1.382, 1.215, 1.215, 1.328, 1.342, 1.262, 1.271, 1.323)
#bar size
bar_size<-factor(c(rep("1 inch", 18), rep("1.5 inches", 18),
                   rep("2 inches", 18)))
#Vendors
Vendors<-factor(rep(c(rep("Vendor 1", 3), rep("Vendor 2", 3),
                      rep("Vendor 3", 3)), 6))
#Heat Level
heat_level<-factor(rep(c(1, 2, 3, 1, 2, 3, 1, 2, 3), 6))

##Dataframe for Problem 5 data##
prb5.tibble<-tibble(strength, bar_size, Vendors, heat_level)

###Part b)###
##ANOVA##
#model with C ('heat level') nested in B ('Vendors) and crossed with A ('bar_size')
prb5.aov<-aov(strength~bar_size*Vendors*heat_level%in%Vendors,
              data = prb5.tibble)

##Hypothesis Testing##
#Degrees of Freedom#
DfA<-summary(prb5.aov)[[1]][['Df']][1] #bar_size
DfB<-summary(prb5.aov)[[1]][['Df']][2] #Vendors
DfAB<-summary(prb5.aov)[[1]][['Df']][3] #bar_size:Vendors
DfBC<-summary(prb5.aov)[[1]][['Df']][4] #heat_level %in% Vendors
DfABC<-summary(prb5.aov)[[1]][['Df']][5] #heat_level %in% Vendors:bar_size
DfRes<-summary(prb5.aov)[[1]][['Df']][6] #residuals
#Mean Squares
MSA<-summary(prb5.aov)[[1]][['Mean Sq']][1]
MSB<-summary(prb5.aov)[[1]][['Mean Sq']][2]
MSAB<-summary(prb5.aov)[[1]][['Mean Sq']][3]
MSBC<-summary(prb5.aov)[[1]][['Mean Sq']][4]
MSABC<-summary(prb5.aov)[[1]][['Mean Sq']][5]
MSRes<-summary(prb5.aov)[[1]][['Mean Sq']][6]
#Hypothesis tests
F0_A<-MSA/MSABC #test for "A" (bar_level)
Fcrit_A<-qf(0.95, DfA, DfABC)
pA<-pf(F0_A, DfA, DfABC, lower.tail = F)

F0_B<-MSB/MSBC #test for "B" ("Vendors")
Fcrit_B<-qf(0.95, DfB, DfBC)
pB<-pf(F0_B, DfB, DfBC, lower.tail = F)

F0_AB<-MSAB/MSABC #test for "AB" ("bar_level:Vendors")
Fcrit_AB<-qf(0.95, DfAB, DfABC)
pAB<-pf(F0_AB, DfAB, DfABC, lower.tail = F)

F0_BC<-MSBC/MSABC #test for "C/B" ("heat_level%in%Vendors)
Fcrit_BC<-qf(0.95, DfBC, DfABC)
pBC<-pf(F0_BC, DfBC, DfABC, lower.tail = F)

F0_ABC<-MSABC/MSRes #test for "AC/B" (bar_size:heat_level%in%Vendors)
Fcrit_ABC<-qf(0.95, DfABC, DfRes)
pABC<-pf(F0_ABC, DfABC, DfRes, lower.tail = F)


####Problem 6####
###Data Input###
#IL-1 counts per minute
counts<-c(6850, 29100, 34300, 27400, 41100, 47000, 20700, 30700, 33600, 3000,
          4900, 5900, 22100, 25700, 27100, 14300, 17200, 25700, 24300, 33600,
          31400, 2400, 4700, 9400, 6500, 7900, 6400, 4800, 4700, 5700, 2000,
          3000, 3700, 2200, 5100, 6800)
#Treatment
treatment<-factor(c(rep(1, 12), rep(2, 12), rep(3, 12)))
#subject
subject.prb6<-factor(c(rep(1,3), rep(2,3), rep(3,3), rep(4,3), rep(5,3),
                       rep(6,3), rep(7,3), rep(8,3), rep(9,3), rep(10,3),
                       rep(11,3), rep(12,3)))
#Phases of activity
phase<-factor(rep(c("Phase A", "Phase B", "Phase C"), 12))

##Dataframe for Problem 6##
prb6.tibble<-tibble(counts, treatment, subject.prb6, phase)

###Part b)###
#split plot-repeated measures anova with "subjects" nested in time
prb6.aov<-aov(counts~treatment*phase+Error(subject.prb6/(phase)), data = prb6.tibble)

##Hypothesis testing##
#Mean Square Phase
MSPhase<-summary(prb6.aov)[[2]][[1]][['Mean Sq']][1]
#Mean Square Error(2)
MSE2.prb6<-summary(prb6.aov)[[2]][[1]][['Mean Sq']][3]
#Mean Square Phase
DfPhase<-summary(prb6.aov)[[2]][[1]][['Df']][1]
#Mean Square Error(2)
DfE2.prb6<-summary(prb6.aov)[[2]][[1]][['Df']][3]

#F Tests
F0_Phase<-MSPhase/MSE2.prb6
Fcrit_Phase<-qf(0.95, DfPhase, DfE2.prb6)
pPhase<-pf(F0_Phase, DfPhase, DfE2.prb6, lower.tail = F)