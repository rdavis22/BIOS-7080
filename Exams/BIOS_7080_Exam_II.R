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
if(!require(multcomp)){
  install.packages("multcomp"); library(multcomp)}
if(!require(additivityTests)){
  install.packages("additivityTests"); library(additivityTests)}
if(!require(daewr)){
  install.packages("daewr"); library(daewr)}

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
###Data Input###
#response variable
y.prb2<-c(29, 33, 30, 40, 40, 44, 63, 59, 56, 60, 68, 58, 36, 39, 37,
                 48, 52, 52, 60, 65, 59, 73, 61, 68)
#factor A
facA.prb2<-factor(c(rep("A1", 12), rep("A2", 12)))
#factor B
facB.prb2<-factor(c(rep("B1", 6), rep("B2", 6), rep("B1", 6), rep("B2", 6)))
#factor C
facC.prb2<-factor(c(rep("C1", 3), rep("C2", 3), rep("C1", 3), rep("C2", 3),
                    rep("C1", 3), rep("C2", 3), rep("C1", 3), rep("C2", 3)))
#dataframe for the data
prb2.tibble<-tibble(y.prb2, facA.prb2, facB.prb2, facC.prb2)

###Part a)###
##ANOVA##
prb2.aov<-aov(y.prb2~facA.prb2*facB.prb2*facC.prb2, data=prb2.tibble)

###Part b)###
##Cell means##
cellmu.prb2<-c(mean(y.prb2[1:3]), mean(y.prb2[4:6]), mean(y.prb2[7:9]),
               mean(y.prb2[10:12]), mean(y.prb2[13:15]), mean(y.prb2[16:18]),
               mean(y.prb2[19:21]), mean(y.prb2[22:24]))
##marginal means##
#Factor A means
Amu.prb2<-c(mean(y.prb2[facA.prb2=="A1"]), mean(y.prb2[facA.prb2=="A2"]))
#number of levels of factor A
n.A<-length(levels(facA.prb2))
#Factor B means (for a given level of C)
Bmu.prb2<-c(mean(y.prb2[facB.prb2=="B1" & facC.prb2=="C1"]),
            mean(y.prb2[facB.prb2=="B1" & facC.prb2=="C2"]),
            mean(y.prb2[facB.prb2=="B2" & facC.prb2=="C1"]),
            mean(y.prb2[facB.prb2=="B2" & facC.prb2=="C2"]))
#number of levels of factor B
n.B<-length(levels(facB.prb2))
#number of levels of factor C
n.C<-length(levels(facC.prb2))

#total mean
mu.prb2<-mean(y.prb2)
#total number of cells
N.prb2<-length(prb2.tibble$y.prb2)
#number of replications
r.prb2<-length(cellmu.prb2[1]) #from the no. of observations per cell
 
##standard errors##
#MSE of the model
MSE.prb2<-summary(prb2.aov)[[1]][['Mean Sq']][8]
#MS of A
MSA.prb2<-summary(prb2.aov)[[1]][['Mean Sq']][1]
#MS of B
MSB.prb2<-summary(prb2.aov)[[1]][['Mean Sq']][2]
#MS of C
MSC.prb2<-summary(prb2.aov)[[1]][['Mean Sq']][3]
#MS of AB
MSAB.prb2<-summary(prb2.aov)[[1]][['Mean Sq']][4]
#MS of AC
MSAC.prb2<-summary(prb2.aov)[[1]][['Mean Sq']][5]
#MS of BC
MSBC.prb2<-summary(prb2.aov)[[1]][['Mean Sq']][6]
#MS of ABC
MSABC.prb2<-summary(prb2.aov)[[1]][['Mean Sq']][7]
#MSE
MSE.prb2<-summary(prb2.aov)[[1]][['Mean Sq']][8]

##standard errors of cell and marginal means
#*Main Effects#
#standard error of factor A
seA.prb2<-sqrt(MSE.prb2/(r.prb2*n.B*n.C))
#standard error of factor B
seB.prb2<-sqrt(MSE.prb2/(r.prb2*n.A*n.C))
#standard error of factor C
seC.prb2<-sqrt(MSE.prb2/(r.prb2*n.A*n.B))

#*Two factor marginal means#
#Factor A by factor B
seAB.prb2<-sqrt(MSE.prb2/(r.prb2*n.C))
#Factor A by factor C
seAC.prb2<-sqrt(MSE.prb2/(r.prb2*n.B))
#Factor B by factor C
seBC.prb2<-sqrt(MSE.prb2/(r.prb2*n.A))

#*standard error of the cells#
secells.prb2<-sqrt(MSE.prb2/r.prb2)

###part c)###
##example
#pf(summary(prb2.aov)[[1]][['F value']][1], 1, 16, lower.tail =  F)

###Part d)###
#Tukey Test
tuk.prb2<-TukeyHSD(prb2.aov, which = c("facA.prb2", "facB.prb2", "facC.prb2",
                                       "facB.prb2:facC.prb2"))


####Problem 3####
###Data Input###
#response variable
y.prb3<-c(580, 1090, 1392, 568, 1087, 1380, 570, 1085, 1386, 550, 1070, 1328,
          530, 1035, 1312, 579, 1000, 1299, 546, 1045, 867, 575, 1053, 904,
          599, 1066, 889)
#Glass type
glass<-factor(c(rep("1", 9), rep("2", 9), rep("3", 9)))
#Temperature
temperature<-factor(c(rep(c("100", "125", "150"), 9)))
#dataframe for prb3
prb3.tibble<-tibble(y.prb3, glass, temperature)

###Part b)###
#model
prb3.model<-y.prb3~glass*temperature
#linear regression for Levene's Median Test
prb3.lm<-lm(prb3.model, data=prb3.tibble)
#Levene's Median Test
prb3.levene<-leveneTest(prb3.lm, center=median)

###Part c)###
#ANOVA
prb3.aov<-aov(prb3.model, data = prb3.tibble)
#number of replications
r.prb3<-length(temperature[temperature=="100" & glass=="1"])
#MSE
MSE.prb3<-summary(prb3.aov)[[1]][['Mean Sq']][4]

###Part d)###
##Glass type effects for temperature=100
#Glass type 1 vs. 2
gls12t100<-mean(y.prb3[temperature=="100"& glass=="1"])-mean(y.prb3[temperature=="100"& glass=="2"])
#Glass type 1 vs. 3
gls13t100<-mean(y.prb3[temperature=="100"& glass=="1"])-mean(y.prb3[temperature=="100"& glass=="3"])
#Glass type 2 vs. 3
gls23t100<-mean(y.prb3[temperature=="100"& glass=="2"])-mean(y.prb3[temperature=="100"& glass=="3"])

#standard error for multiple contrasts (mu1-mu2)
sc.prb3<-sqrt((MSE.prb3/r.prb3)*(1^2+(-1)^2))
#Bonf. t-value for base effects (alpha/2=0.975, k=3, v=ab(r-1)=18) *from appendix V in Kuehl
tval.prb3<-2.64  

##Simultaneous Confidence Intervals for glass types for temperature=100
SCI95_gls12t100<-c(gls12t100-tval.prb3*sc.prb3, gls12t100+tval.prb3*sc.prb3)
SCI95_gls13t100<-c(gls13t100-tval.prb3*sc.prb3, gls13t100+tval.prb3*sc.prb3)
SCI95_gls23t100<-c(gls23t100-tval.prb3*sc.prb3, gls23t100+tval.prb3*sc.prb3)


####Problem 4####
###Data Input###
#thickness of the aluminum oxide layer (response)
y.prb4<-c(125, 130, 128, 134, 143, 126, 150, 127, 124, 118, 130, 155, 168, 159, 138)
#height position
height<-factor(c(rep("1", 5), rep("2", 5), rep("3", 5)))
#clamp position of the nickel rod
clamp<-factor(c(rep(c("1", "2", "3", "4", "5"), 3)))

#dataframe for problem 4
prb4.tibble<-tibble(y.prb4, height, clamp)

###Parts b) and c)###
##ANOVA##
prb4.aov<-aov(y.prb4~height*clamp, data = prb4.tibble)

###Parts d) and e)###
##Tukey Additivity Test and 1 df SS (daewr package)##
t1df<-Tukey1df(data.frame(prb4.tibble))
##Tukey Additivity Tests (additivtyTests package)##
nickle.mat<-matrix(y.prb4, nrow=3, byrow = T)
tukadd.prb4<-tukey.test(nickle.mat, alpha=0.05)


####Problem 5####
###Part a and b)###
#Etch Rate (total)
etch.rate<-c(550, 669, 604, 650, 633, 642, 601, 635, 1037, 749, 1052, 868, 1075,
             860, 1063, 729)
names(etch.rate)<-c("(1)","a","b","ab","c","ac","bc","abc", "d", "ad", "bd",
                   "abd", "cd", "acd", "bcd", "abcd")
###Factors###
#anode-cathode gap
A<-rep(c(-1, 1), 8)
#Reactor Chamber
B<-rep(c(-1, -1, 1, 1), 4)
#C2F6 gas flow
C<-rep(c(-1, -1, -1, -1, 1, 1, 1, 1), 2)
#Power applied to the cathode
D<-c(rep(-1, 8), rep(1, 8))

#Matrix for the experiment
prb6.mat<-cbind(etch.rate, A, B, C, D)
#number of replications
n.prb5<-1
#number of effects
k.prb5<-4

###Get Estimation of Effects###
##Effect estimates of main effects ("%*%" is matrix multiplication)
Aeff <- (etch.rate %*% A)/(2^(k.prb5-1)*n.prb5)
Beff <- (etch.rate %*% B)/(2^(k.prb5-1)*n.prb5)
Ceff <- (etch.rate %*% C)/(2^(k.prb5-1)*n.prb5)
Deff <- (etch.rate %*% D)/(2^(k.prb5-1)*n.prb5)

##Interaction effects
#Two Factor interactions
AB<-A*B
AC<-A*C
AD<-A*D
BC<-B*C
BD<-B*D
CD<-C*D
#Three Factor interactions
ABC<-A*B*C
ABD<-A*B*D
ACD<-A*C*D
BCD<-B*C*D
#Four Factor interactions
ABCD<-A*B*C*D

#Two Factor Interaction Effects
ABeff<-(etch.rate %*% AB)/(2^(k.prb5-1)*n.prb5)
ACeff<-(etch.rate %*% AC)/(2^(k.prb5-1)*n.prb5)
ADeff<-(etch.rate %*% AD)/(2^(k.prb5-1)*n.prb5)
BCeff<-(etch.rate %*% BC)/(2^(k.prb5-1)*n.prb5)
BDeff<-(etch.rate %*% BD)/(2^(k.prb5-1)*n.prb5)
CDeff<-(etch.rate %*% CD)/(2^(k.prb5-1)*n.prb5)
#Three Factor Interaction Effects
ABCeff<-(etch.rate %*% ABC)/(2^(k.prb5-1)*n.prb5)
ABDeff<-(etch.rate %*% ABD)/(2^(k.prb5-1)*n.prb5)
ACDeff<-(etch.rate %*% ACD)/(2^(k.prb5-1)*n.prb5)
BCDeff<-(etch.rate %*% BCD)/(2^(k.prb5-1)*n.prb5)
#Four Factor Interaction Effects
ABCDeff<-(etch.rate %*% ABCD)/(2^(k.prb5-1)*n.prb5)

##Effects matrix
Effects<-t(etch.rate)%*%cbind(A,B,C,D,AB,AC,AD,BC,BD,CD,ABC,ABD,ACD,BCD,ABCD)/
  (2^(k.prb5-1)*n.prb5)
Summary<-rbind(cbind(A,B,C,D,AB,AC,AD,BC,BD,CD,ABC,ABD,ACD,BCD,ABCD),Effects)
#dimension names
dimnames(Summary)[[1]]<-c(names(etch.rate), "Effect")

###ANOVA###
##turn the main effects into factors
Af<-as.factor(A)
Bf<-as.factor(B)
Cf<-as.factor(C)
Df<-as.factor(D)
options(contrasts=c("contr.sum","contr.poly"))
#run a linear regression
etch.lm<-lm(etch.rate ~ Af*Bf*Cf*Df)
#ANOVa for the linear regression model to get the sum of squares
prb5.anova<-anova(etch.lm)

#% contribution of each SS
perctSS<-c()
#total sum of Squares
totSS<-sum(prb5.anova$`Sum Sq`)
for (i in 1:length(prb5.anova$`Sum Sq`)){
  #take each sum of squares and divide by the total sum of squares
  perctSS[i]<-(prb5.anova$`Sum Sq`[i]/totSS)*100
}

###Part c)###
#3 maximum sum of squares
max3SS<-sort(prb5.anova$`Sum Sq`)[14:16]
#SS of Error
SSError<-sum(prb5.anova$`Sum Sq`)-sum(max3SS)
#DF for Error
DFError<-(length(prb5.anova$`Sum Sq`)-length(max3SS)-1)
#MSE of model (length of prb5.anova$'Sum Sq'-# of terms (3)-1)
MSE<-SSError/DFError

##F-value for each term
#F value for A
FA.prb5<-max3SS[1]/MSE
#F critical value for A
FcritA.prb5<-qf(0.95, 1, DFError)
#pvalue for A
pA.prb5<-pf(FA.prb5, 1, DFError, lower.tail = F)
#F value for D
FD.prb5<-max3SS[3]/MSE
#F critical value for D
FcritD.prb5<-qf(0.95, 1, DFError)
#pvalue for D
pD.prb5<-pf(FD.prb5, 1, DFError, lower.tail = F)
#F value for AD
FAD.prb5<-max3SS[2]/MSE
#F critical value for AD
FcritAD.prb5<-qf(0.95, 1, DFError)
#pvalue for AD
pAD.prb5<-pf(FAD.prb5, 1, DFError, lower.tail = F)
