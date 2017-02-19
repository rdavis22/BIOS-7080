####BIOS 7080: Exam I####
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(car))
  install.packages("car")
if(!require(multcomp))
  install.packages("multcomp")
if(!require(agricolae))
  install.packages("agricolae")
if(!require(MASS))
  install.packages("MASS")
if(!require(lmtest))
  install.packages("lmtest")
if(!require(pwr))
  install.packages("pwr")

####Q1####
###Data Input###
#caffeine dosage (mL)
treat.prb1<-factor(c(rep(0, 10), rep(100, 10), rep(200, 10), rep(300, 10)))
#number of finger taps after two hours
finger_taps.prb1<-c(242, 245, 244, 248, 247, 248, 242, 244, 246, 242, 248, 246,
                    245, 247, 248, 250, 247, 246, 243, 244, 246, 248, 250, 252,
                    248, 250, 252, 248, 245, 250, 248, 250, 251, 251, 248, 251,
                    252, 249, 253, 251)
#combine the data into a tibble
prb1.tibble<-tibble(treat.prb1, finger_taps.prb1)

###Computation###
##ANOVA##
prb1.anva<-aov(finger_taps.prb1~treat.prb1, data = prb1.tibble)
#caputre the MSE of the ANOVA
MSE.prb1<- summary(prb1.anva)[[1]][['Mean Sq']][[2]]

##b) assess model adequecy, fit, error normality, and error homoscedasticity
##1b-i) model fit##
#calculate residuals
resids.prb1<-residuals(prb1.anva)
#standardized residuals
stdrdresids.prb1<-stdres(prb1.anva) #*sum to zero for each level of "treat.prb1"
#plot of standardized residuals vs. Caffeine dosage
resplot.prb1<-ggplot()+
  geom_point(aes(x=treat.prb1, y=stdrdresids.prb1))+
  labs(x="Treatments (caffeine dosage /mL)", y="Standardized Residuals",
       title="Standardized Residuals vs. Treamtent (caffeine dosage /mL)")

##1b-ii) normality of errors##
#Quantile-quantile plot
qq.prb1<-ggqqline(resids.prb1)+
  labs(x="Quantile of Standard Normal", y="Residuals", title="QQPlot of Residuals")

##1b-iii) homogeneous variances assumption##

#S-L plot
slp.prb1<-spreadLevelPlot(prb1.anva, main="S-L Plot: Caffeine intake")

#Levene Test of medians
lvne_test.prb1<-leveneTest(prb1.anva, center="median")

##e) ("c" and "d" were already answered)
#random assignment of treatment to response
rnd_resp.prb1<-sample(finger_taps.prb1)
rnd_trt.prb1<-sample(treat.prb1)


####Q2####
###2a) BUild the Contrasts###
#contrast for part a1) (mu0+mu300)-(m100+mu200)
C_2a1<-mean(finger_taps.prb1[treat.prb1=="0"])+mean(finger_taps.prb1[treat.prb1=="300"])-
  mean(finger_taps.prb1[treat.prb1=="100"])-mean(finger_taps.prb1[treat.prb1=="200"])

##standard error of contrasts
#number of repeats in each group
r_2a1<-length(finger_taps.prb1[treat.prb1=="0"])
#MSE from ANOVA
#last part is the sum of squares of the "k" values of the contrast
se_2a1<-sqrt(MSE.prb1/r_2a1*(1^2+1^2+(-1)^2+(-1)^2))

#95% SCI (t0.025,3,36=2.52 from Appendix V in Kuehl)
SCI95_2a1<-c(C_2a1-se_2a1*2.52, C_2a1+se_2a1*2.52)

#contrast for part a2)
C_2a2<-mean(finger_taps.prb1[treat.prb1=="0"])-mean(finger_taps.prb1[treat.prb1=="100"])
#last part is the sum of squares of the "k" values of the contrast
se_2a2<-sqrt(MSE.prb1/r_2a1*(1^2+1^2))

#95% SCI (t0.025,3,36=2.52 from Appendix V in Kuehl)
SCI95_2a2<-c(C_2a2-se_2a2*2.52, C_2a2+se_2a2*2.52)

###2b) Scheffe test for part 2a)###
#get the scheffe statistic for 2a1 (se*sqrt((t-1)*F_alpha(E),(t-1),v))
Schef_stat_2a1<-se_2a1*sqrt(3*qf(.95, 3, 36)) #3.9499>abs(-0.1)
#get the scheffe statistic for 2a2
Schef_stat_2a2<-se_2a2*sqrt(3*qf(.95, 3, 36)) #2.7930>abs(-1.6)

#overall Scheffe Statistic
Schf_stat.prb2<-scheffe.test(y=prb1.anva, trt="treat.prb1", DFerror=36,
                        MSerror=MSE_2a1, Fc=qf(0.95, 3, 36), alpha=0.05)

###2c) Dunnett's Comparison of All Treatments with a Control###
#Dunnett Statistic (alpha=0.05, k=3, v=36)
d.prb2<-2.45 #estimate from table VI (two-sided)
#Dunnett criterion
Dun.prb2<-d.prb2*sqrt(2*MSE.prb1/r_2a1)

#Mean amount of finger tapping (0mL caffeine is the control)
mu_control<-mean(finger_taps.prb1[treat.prb1=="0"])
mu_100<-mean(finger_taps.prb1[treat.prb1=="100"])
mu_200<-mean(finger_taps.prb1[treat.prb1=="200"])
mu_300<-mean(finger_taps.prb1[treat.prb1=="300"])

#Difference of control (0mL caffeine) and other means
D100.prb2<-mu_100-mu_control
D200.prb2<-mu_200-mu_control
D300.prb2<-mu_300-mu_control

#95% SCI for problem 2
SCI95_100<-c(D100.prb2-Dun.prb2, D100.prb2+Dun.prb2)
SCI95_200<-c(D200.prb2-Dun.prb2, D200.prb2+Dun.prb2)
SCI95_300<-c(D300.prb2-Dun.prb2, D300.prb2+Dun.prb2)

#***alternative method with t-tests
#summary(glht(prb1.anva, linfct=mcp(treat.prb1="Dunnett")))

###2d) Multiple Comparisons with Best Mean###
#Dunnett from Appendix VI (one-sided, alpha=0.05, k=3, v=36) in Kuehl
d.prb2d<-2.14
#M value for confidence interval with D
M.prb2d<-d.prb2d*sqrt(2*MSE.prb1/r_2a1)

#Differences from 300 mL caffeine (highest mean freq of finger tap)
D0.prb2d<-mu_control-mu_300
D100.prb2d<-mu_100-mu_300
D200.prb2d<-mu_200-mu_300
D300.prb2d<-mu_300-mu_200

#95% CI's
CI95_D0<-c(D0.prb2d-M.prb2d, D0.prb2d+M.prb2d)
CI95_D100<-c(D100.prb2d-M.prb2d, D100.prb2d+M.prb2d)
CI95_D200<-c(D200.prb2d-M.prb2d, D200.prb2d+M.prb2d)
CI95_D300<-c(D300.prb2d-M.prb2d, D300.prb2d+M.prb2d)

#95% SCI's
SCI95_D0<-c(D0.prb2d-M.prb2d, 0)
SCI95_D100<-c(D100.prb2d-M.prb2d, 0)
SCI95_D200<-c(D200.prb2d-M.prb2d, D200.prb2d+M.prb2d)
SCI95_D300<-c(D300.prb2d-M.prb2d, D300.prb2d+M.prb2d)


####Q3####
##3a) Power Analysis for 5 means##
#vector of means
mu_vec<-c(11, 12, 15, 18, 19)
#grand mean of all means
mu_grnd<-mean(mu_vec)
#number of groups
t.prb3<-length(mu_vec)
#MSE
MSE.prb3<-9
#significance level
alpha.prb3<-0.01

#squares of differences between means and grand mean
taui_2<-(mu_vec-rep(mu_grnd, 5))^2

#sum of squares
sigma_tau2<-sum(taui_2)

#phi^2/r #Now check table IX in Kuehl appendex (v1=(t-1)=4, v2=t(r-1, alpha=0.01))
phi2_r<-sigma_tau2/(t.prb3*MSE.prb3)

#check power analysis in R (f=sqrt(5/25*taui_2/MSE.prb3))
#this permutation assumes r(i.e. "n_i")=5 and N=25 (because 5*5=25 for even numbers of subjects per group)
f1.prb3<-sqrt(5/25*sigma_tau2/MSE.prb3)
#power with r=5
powr.prb3<-pwr.anova.test(k=t.prb3, n=5, f=f1.prb3, sig.level=alpha.prb3) #power.prb3=0.8816 (need 0.90)

f2.prb3<-sqrt(6/30*sigma_tau2/MSE.prb3)
#power with r=6
powr.prb3<-pwr.anova.test(k=t.prb3, n=6, f=f1.prb3, sig.level=alpha.prb3) #power.prb3=0.9596 (stop here)


####Q4####
###Data Input###
#concentration in a three-component liquid mixture
concentration.prb4<-c(58.2, 57.2, 58.4, 55.8, 54.9, 56.3, 54.5, 57.0, 55.3,
                      50.1, 54.2, 55.4, 52.9, 49.9, 50.0, 51.7)

#catalyst type
catalyst.prb4<-factor(c(rep(1, 5), rep(2, 4), rep(3, 3), rep(4, 4)))

#create data frame
prb4.tibble<-tibble(concentration.prb4, catalyst.prb4)

###4a) 95% CI for mean###
#ANOVA
prb4.anva<-aov(concentration.prb4~catalyst.prb4, data=prb4.tibble)

#mean of concentration for catalyst "1"
mu_1.prb4<-mean(concentration.prb4[catalyst.prb4=="1"])
#overall size of sample
N.prb4<-length(concentration.prb4)
#n for conc. for catalyst 1
nconc1.prb4<-length(concentration.prb4[catalyst.prb4=="1"])
#MSE (sigma^2)
MSE.prb4<-summary(prb4.anva)[[1]][['Mean Sq']][[2]]
#standard error of mean conc. for catalyst=="1"
se1.prb4<-sqrt(MSE.prb4/nconc1.prb4)
#t-value for the 95% CI
t_95.prb4<-qt(0.975, N.prb4-length(levels(catalyst.prb4)))

#95% CI for catalyst=1
CI95_1.prb4<-c(mu_1.prb4-t_95.prb4*se1.prb4, mu_1.prb4+t_95.prb4*se1.prb4)

###4b) 95% CI for the other means###
#means for the other catalyst groups
mu_2.prb4<-mean(concentration.prb4[catalyst.prb4=="2"])
mu_3.prb4<-mean(concentration.prb4[catalyst.prb4=="3"])
mu_4.prb4<-mean(concentration.prb4[catalyst.prb4=="4"])

#number of in each catalyst group
nconc2.prb4<-length(concentration.prb4[catalyst.prb4=="2"])
nconc3.prb4<-length(concentration.prb4[catalyst.prb4=="3"])
nconc4.prb4<-length(concentration.prb4[catalyst.prb4=="4"])

#each pairwise difference in mean
mu21.prb4<-mu_2.prb4-mu_1.prb4
mu31.prb4<-mu_3.prb4-mu_1.prb4
mu41.prb4<-mu_4.prb4-mu_1.prb4
mu32.prb4<-mu_3.prb4-mu_2.prb4
mu42.prb4<-mu_4.prb4-mu_2.prb4
mu43.prb4<-mu_4.prb4-mu_3.prb4

#standard errors for each pairwise comparion group
se21.prb4<-sqrt(MSE.prb4*(1/nconc2.prb4+1/nconc1.prb4))
se31.prb4<-sqrt(MSE.prb4*(1/nconc3.prb4+1/nconc1.prb4))
se41.prb4<-sqrt(MSE.prb4*(1/nconc4.prb4+1/nconc1.prb4))
se32.prb4<-sqrt(MSE.prb4*(1/nconc3.prb4+1/nconc2.prb4))
se42.prb4<-sqrt(MSE.prb4*(1/nconc4.prb4+1/nconc2.prb4))
se43.prb4<-sqrt(MSE.prb4*(1/nconc4.prb4+1/nconc3.prb4))

#95% CI's for the catalyst groups 2, 3, and 4
CI95_21.prb4<-c(mu21.prb4-t_95.prb4*se21.prb4, mu21.prb4+t_95.prb4*se21.prb4)
CI95_31.prb4<-c(mu31.prb4-t_95.prb4*se31.prb4, mu31.prb4+t_95.prb4*se31.prb4)
CI95_41.prb4<-c(mu41.prb4-t_95.prb4*se41.prb4, mu41.prb4+t_95.prb4*se41.prb4)
CI95_32.prb4<-c(mu32.prb4-t_95.prb4*se32.prb4, mu32.prb4+t_95.prb4*se32.prb4)
CI95_42.prb4<-c(mu42.prb4-t_95.prb4*se42.prb4, mu42.prb4+t_95.prb4*se42.prb4)
CI95_43.prb4<-c(mu43.prb4-t_95.prb4*se43.prb4, mu43.prb4+t_95.prb4*se43.prb4)

###4c-e)Tukey Test###
#gets the 95% SCI
tuky.prb4<-TukeyHSD(prb4.anva)
#plot of Tukey test
p.prb4<-(TukeyHSD(prb4.anva, "catalyst.prb4"))

#An alternative method
tuky_alt.prb4<-glht(prb4.anva, linfct=mcp(catalyst.prb4="Tukey"))