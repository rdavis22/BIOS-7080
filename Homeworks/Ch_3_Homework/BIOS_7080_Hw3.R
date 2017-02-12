####BIOS 7080: Hw3 (Treatment Comparisons)####
###Load packages###
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(car))
  install.packages("car")
if(!require(multcomp))
  install.packages("multcomp")
if(!require(agricolae))
  install.packages("agricolae")

####Problem 3.1####
###Data Input###
#type of traffic signal
traffic_sig<-c("Pretimed", "Pretimed", "Pretimed", "Pretimed", "Pretimed",
               "Semi-actuated", "Semi-actuated", "Semi-actuated", "Semi-actuated",
               "Semi-actuated", "Fully-actuated", "Fully-actuated", "Fully-actuated",
               "Fully-actuated", "Fully-actuated")

#make traffic_sig a categorical factor
traffic_sig<-factor(traffic_sig, levels = c("Pretimed", "Semi-actuated",
                                            "Fully-actuated"))
#average amount of time stopped
time<-c(36.6, 39.2, 30.4, 37.1, 34.1, 17.5, 20.6, 18.7, 25.7, 22.0, 15.0, 10.4,
        18.9, 10.5, 15.2)
#dataframe for the data
prb1.tibble<-tibble(traffic_sig, time)
#ANOVA for the data
prb1.anva<-aov(time~traffic_sig, data=prb1.tibble)

##1a-i)
#Contrast between pretimed and average of semi-and fully actuatued signals
C_i<-mean(time[traffic_sig=="Pretimed"])-0.5*(mean(time[traffic_sig=="Semi-actuated"])+
                                               mean(time[traffic_sig=="Fully-actuated"]))
#standard error of C_i
#***number of replications are the same for all treatments in this case
r_i<-length(time[traffic_sig=="Pretimed"])
MSE_i<-summary(prb1.anva)[[1]][["Mean Sq"]][2]
se_i<-sqrt(MSE_i/r_i*(1^2+(-0.5)^2+(-0.5)^2))

##1a-ii)  
#Contrast between semi-and fully actuatued signals
C_ii<-mean(time[traffic_sig=="Semi-actuated"])-mean(time[traffic_sig=="Fully-actuated"])

#standard error of C_ii (length(time[...]=r_i)*all the r_i are equal)
se_ii<-sqrt(MSE_i/r_i*(1^2+(-1)^2))

##1b)
#sum of squares of the contrasts
SSC_i<-r_i*C_i^2/(sum(1^2+(-0.5)^2+(-0.5)^2))
SSC_ii<-r_i*C_ii^2/(sum(1^2+(1)^2))

##1c)
#T-test for C_i
t_ci<-C_i/se_i #9.713>t #9.71
tc_crit<-abs(qt(0.025, 12)) #2.18 (9.71>2.18)

#T-test for C_ii
t_cii<-C_ii/se_ii #3.22>2.18

##1d)
#F-Test for C_i
f_ci<-SSC_i/summary(prb1.anva)[[1]][["Mean Sq"]][2] #94.34
fc_crit<-abs(qf(0.95, 1, 12)) #4.75

#F-test for C_ii
f_cii<-SSC_ii/summary(prb1.anva)[[1]][["Mean Sq"]][2] #94.34


####Problem 3.4####
###Means for the three groups
mu_pre<-mean(time[traffic_sig=="Pretimed"])
mu_semi<-mean(time[traffic_sig=="Semi-actuated"])
mu_fully<-mean(time[traffic_sig=="Fully-actuated"])

##1A) MCB##
#The "fully-actuated" mean is the reference mean
D_1<-mu_pre-mu_fully
D_2<-mu_semi-mu_fully
D_3<-mu_fully-mu_semi

#MSE for ANOVA table
MSE.prb1<-summary(prb1.anva)[[1]][["Mean Sq"]][2]
#d(alpha=0.05, k=2, v=12, one-sided)=2.11
d.prb1<-2.11
#get the 'M' value for MCB procedure
M.prb1<-d.prb1*sqrt(2*MSE.prb1/r_i)

#95%CI for treatment means
CI95_pre<-c(D_1-M.prb1, D_1+M.prb1)
CI95_semi<-c(D_2-M.prb1, D_2+M.prb1)
CI95_fully<-c(D_3-M.prb1, D_3+M.prb1)

#95% Simultaneous CI for treatment means
SCI95_pre<-c(0, D_1+M.prb1)
SCI95_semi<-c(0, D_2+M.prb1)
SCI95_fully<-c(D_3-M.prb1, 0)

####Problem 3.5####
###Data Input
#patient
patient<-c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6, 7, 7, 8, 8)
#cholesterol level (mg/dL)
cholesterol<-c(167.3, 166.7, 186.7, 184.2, 100.0, 107.9, 214.5, 215.3, 148.5,
               149.5, 171.5, 167.3, 161.5, 159.4, 243.6, 245.5)
prb5.tibble<-tibble(patient, cholesterol)

#ANOVA
prb5.anva<-aov(cholesterol~factor(patient), data=prb5.tibble)

##mean of cholesterol per patient (turn into a loop later)
mu_pt1<-mean(cholesterol[patient==1])
mu_pt2<-mean(cholesterol[patient==2])
mu_pt3<-mean(cholesterol[patient==3])
mu_pt4<-mean(cholesterol[patient==4])
mu_pt5<-mean(cholesterol[patient==5])
mu_pt6<-mean(cholesterol[patient==6])
mu_pt7<-mean(cholesterol[patient==7])
mu_pt8<-mean(cholesterol[patient==8])

#M value
MSE.prb5<-summary(prb5.anva)[[1]][['Mean Sq']][2]
#d value
d.prb5<-2.81 #from Appendix VI (one-sided, alpha=0.05) in Kuehl
#number of replications
r.prb5<-length(cholesterol[patient==1])
M.prb5<-d.prb5*sqrt(2*MSE.prb5/r.prb5)

#D values
D_pt1<-mu_pt8-mu_pt1
D_pt2<-mu_pt8-mu_pt2
D_pt3<-mu_pt8-mu_pt3
D_pt4<-mu_pt8-mu_pt4
D_pt5<-mu_pt8-mu_pt5
D_pt6<-mu_pt8-mu_pt6
D_pt7<-mu_pt8-mu_pt7
D_pt8<-mu_pt4-mu_pt8

#95%CI
CI95_pt1<-c(D_pt1-M.prb5, D_pt1+M.prb5)
CI95_pt2<-c(D_pt2-M.prb5, D_pt2+M.prb5)
CI95_pt3<-c(D_pt3-M.prb5, D_pt3+M.prb5)
CI95_pt4<-c(D_pt4-M.prb5, D_pt4+M.prb5)
CI95_pt5<-c(D_pt5-M.prb5, D_pt5+M.prb5)
CI95_pt6<-c(D_pt6-M.prb5, D_pt6+M.prb5)
CI95_pt7<-c(D_pt7-M.prb5, D_pt7+M.prb5)
CI95_pt8<-c(D_pt8-M.prb5, D_pt8+M.prb5)

#Simultaneous Confidence interval
SCI95_pt1<-c(0, D_pt1+M.prb5)
SCI95_pt2<-c(0, D_pt2+M.prb5)
SCI95_pt3<-c(0, D_pt3+M.prb5)
SCI95_pt4<-c(0, D_pt4+M.prb5)
SCI95_pt5<-c(0, D_pt5+M.prb5)
SCI95_pt6<-c(0, D_pt6+M.prb5)
SCI95_pt7<-c(0, D_pt7+M.prb5)
SCI95_pt8<-c(D_pt8-M.prb5, 0)


####Problem 3.7####
###Data Intput
##Tissue Growth (mmX10)
#culture
culture<-c("control", "control", "control", "control", "control", "glucose",
           "glucose", "glucose", "glucose", "glucose", "fructose", "fructose",
           "fructose", "fructose", "fructose", "sucrose", "sucrose", "sucrose",
           "sucrose", "sucrose")
#Turn culture into class factor
culture<-factor(culture, levels = c("control", "glucose","fructose", "sucrose"))

growth<-c(45, 39, 40, 45, 42, 25, 28, 30, 29, 33, 28, 31, 24, 28, 27, 31, 37,
          35, 33, 34)

prb7.tibble<-tibble(culture, growth)

##ANOVA##
prb7.anva<-aov(growth~culture, data=prb7.tibble)

##Dunnett's Test
#Dunnett Statistic (alpha=0.05, k=3, v=16)
d.prb7<-2.59
#MSE
MSE.prb7<-summary(prb7.anva)[[1]][["Mean Sq"]][2]
#replications
r.prb7<-length(culture[culture=="control"])
#Dunnett criterion
Dun.prb7<-d.prb7*sqrt(2*MSE.prb7/r.prb7)

#Mean Growth of different culture Media
mu_control<-mean(growth[culture=="control"])
mu_glucose<-mean(growth[culture=="glucose"])
mu_fructose<-mean(growth[culture=="fructose"])
mu_sucrose<-mean(growth[culture=="sucrose"])

#Difference of control and other means
Dgluc.prb7<-mu_glucose-mu_control
Dfruc.prb7<-mu_fructose-mu_control
Dsucr.prb7<-mu_sucrose-mu_control

#95% SCI for problem 7
CI95_gluc<-c(Dgluc.prb7-Dun.prb7, Dgluc.prb7+Dun.prb7)
CI95_fruc<-c(Dfruc.prb7-Dun.prb7, Dfruc.prb7+Dun.prb7)
CI95_sucr<-c(Dsucr.prb7-Dun.prb7, Dsucr.prb7+Dun.prb7)

#***Alternative method (no SCI, though)
#glht(prb7.anva, linfct=mcp(culture="Dunnett"))


####Problem 3.8####
###A)###
#Contrasts
C1<-c(1, 3, -1, -1, -1) #sum(C1)=1-->Not a Contrast
C2<-c(1, -1, 0, -1, 1) #sum(C2)=0-->A Contrast
C3<-c(-1, 1, -1, 1, -1) #sum(C3)=-1-->Not a Contrast
C4<-c(0, 0, 2, -1, -1) #sum(C4)=0-->A Contrast

###B)###
#test for orthogonality
orthog<-sum(C1*C2) #= -2; therefore, these two sets of proposed contrasts are not orthogonal


####Problem 3.9####
##Scheffe test for contrasts in 3.1-- manual##
#number of treatments
#scheffe statistic
Schef_stat_i<-se_i*sqrt(2*qf(.95, 2, 12))
Schef_stat_ii<-se_ii*sqrt(2*qf(.95, 2, 12))

#95% CI for both C_i and C_ii contrasts
CI95_schf_i<-c(C_i-Schef_stat_i, C_i+Schef_stat_i)
CI95_schf_ii<-c(C_ii-Schef_stat_ii, C_ii+Schef_stat_ii)

#alternative (only gives scheffe statistic for contrast with all trt. groups)
Schf_stat<-scheffe.test(y=prb1.anva, trt="traffic_sig", DFerror=12,
                        MSerror=MSE_i, Fc=qf(0.95, 2, 12), alpha=0.05)


####Problem 3.10####
t_crit.prb10<-2.56 #from Bonferroni t-table (t(alpha=0.05, k=2, v=12)
#since the above is less than both t_ci and t_cii, we reject the null...
#hypothesis that the contrasts are 0

#Alternative method in R
bonf_test.prb10<-pairwise.t.test(time, traffic_sig, p.adjust.method = "bonf")