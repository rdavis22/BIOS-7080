####BIOS 7080: Hw3 (Treatment Comparisons)####
###Load packages###
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(car))
  install.packages("car")
if(!require(multcomp))
  install.packages("multcomp")

####Data Input####
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

####Problem 3.1####
#ANOVA for the data
prb1.anva<-aov(time~traffic_sig, data=prb1.tibble)

##1a-i)
#Contrast between pretimed and average of semi-and fully actuatued signals
C_i<-mean(time[traffic_sig=="Pretimed"])-0.5*(mean(time[traffic_sig=="Semi-actuated"])+
                                               mean(time[traffic_sig=="Fully-actuated"]))
#standard error of C_i
#***number of replications are the same for all treatments in this case
r_i<-length(time[traffic_sig=="Pretimed"])
se_i<-sqrt(summary(prb1.anva)[[1]][["Mean Sq"]][2]/r_i*(1^2+(-0.5)^2+(-0.5)^2))

##1a-ii)  
#Contrast between semi-and fully actuatued signals
C_ii<-mean(time[traffic_sig=="Semi-actuated"])-mean(time[traffic_sig=="Fully-actuated"])

#standard error of C_ii (length(time[...]=r_i)*all the r_i are equal)
se_ii<-sqrt(summary(prb1.anva)[[1]][["Mean Sq"]][2]/r_i*(1^2+(-1)^2))

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
MSE<-summary(prb1.anva)[[1]][["Mean Sq"]][2]
#d(alpha=0.05, k=2, v=12, one-sided)=2.11
d<-2.11
#get the 'M' value for MCB procedure
M<-d*sqrt(2*MSE/r_i)

#95%CI
CI95_pre<-c(0, D_1+M)
CI95_semi<-c(0, )