####BIOS 7080: Hw2####
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(pwr))
  install.packages("pwr")

####Problem 1####
#type of traffic signal
traffic_sig<-c("Pretimed", "Pretimed", "Pretimed", "Pretimed", "Pretimed",
               "Semi-actuated", "Semi-actuated", "Semi-actuated", "Semi-actuated",
               "Semi-actuated", "Fully-actuated", "Fully-actuated", "Fully-actuated",
               "Fully-actuated", "Fully-actuated")
#average amount of time stopped
time<-c(36.6, 39.2, 30.4, 37.1, 34.1, 17.5, 20.6, 18.7, 25.7, 22.0, 15.0, 10.4,
        18.9, 10.5, 15.2)

prb1.tibble<-tibble(traffic_sig, time)

#part c) ANOVA for the traffic data
prb1.anva<-aov(time~traffic_sig, data=prb1.tibble)

#part d&e) least square means, std, and 95% CI of each traffic stop type
x_pre<-mean(time[1:5])
std_pre<-summary(prb1.anova)[[1]][["Mean Sq"]][2] #MSE of residuls    #sd(time[1:5])
#*note, df=N-t for getting the t distribution #use MSE of residual from ANOVA table
CI_pre<-c(x_pre-std_pre*qt(0.975, df=12), x_pre+std_pre*qt(0.975, df=12))
x_semi<-mean(time[6:10])
std_semi<-summary(prb1.anva)[[1]][["Mean Sq"]][2]#sd(time[6:10])
CI_semi<-c(x_semi-std_semi*qt(0.975, df=12), x_semi+std_semi*qt(0.975, df=12))
x_fully<-mean(time[11:15])
std_fully<-summary(prb1.anva)[[1]][["Mean Sq"]][2]#sd(time[11:15])
CI_fully<-c(x_fully-std_fully*qt(0.975, df=12), x_fully+std_fully*qt(0.975, df=12))

####Problem 2####
#data wrangle
treat.prb2<-c(0,0,0,0,50,50,50, 50, 100, 100, 100, 100, 150, 150, 150, 150,
              200, 200, 200, 200)
treat.prb2<-factor(treat.prb2)
lettuce<-c(104, 114, 90, 140, 134, 130, 144, 174, 146, 142, 152, 156, 147, 
           160, 160, 163, 131, 148, 154, 163)
prb2.tibble<-tibble(treat.prb2, lettuce)

#Part C) ANOVA
prb2.anva<-aov(lettuce~treat.prb2, data=prb2.tibble)

#part d&e) least square means, std, and 95% CI of each lettuce treat group
#group trt=0
x_0<-mean(lettuce[1:4])
#MSE of residuals
std_0<-summary(prb2.anva)[[1]][["Mean Sq"]][2] #sd(lettuce[1:4])
CI_0<-c(x_0-std_0*qt(0.975, df=15), x_0+std_0*qt(0.975, df=15))
#group trt=50
x_50<-mean(lettuce[5:8])
std_50<-summary(prb2.anva)[[1]][["Mean Sq"]][2] #sd(lettuce[5:8])
CI_50<-c(x_50-std_50*qt(0.975, df=15), x_50+std_50*qt(0.975, df=15))
#group trt=100
x_100<-mean(lettuce[9:12])
std_100<-summary(prb2.anva)[[1]][["Mean Sq"]][2] #sd(lettuce[9:12])
CI_100<-c(x_100-std_100*qt(0.975, df=15), x_100+std_100*qt(0.975, df=15))
#group trt=50
x_150<-mean(lettuce[13:16])
std_150<-summary(prb2.anva)[[1]][["Mean Sq"]][2]#sd(lettuce[13:16])
CI_150<-c(x_150-std_150*qt(0.975, df=15), x_150+std_150*qt(0.975, df=15))
#group trt=50
x_200<-mean(lettuce[17:20])
std_200<-summary(prb2.anva)[[1]][["Mean Sq"]][2] #sd(lettuce[17:20])
CI_200<-c(x_200-std_200*qt(0.975, df=15), x_200+std_200*qt(0.975, df=15))

#part g) randomization of numbers 1-20
rndm_samp.prb2<-sample(treat.prb2, 20)


####Problem 3####
#data wrangle
treat.prb3<-c("Premolt","Premolt","Premolt","Premolt","Premolt","Fasting",
              "Fasting","Fasting","Fasting","Fasting", "60g bran","60g bran",
              "60g bran","60g bran","60g bran","80g bran","80g bran","80g bran",
              "80g bran","80g bran","Laying mash","Laying mash","Laying mash",
              "Laying mash","Laying mash")
treat.prb3<-factor(treat.prb3)
serum_t3<-c(94.09, 90.45, 99.38, 73.56, 74.39, 98.81, 103.55, 115.23, 129.06, 117.61,
            197.18, 207.31, 177.50, 226.05, 222.74, 102.93, 117.51, 119.92, 112.01, 101.10,
            83.14, 89.59, 87.76, 96.43, 82.94)
prb3.tibble<-tibble(treat.prb3, serum_t3)

#Part C) ANOVA
prb3.anva<-aov(serum_t3~treat.prb3, data=prb3.tibble)

#part d&e) least square means, std, and 95% CI of each lettuce treat group
#group trt=Premolt
x_Premolt<-mean(serum_t3[1:5])
#MSE of residuals
std_Premolt<-summary(prb3.anva)[[1]][["Mean Sq"]][2] #sd(serum_t3[1:5])
CI_Premolt<-c(x_Premolt-std_Premolt*qt(0.975, df=20), x_Premolt+std_Premolt*qt(0.975, df=20))
#group trt=Fasting
x_Fast<-mean(serum_t3[6:10])
std_Fast<-summary(prb3.anva)[[1]][["Mean Sq"]][2] #sd(serum_t3[6:10])
CI_Fast<-c(x_Fast-std_Fast*qt(0.975, df=20), x_Fast+std_Fast*qt(0.975, df=20))
#group trt=60 gram bran
x_60<-mean(serum_t3[11:15])
std_60<-summary(prb3.anva)[[1]][["Mean Sq"]][2] #sd(serum_t3[11:15])
CI_60<-c(x_60-std_60*qt(0.975, df=20), x_60+std_60*qt(0.975, df=20))
#group trt=80 gram bran
x_80<-mean(serum_t3[16:20])
std_80<-summary(prb3.anva)[[1]][["Mean Sq"]][2] #sd(serum_t3[16:20])
CI_80<-c(x_80-std_80*qt(0.975, df=20), x_80+std_80*qt(0.975, df=20))
#group trt=Laying mash
x_LM<-mean(serum_t3[21:25])
std_LM<-summary(prb3.anva)[[1]][["Mean Sq"]][2] #sd(serum_t3[21:25])
CI_LM<-c(x_LM-std_LM*qt(0.975, df=20), x_LM+std_LM*qt(0.975, df=20))

#part g) randomization of numbers 1-20
rndm_samp.prb3<-sample(treat.prb3, 25)


####Problem 4####
#data wrangle
treat.prb4<-c("Control 1978", "Control 1978", "Control 1978", "Control 1978",
              "Control 1978", "Control 1978", "1979", "1979", "1979", "1979",
              "1979", "1979", "1979", "1979", "1979", "1980", "1980", "1980",
              "1980", "1980", "1980", "1980", "1980", "1980", "1981", "1981",
              "1981", "1981", "1981", "1981", "1981", "1981", "1981", "1981")
treat.prb4<-factor(treat.prb4)
#number of different strategies used
num_strat<-c(6.88, 5.40, 16.00, 9.80, 7.63, 5.00, 7.25, 10.50, 8.43, 8.63, 8.63,
            7.00, 11.13, 7.25, 10.38, 10.85, 7.43, 6.71, 7.60, 7.60, 5.57, 8.71,
            5.86, 7.20, 7.29, 14.38, 6.00, 5.00, 5.38, 14.14, 9.25, 5.71, 7.35, 10.75)
prb4.tibble<-tibble(treat.prb4, num_strat)

#Part C) ANOVA
prb4.anva<-aov(num_strat~treat.prb4, data=prb4.tibble)

#part d&e) least square means, std, and 95% CI of each lettuce treat group
#group trt=Control
x_ctrl<-mean(num_strat[1:6])
#MSE of residuals
std_ctrl<-summary(prb4.anva)[[1]][["Mean Sq"]][2] #sd(num_strat[1:6])
CI_ctrl<-c(x_ctrl-std_ctrl*qt(0.975, df=30), x_ctrl+std_ctrl*qt(0.975, df=30))
#group trt=1979
x_1979<-mean(num_strat[7:15])
std_1979<-summary(prb4.anva)[[1]][["Mean Sq"]][2]#sd(num_strat[7:15])
CI_1979<-c(x_1979-std_1979*qt(0.975, df=30), x_1979+std_1979*qt(0.975, df=30))
#group trt=1980
x_1980<-mean(num_strat[16:24])
std_1980<-summary(prb4.anva)[[1]][["Mean Sq"]][2]#sd(num_strat[16:24])
CI_1980<-c(x_1980-std_1980*qt(0.975, df=30), x_1980+std_1980*qt(0.975, df=30))
#group trt=1981
x_1981<-mean(num_strat[25:34])
std_1981<-summary(prb4.anva)[[1]][["Mean Sq"]][2]#sd(num_strat[25:34])
CI_1981<-c(x_1981-std_1981*qt(0.975, df=30), x_1981+std_1981*qt(0.975, df=30))

#part g) randomization of numbers 1-20
rndm_samp.prb4<-sample(treat.prb4, 34)


####Problem 5####
#data wrangle
#level of copper used
treat.prb5<-c(0.00, 0.00, 0.00, 0.00, 0.05, 0.05, 0.10, 0.10, 0.20, 0.20, 0.50,
              0.50)
treat.prb5<-factor(treat.prb5)
#spectroscopy data
spect<-c(0.045, 0.047, 0.051, 0.054, 0.084, 0.087, 0.115, 0.116, 0.183,
             0.191, 0.395, 0.399)
prb5.tibble<-tibble(treat.prb5, spect)

#Part C) ANOVA
prb5.anva<-aov(spect~treat.prb5, data=prb5.tibble)

#part d&e) least square means, std, and 95% CI of each lettuce treat group
#group trt=0.00
x_0.00<-mean(spect[1:4])
#MSE of residuals
std_0.00<-summary(prb5.anva)[[1]][["Mean Sq"]][2] #sd(spect[1:4])
CI_0.00<-c(x_0.00-std_0.00*qt(0.975, df=7), x_0.00+std_0.00*qt(0.975, df=7))
#group trt=0.05
x_0.05<-mean(spect[5:6])
std_0.05<-summary(prb5.anva)[[1]][["Mean Sq"]][2]#sd(spect[5:6])
CI_0.05<-c(x_0.05-std_0.05*qt(0.975, df=7), x_0.05+std_0.05*qt(0.975, df=7))
#group trt=0.10
x_0.10<-mean(spect[7:8])
std_0.10<-summary(prb5.anva)[[1]][["Mean Sq"]][2]#sd(spect[7:8])
CI_0.10<-c(x_0.10-std_0.10*qt(0.975, df=7), x_0.10+std_0.10*qt(0.975, df=7))
#group trt=0.20
x_0.20<-mean(spect[9:10])
std_0.20<-summary(prb5.anva)[[1]][["Mean Sq"]][2]#sd(spect[9:10])
CI_0.20<-c(x_0.20-std_0.20*qt(0.975, df=7), x_0.20+std_0.20*qt(0.975, df=7))
#group trt=0.50
x_0.50<-mean(spect[11:12])
std_0.50<-summary(prb5.anva)[[1]][["Mean Sq"]][2] #sd(spect[11:12])
CI_0.50<-c(x_0.50-std_0.50*qt(0.975, df=7), x_0.50+std_0.50*qt(0.975, df=7))

#part g) randomization of numbers 1-20
rndm_samp.prb5<-sample(treat.prb5, 12)


####Problem 6####
#data wrangle
treat.prb6<-c("Premolt","Premolt","Premolt","Premolt","Premolt","Fasting",
              "Fasting","Fasting","Fasting","Fasting", "60g bran","60g bran",
              "60g bran","60g bran","60g bran","80g bran","80g bran","80g bran",
              "80g bran","80g bran","Laying mash","Laying mash","Laying mash",
              "Laying mash","Laying mash")
treat.prb6<-factor(treat.prb6)
serum_t3.prb6<-c(94.09, 90.45, 99.38, 73.56, NA, 98.81, 103.55, 115.23, 129.06, 117.61,
            197.18, 207.31, 177.50, NA, NA, 102.93, 117.51, 119.92, 112.01, 101.10,
            82.94, 83.14, 89.59, 87.76, NA)
prb6.tibble<-tibble(treat.prb6, serum_t3.prb6)

#Part C) ANOVA
prb6.anva<-aov(serum_t3.prb6~treat.prb6, data=prb6.tibble)

#part d&e) least square means, std, and 95% CI of each lettuce treat group
#group trt=Premolt
x_Premolt.prb6<-mean(serum_t3.prb6[1:5], na.rm=T)
#MSE of residuals
std_Premolt.prb6<-summary(prb6.anva)[[1]][["Mean Sq"]][2] #sd(serum_t3.prb6[1:5], na.rm=T)
CI_Premolt.prb6<-c(x_Premolt.prb6-std_Premolt.prb6*qt(0.975, df=16), x_Premolt.prb6+std_Premolt.prb6*qt(0.975, df=16))
#group trt=Fasting
x_Fast.prb6<-mean(serum_t3.prb6[6:10], na.rm=T)
std_Fast.prb6<-summary(prb6.anva)[[1]][["Mean Sq"]][2]#sd(serum_t3.prb6[6:10], na.rm=T)
CI_Fast.prb6<-c(x_Fast.prb6-std_Fast.prb6*qt(0.975, df=16), x_Fast.prb6+std_Fast.prb6*qt(0.975, df=16))
#group trt=60 gram bran
x_60.prb6<-mean(serum_t3.prb6[11:15], na.rm=T)
std_60.prb6<-summary(prb6.anva)[[1]][["Mean Sq"]][2]#sd(serum_t3.prb6[11:15], na.rm=T)
CI_60.prb6<-c(x_60.prb6-std_60.prb6*qt(0.975, df=16), x_60.prb6+std_60.prb6*qt(0.975, df=16))
#group trt=80 gram bran
x_80.prb6<-mean(serum_t3.prb6[16:20], na.rm=T)
std_80.prb6<-summary(prb6.anva)[[1]][["Mean Sq"]][2]#sd(serum_t3.prb6[16:20], na.rm=T)
CI_80.prb6<-c(x_80.prb6-std_80.prb6*qt(0.975, df=16), x_80.prb6+std_80.prb6*qt(0.975, df=16))
#group trt=Laying mash
x_LM.prb6<-mean(serum_t3.prb6[21:25], na.rm=T)
std_LM.prb6<-summary(prb6.anva)[[1]][["Mean Sq"]][2] #sd(serum_t3.prb6[21:25], na.rm=T)
CI_LM.prb6<-c(x_LM.prb6-std_LM.prb6*qt(0.975, df=16), x_LM.prb6+std_LM.prb6*qt(0.975, df=16))