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
###BUild the Contrasts###