####BIOS 7080: Hw 4 (Agreement between data and model)####
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

####Problem 4.1####
###Data input###
#Test temperature
test_temp<-c(rep("1520", 6), rep("1620", 6), rep("1660", 6), rep("1708", 6))
test_temp<-factor(test_temp, levels = c("1520", "1620", "1660", "1708"))
#hours to failure
hrs_flr<-c(1953, 2135, 2471, 4727, 6134, 6314, 1190, 1286, 1550, 2125, 2557,
           2845, 651, 837, 848, 1038, 1361, 1543, 511, 651, 651, 652, 688, 729)
prb1.tibble<-tibble(test_temp, hrs_flr)
attach(prb1.tibble)

#ANOVA
prb1.anva<-aov(hrs_flr~test_temp, data=prb1.tibble)
#get the residuals of the ANOVA
prb1.residuals<-residuals(prb1.anva) #'rstandard' function would give standard residuals

###Part a)###
##qqplot(Normality Assumption)##
#make sure to load the ggqqline.R function!!!!
qq.prb1<-ggqqline(prb1.residuals)+labs(x="Quantile of Standard Normal",
                                       y="Residuals", title="QQPlot of Residuals")

##Spread-Location Plot(Equal Variances Assumption##
#predicted values
predvals.prb1<-predict(prb1.anva)
#square root of the absolute value of 
adjprb1.residuals<-sqrt(abs(prb1.residuals))

#data frame for spread-location plot
slp.data<-tibble(predvals.prb1, adjprb1.residuals)

slp.prb1<-ggplot(data=slp.data)+
  geom_point(aes(x=predvals.prb1, y=adjprb1.residuals))+
  #regression line for plot
  geom_smooth(aes(x=predvals.prb1, y=adjprb1.residuals), method="lm", se=FALSE)+
  labs(x="Estimated Means", y="Sqrt abs. residuals", title="Spread-Location Plot")

#*Alternative method for S-L plot
slp_alt.prb1<-spreadLevelPlot(prb1.anva)

##Residuals vs. Estimated Treatments Plot (Independence of Errors Assumption)##
resplot.prb1<-ggplot()+
  geom_point(aes(x=predvals.prb1, y=prb1.residuals))+
  labs(x="Estimated treatment Means", y="Residuals",
       title="Est. Treatment Means vs. Residuals")

#Levene Test (Median)
lvne_test.prb1<-leveneTest(prb1.anva, center=median)

###Part b)###
##groups means
mu_1520<-mean(test)