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
  geom_smooth(aes(x=predvals.prb1, y=adjprb1.residuals), method="loess", se=FALSE)+
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

###Part b): Ladder of Powers transformation###
#groups means and standard deviations##
mu_1520<-mean(hrs_flr[test_temp=="1520"])
mu_1620<-mean(hrs_flr[test_temp=="1620"])
mu_1660<-mean(hrs_flr[test_temp=="1660"])
mu_1708<-mean(hrs_flr[test_temp=="1708"])
std_1520<-sd(hrs_flr[test_temp=="1520"])
std_1620<-sd(hrs_flr[test_temp=="1620"])
std_1660<-sd(hrs_flr[test_temp=="1660"])
std_1708<-sd(hrs_flr[test_temp=="1708"])
#vectors of all the means and standard deviations
mu.prb1b<-c(mu_1520, mu_1620, mu_1660, mu_1708)
std.prb1b<-c(std_1520, std_1620, std_1660, std_1708)
##Linear regression of log(std) vs. log(mu) to get estimate for Beta
trnsfrm.prb1<-lm(log(std.prb1b)~log(mu.prb1b)) #Beta= -1.733 (use -1 in "ladder of powers")
p.prb1b<-ggplot()+
  geom_point(aes(x=log(mu.prb1b), y=log(std.prb1b)))+
  geom_abline(aes(slope=trnsfrm.prb1$coefficients[2],
              intercept = trnsfrm.prb1$coefficients[1]))+
  labs(x="Log of the Mean", y="Log of the Standard Deviation")

###1c): Transformed "hrs_failure" (x=1/y)###
#transformed (1/hrs_flr) "hrs_flr" data based on "ladder of powers"
hrs_flrtrnsfrm<-1/hrs_flr

#ANOVA with transformed data
detach(prb1.tibble)
prb1c.anva<-aov(hrs_flrtrnsfrm~test_temp, data=prb1c.tibble)

#QQ Plot of transformed Data
qq.prb1c<-ggqqline(prb1c.anva$residuals)+labs(x="Quantile of Standard Normal",
                                       y="Residuals", title="QQPlot of Residuals")

##data frame for problem 1c
#predicted values
predvals.prb1c<-1/predvals.prb1
#adjusted residuals
adjprb1c.resid<-sqrt(abs(prb1c.anva$residuals))
  
prb1c.tibble<-tibble(predvals.prb1c, adjprb1c.resid)
#Spread-Location Plot (transformed data)
slp.prb1c<-ggplot(data=prb1c.tibble, aes(x=1/predvals.prb1, y=adjprb1c.resid))+
  geom_point()+
  geom_smooth(method = "loess", se=F)+
  labs(x="Estimated Means", y="Sqrt abs. residuals", title="Spread-Location Plot (Transformed)")

#Levene Test (transformed data)
lvne_test.prb1c<-leveneTest(prb1c.anva, center=median)

###1d):Custom contrasts for ANOVA of transformed data###
#initial contrast matrix (first column is the intercept term(1/number of terms repeated by the length of the number of terms))
mat<-cbind(rep(1/4, 4), "linear"=c(-0.773, -0.051, 0.238, 0.585),
           "Quadratic"=c(0.382, -0.637, -0.328, 0.583),
           "Cubic"=c(-0.078, 0.584, -0.765, 0.259))
#final contrast matrix (inverse of original transposed)
mymat<-solve(t(mat))

#remove the intercept term (1st column of "1's")
my.contrasts<-mymat[,2:4]

##create tibble of transformed data, apply contrasts, and then run anova##
#multiply transform "hours to failure" by 1000
hrs_flrtrnsfrm.prb1d<-1000*hrs_flrtrnsfrm
#create tibble
prb1d.tibble<-tibble(hrs_flrtrnsfrm.prb1d, test_temp)

#apply contrasts
contrasts(prb1d.tibble$test_temp)<-my.contrasts

#ANOVA with custom contrasts
prb1d.anva<-aov(hrs_flrtrnsfrm.prb1d~test_temp, data=prb1d.tibble)
#use "summary.lm" to get the summary statistics for factored anova (recall:...
#'test_temp' is class factor). This will use t-test in R as opposed to F-test...
#in most statistics textbooks.