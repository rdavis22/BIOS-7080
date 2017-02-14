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


####Problem 4.3####
###Data Input###
#number of stolons rooted vs. not rooted (response variable)
y<-c(15, 13, 13, 6, 16, 14, 8, 9, 8, 49, 51, 51, 42, 48, 50, 56, 55, 40,
        11, 11, 6, 4, 12, 9, 18, 10, 16, 53, 53, 58, 60, 52, 55, 46, 54, 48)
#which replication trial number
replication<-factor(c(rep ("replication 1", 18), rep("replication 2", 18)))
#rooted vs. not rooted
root<-factor(c(rep("rooted", 9), rep("not rooted", 9), rep("rooted", 9),
               rep("not rooted", 9)))
#clone number
clone<-factor(rep(1:9, 4))

##arcsin transform of y into x=asin(sqrt(pie_hat))
#get pie_hat estimate
pie_hat<-(y[root=="rooted"]+3/8)/(length(y[root=="rooted"])+3/4)
#arcsin transform of response variable
x<-asin(sqrt(pie_hat))

#combine the data into a tibble
prb4.tibble<-tibble(y, replication, root, clone)

##Perform ANOVA for the data (x vs. the rooted clones)
prb4.anva<-aov(x~clone[root=="rooted"])


####Problem 4.5####
###Data input###
rand_samp.prb5<-c(14.3, 16.0, 17.3, 17.5, 17.8, 18.7, 18.8, 18.9, 20.0, 20.8,
                  21.4, 22.7, 23.2, 25.6, 27.8)
##4.5a)##
# f-values
f_vals.prb5<-c()
for (i in seq_along(rand_samp.prb5)){
  f_vals.prb5[i]<-(i-0.5)/length(rand_samp.prb5)
}

#standard normal quantiles of f-values
snq.prb5<-qnorm(f_vals.prb5)

##4.5b)##
#form tibble for easier plotting
prb5.tibble<-tibble(rand_samp.prb5, f_vals.prb5, snq.prb5)

#plot of observations vs. standard normal quantiles
p.prb5<-ggplot(data=prb5.tibble, aes(x=snq.prb5, y=rand_samp.prb5))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  labs(title="Observations vs. Standard Normal Quantiles",
       x="Standard Normal Quantiles", y="Observations from Rand. Sample")

####Problem 4.6####
###Data input###
rand_samp.prb6<-c(2, 3, 4, 5, 10, 28, 34, 35, 39, 63, 87, 97, 112, 156, 188, 253)
##4.6a)##
# f-values
f_vals.prb6<-c()
for (i in seq_along(rand_samp.prb6)){
  f_vals.prb6[i]<-(i-0.5)/length(rand_samp.prb6)
}

#standard normal quantiles of f-values
snq.prb6<-qnorm(f_vals.prb6)

##4.5b)##
#form tibble for easier plotting
prb6.tibble<-tibble(rand_samp.prb6, f_vals.prb6, snq.prb6)

#plot of observations vs. standard normal quantiles
p.prb6<-ggplot(data=prb6.tibble, aes(x=snq.prb6, y=rand_samp.prb6))+
  geom_point()+
  geom_smooth(method="lm", se=F)+
  labs(title="Observations vs. Standard Normal Quantiles",
       x="Standard Normal Quantiles", y="Observations from Rand. Sample")