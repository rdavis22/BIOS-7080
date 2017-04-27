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


####Problem 4####
