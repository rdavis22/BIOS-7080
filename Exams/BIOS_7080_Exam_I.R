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

##b) assess model adequecy, fit, error normality, and error homoscedasticity
##model fit
#calculate residuals
prb1.residuals<-residuals(prb1.anva)
