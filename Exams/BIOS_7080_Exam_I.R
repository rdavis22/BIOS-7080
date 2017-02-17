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