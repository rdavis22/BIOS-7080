####BIOS 7080: Hw 4 (Agreement between data and model)####
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(car))
  install.packages("car")
if(!require(multcomp))
  install.packages("multcomp")
if(!require(agricolae))
  install.packages("agricolae")

####Problem 4.1####
###Data input###
#Test temperature
test_temp<-c(rep("1520", 6), rep("1620", 6), rep("1660", 6), rep("1708", 6))
test_temp<-factor(test_temp, levels = c("1520", "1620", "1660", "1708"))
#hours to failure
hrs_flr<-c(1953, 2135, 2471, 4727, 6134, 6314, 1190, 1286, 1550, 2125, 2557,
           2845, 651, 837, 848, 1038, 1361, 1543, 511, 651, 651, 652, 688, 729)
prb1.tibble<-tibble(test_temp, hrs_flr)

#ANOVA
prb1.anva<-aov(hrs_flr~test_temp, data=prb1.tibble)
prb1.residuals<-rstandard(prb1.anva)

##qqplot##
#make sure to load the ggqqline.R function!!!!
p.prb1<-ggqqline(prb1.residuals)+labs(title="QQPlot of Residuals")