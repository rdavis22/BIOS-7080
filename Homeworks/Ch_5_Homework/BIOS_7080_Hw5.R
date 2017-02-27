####BIOS 7080: Hw Ch. 5####
if(!require(tidyverse))
  install.packages("tidyverse")
if(!require(lme4))
  install.packages("lme4")
if(!require(lmerTest))
  install.packages("lmerTest")
if(!require(lsmeans))
  install.packages("lsmeans")

####Problem E.g. 5.1####
casting<-factor(c(rep(1, 10), rep(2, 10), rep(3, 10)))
tens_stren<-c(88.0, 88.0, 94.8, 90.0, 93.0, 89.0, 86.0, 92.9, 89.0, 93.0, 85.9,
              88.6, 90.0, 87.1, 85.6, 86.0, 91.0, 89.6, 93.0, 87.5, 94.2, 91.5,
              92.0, 96.5, 95.6, 93.8, 92.5, 93.2, 96.2, 92.5)
prb1.tibble<-tibble(casting, tens_stren)

prb1.anva<-aov()