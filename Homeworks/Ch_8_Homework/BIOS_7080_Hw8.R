####BIOS 7080: Ch. 8 Homework####
###Load Libraries###
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

####Problem 8.1####
###Data Input###
#weight in pounds of harvested fruit
weight<-c(450, 469, 249, 125, 280, 352, 221, 251, 358, 512, 281, 58, 352, 293,
          283, 186, 331, 402, 183, 70, 258, 281, 219, 46, 317, 423, 379, 63, 289,
          239, 269, 357, 479, 341, 404, 115, 182, 349, 276, 182, 245, 380, 263,
          62, 336, 282, 171, 98)
#method of irrgation treatment
method<-factor(c(rep("Trickle", 8), rep("Basin", 8), rep("Spray", 8),
                 rep("Sprinkler", 8), rep("Sprinkler+Spray", 8), rep("Flood", 8)))
#block
block.prb8_1<-factor(rep(c(1, 2, 3, 4, 5, 6, 7, 8), 6))

##Dataframe for prb 8.1##
prb8_1.tibble<-tibble(weight, method, block.prb8_1)

###Part a)###
prb8_1.aov<-aov(weight~method+block.prb8_1, data=prb8_1.tibble)
