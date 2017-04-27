####BIOS 7080: Ch. 15 Homework####
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

####Problem 1####
###Data Input###
#serum glucose levels
glucose<-c(28, 34, 32, 15, 29, 27, 12, 33, 28, 21, 44, 39, 22, 18, 12, 23, 22,
           10, 18, 16, 9, 25, 24, 15, 31, 30, 39, 28, 27, 36, 24, 26, 36, 21,
           26, 32)
