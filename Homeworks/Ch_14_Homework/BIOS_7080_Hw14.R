####BIOS 7080: Ch. 14 Homework####
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

####Problem 14. 1####
###Data Input###
#weight of the seed per plant in grams
weights<-c(40.7, 24.2, 16.1, 11.2, 37.8, 44.4, 17.6, 12.7, 32.9, 27.8, 19.9,
           14.5, 43.1, 34.1, 20.1, 15.4, 39.4, 31.3, 17.9, 14.8, 47.8, 34.5,
           30.5, 17.3, 44.4, 25.6, 22.5, 17.7, 49.0, 50.4, 25.2, 18.7, 68.7,
           26.2, 20.5, 18.9, 56.2, 48.1, 28.2, 26.2, 44.8, 41.1, 30.0, 19.2,
           59.3, 46.0, 24.7, 22.0)
#plants per row (i.e. plant density)
plants<-factor(rep(c(10, 15, 25, 40), 12))
#block
block<-factor(rep(c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4)), 3))
#hybrids
hybrid<-factor(c(rep("TAM", 16), rep("RS", 16), rep("Tx", 16)))

##Dataframe for problem 14.1##
prb14_1.tibble<-tibble(weights, plants, block, hybrid)

###Part a)###
##ANOVA##
prb14_1.aov<-aov(weights~plants*hybrid+Error(block/plants), data = prb14_1.tibble)

###Part b)###
#grand mean
muweights<-mean(weights)
##Cell means##
celmu.list<-list(
  #mean for plant==10, hybrid=="TAM"
  celmu_10TAM<-mean(weights[plants=="10" & hybrid=="TAM"]),
  #mean for plant==15, hybrid=="TAM"
  celmu_15TAM<-mean(weights[plants=="15" & hybrid=="TAM"]),
  #mean for plant==25, hybrid=="TAM"
  celmu_25TAM<-mean(weights[plants=="25" & hybrid=="TAM"]),
  #mean for plant==40, hybrid=="TAM"
  celmu_40TAM<-mean(weights[plants=="40" & hybrid=="TAM"]),
  #mean for plant==10, hybrid=="RS"
  celmu_10RS<-mean(weights[plants=="10" & hybrid=="RS"]),
  #mean for plant==15, hybrid=="RS"
  celmu_15RS<-mean(weights[plants=="15" & hybrid=="RS"]),
  #mean for plant==25, hybrid=="RS"
  celmu_25RS<-mean(weights[plants=="25" & hybrid=="RS"]),
  #mean for plant==40, hybrid=="RS"
  celmu_40RS<-mean(weights[plants=="40" & hybrid=="RS"]),
  #mean for plant==10, hybrid=="Tx"
  celmu_10Tx<-mean(weights[plants=="10" & hybrid=="Tx"]),
  #mean for plant==15, hybrid=="Tx"
  celmu_15Tx<-mean(weights[plants=="15" & hybrid=="Tx"]),
  #mean for plant==25, hybrid=="Tx"
  celmu_25Tx<-mean(weights[plants=="25" & hybrid=="Tx"]),
  #mean for plant==40, hybrid=="Tx"
  celmu_40Tx<-mean(weights[plants=="40" & hybrid=="Tx"])
)
##Marginal Means for 'Hybrids'
marmuhyb.list<-list(
  marmu_TAM<-mean(weights[hybrid=="TAM"]),
  marmu_RS<-mean(weights[hybrid=="RS"]),
  marmu_Tx<-mean(weights[hybrid=="Tx"])
)
##Marginal Means for 'plants'
marmuplnt.list<-list(
  marmu_10<-mean(weights[plants=="10"]),
  marmu_15<-mean(weights[plants=="15"]),
  marmu_25<-mean(weights[plants=="25"]),
  marmu_40<-mean(weights[plants=="40"])
)

##Standard Errors##
#number of blocks (i.e. replications)
r.prb14_1<-length(levels(block))
#number of plant density treatments
a.prb14_1<-length(levels(plants))
#number of hybrids treatments
b.prb14_1<-length(levels(hybrid))
#MSE of the "whole-plot"
MSE1.prb14_1<-summary(prb14_1.aov)[[2]][[1]][['Mean Sq']][2]
#Df E1
DfE1.prb14_1<-summary(prb14_1.aov)[[2]][[1]][['Df']][2]
#MSE of the "sub-plots
MSE2.prb14_1<-summary(prb14_1.aov)[[3]][[1]][['Mean Sq']][3]
#Df E1
DfE2.prb14_1<-summary(prb14_1.aov)[[3]][[1]][['Df']][3]

#cell means standard errors
secelmu.prb14_1<-sqrt(MSE2.prb14_1/r.prb14_1)
#marginal mean standard errors for plants
semarmuplnt.prb14_1<-sqrt(MSE1.prb14_1/(r.prb14_1*b.prb14_1))
#marginal mean standard errors for hybrids
semarmuhyb.prb14_1<-sqrt(MSE2.prb14_1/(r.prb14_1*a.prb14_1))

###Part c)###
##Standard Errors for the differences between two means
#Hybrids
sediffhyb.prb14_1<-sqrt(2*MSE2.prb14_1/(r.prb14_1*a.prb14_1))
#Plants Populations
sediffplnt.prb14_1<-sqrt(2*MSE1.prb14_1/(r.prb14_1*b.prb14_1))
#Hybrids (at same level of "Plants")
sediffhybplnt.prb14_1<-sqrt(2*MSE2.prb14_1/r.prb14_1)

###Part d)###
##Mean Squares##
#Mean Square of plants
MSplants.prb14_1<-summary(prb14_1.aov)[[2]][[1]][['Mean Sq']][1]
#Mean Square hybrid
MShybrid.prb14_1<-summary(prb14_1.aov)[[3]][[1]][['Mean Sq']][1]
#Mean Square hybrid:plants
MSintrct.prb14_1<-summary(prb14_1.aov)[[3]][[1]][['Mean Sq']][2]

##Degrees of Freedom##
#Df plants
Dfplants.prb14_1<-summary(prb14_1.aov)[[2]][[1]][['Df']][1]
#Df hybrid
Dfhybrid.prb14_1<-summary(prb14_1.aov)[[3]][[1]][['Df']][1]
#Df hybrid:plants
Dfintrct.prb14_1<-summary(prb14_1.aov)[[3]][[1]][['Df']][2]

##F Tests##
#F Test for Interaction
F0_intrct.prb14_1<-MSintrct.prb14_1/MSE2.prb14_1
#critical F value (F(0.05, Dfplants.prb14_1, DfE1.prb14_1)
Fcrit_intrct<-qf(0.95, Dfintrct.prb14_1, DfE2.prb14_1)
#p_value
pF0_intrct<-pf(F0_intrct.prb14_1, Dfintrct.prb14_1, DfE2.prb14_1,lower.tail = F)

#F Test for Plants
F0_plants.prb14_1<-MSplants.prb14_1/MSE1.prb14_1
#critical F value (F(0.05, Dfplants.prb14_1, DfE1.prb14_1)
Fcrit_plants<-qf(0.95, Dfplants.prb14_1, DfE1.prb14_1)
#p_value
pF0_plants<-pf(F0_plants.prb14_1, Dfplants.prb14_1, DfE1.prb14_1,lower.tail = F)

#F Test for Hybrid
F0_hybrid.prb14_1<-MShybrid.prb14_1/MSE2.prb14_1
#critical F value (F(0.05, Dfplants.prb14_1, DfE1.prb14_1)
Fcrit_hybrid<-qf(0.95, Dfhybrid.prb14_1, DfE2.prb14_1)
#p_value
pF0_hybrid<-pf(F0_hybrid.prb14_1, Dfhybrid.prb14_1, DfE2.prb14_1,lower.tail = F)

###Part e)###
# error degrees of freedom for the RCBD
f2.prb14_1<-(a.prb14_1*b.prb14_1-1)*(r.prb14_1-1)
#"K" Factors for subplot and whole plot design
Ksub.prb14_1<-((DfE2.prb14_1+1)*(f2.prb14_1+3))/((DfE2.prb14_1+3)*(f2.prb14_1+1))
Kwhol.prb14_1<-((DfE1.prb14_1+1)*(f2.prb14_1+3))/((DfE1.prb14_1+3)*(f2.prb14_1+1))

##Relative Efficiency for subplot comparisons##
REsub.prb14_1<-Ksub.prb14_1*((a.prb14_1*(b.prb14_1-1)*MSE2.prb14_1)
                          +(a.prb14_1-1)*MSE1.prb14_1)/((a.prb14_1*b.prb14_1-1)*MSE2.prb14_1)

##Relative Efficiency for whole plot comparisons##
REwhol.prb14_1<-Kwhol.prb14_1*((a.prb14_1*(b.prb14_1-1)*MSE2.prb14_1)
                             +(a.prb14_1-1)*MSE1.prb14_1)/((a.prb14_1*b.prb14_1-1)*MSE1.prb14_1)

###Part f)###
##get the contrasts
#contrast matrix
mat.14_1<-matrix(c(c(1/4, 1/4, 1/4, 1/4), c(-0.546, -0.327, 0.109, 0.764),
              c(0.513, -0.171, -0.741, 0.399), c(-0.435, 0.783, -0.435, 0.087)), ncol=4)
#transpose and get the inverse
mymat.prb14_1<-solve(t(mat.14_1))
#remove the intercept
mycontrasts.prb14_1<-mymat.prb14_1[,2:4]
#apply the contrasts to the "plants" treatment variable
contrasts(prb14_1.tibble$plants)<-mycontrasts.prb14_1

##contrast ANOVA
#ANOVA model
prb14_1contr.aov<-aov(weights~plants*hybrid+Error(block/plants), data = prb14_1.tibble)
#partition sum of squares for the main effect of 'plants' and its interaction with 'hybrid'
summary(prb14_1contr.aov, split=list(plants=list("Linear"=1, "Quadratic" = 2, "Cubic"=3),
                                     `plants:hybrid`=list("Linear"=c(1,4), "Quadratic"=c(2,5), "Cubic"=c(3,6))))

#***NOTE!!!: The 2DF partitiion sum of squares for the interaction must contain...
#...the correct terms! Use a vector in the 'split' argument of 'summary.aov'...
#...to get the correct partition SS for the interaction based on the contrast matrix...
#E.g. since there are six terms in the interaction (i.e. plants=Linear, plants=Quadratic,...
#...plants=Cubic, hybrid=Linear, hybrid=Quadratic, hybrid=Cubic), the vector in the...
#'split' argument must contain the appropriate contrasts for each interaction term...
#'#(i.e. "Linear" must contain linear components (i.e. 1, 4), "Quadratic" must...
#'#contain quadratic component (2, 5))

##Profile Plot## (based off the cell and marginal means table)
#get a vector of the cell means
celmu14_1.vec<-unlist(celmu.list)
#get a vector of the plant population densities to match the cell means
plants.vec<-factor(rep(c("10", "15", "25", "40"), 3))
#get a vector of the hybrdis to match the cell means
hybrids.vec<-factor(c(rep("TAM 680", 4), rep("RS 671", 4), rep("Tx 399*Tx2536", 4)))
#dataframe for the profile plot
prb14_1profplot.tibble<-tibble(celmu14_1.vec, plants.vec, hybrids.vec)

#Prof. Plot
profplt.prb14_1<-ggplot(data = prb14_1profplot.tibble,
                       aes(x=plants.vec , y=celmu14_1.vec,
                           colour=hybrids.vec, group=hybrids.vec))+
  geom_point()+
  geom_line()+
  labs(y="Head Seed weight (g)", x="Plant Pop. Density", title="Head See weight (g) vs. Plant Pop. Density for three types of Plant Hybrids",
       legend="Plant Hybrids")


####Problem 14.7####
###Data Input###
#efficiency of water use
efficiency<-c(8.1, 9.7, 8.6, 15.5, 36.0, 34.2, 34.5, 33.1, 34.6, 34.0, 40.7,
              39.3, 10.0, 6.2, 5.1, 10.9, 21.5, 19.7, 19.9, 21.9, 30.7, 28.9,
              26.4, 25.7, 10.6, 6.3, 4.5, 10.4, 19.4, 19.7, 21.7, 19.9, 23.2,
              23.0, 19.4, 23.2)
#water
water<-factor(c(rep("16", 12), rep("22", 12), rep("28", 12)))
#nitrogen
nitrogen<-factor(rep(c(rep("0", 4), rep("130", 4), rep("260", 4)), 3))
#Phosphorous rates
phosphorus<-factor(rep(c("P1","P2"), 18))
#block
block.prb14_7<-factor(rep(c("B1", "B1", "B2", "B2"), 9))

##Dataframe for Ch. 14, problem 7##
prb14_7.tibble<-tibble(efficiency, water, nitrogen, phosphorus, block.prb14_7)

###Part a)###
##ANOVA##
prb14_7.aov<-aov(efficiency~nitrogen*water*phosphorus+Error(block.prb14_7/phosphorus),
                 data = prb14_7.tibble)

###Part b)###
#grand mean
muefficiency<-mean(efficiency)
##Cell means##
celmu14_7.list<-list(
  #mean for water==16, nitrogen==0, phosphorus==P1
  celmu_16_0_P1<-mean(efficiency[water=="16" & nitrogen=="0" & phosphorus=="P1"]),
  #mean for water==16, nitrogen==130, phosphorus==P1
  celmu_16_130_P1<-mean(efficiency[water=="16" & nitrogen=="130" & phosphorus=="P1"]),
  #mean for water==16, nitrogen==260, phosphorus==P1
  celmu_16_260_P1<-mean(efficiency[water=="16" & nitrogen=="260" & phosphorus=="P1"]),
  #mean for water==22, nitrogen==0, phosphorus==P1
  celmu_22_0_P1<-mean(efficiency[water=="22" & nitrogen=="0" & phosphorus=="P1"]),
  #mean for water==22, nitrogen==130, phosphorus==P1
  celmu_22_130_P1<-mean(efficiency[water=="22" & nitrogen=="130" & phosphorus=="P1"]),
  #mean for water==22, nitrogen==260, phosphorus==P1
  celmu_22_260_P1<-mean(efficiency[water=="22" & nitrogen=="260" & phosphorus=="P1"]),
  #mean for water==28, nitrogen==0, phosphorus==P1
  celmu_28_0_P1<-mean(efficiency[water=="28" & nitrogen=="0" & phosphorus=="P1"]),
  #mean for water==28, nitrogen==130, phosphorus==P1
  celmu_28_130_P1<-mean(efficiency[water=="28" & nitrogen=="130" & phosphorus=="P1"]),
  #mean for water==28, nitrogen==260, phosphorus==P1
  celmu_28_260_P1<-mean(efficiency[water=="28" & nitrogen=="260" & phosphorus=="P1"]),
  #mean for water==16, nitrogen==0, phosphorus==P2
  celmu_16_0_P2<-mean(efficiency[water=="16" & nitrogen=="0" & phosphorus=="P2"]),
  #mean for water==16, nitrogen==130, phosphorus==P2
  celmu_16_130_P2<-mean(efficiency[water=="16" & nitrogen=="130" & phosphorus=="P2"]),
  #mean for water==16, nitrogen==260, phosphorus==P2
  celmu_16_260_P2<-mean(efficiency[water=="16" & nitrogen=="260" & phosphorus=="P2"]),
  #mean for water==22, nitrogen==0, phosphorus==P2
  celmu_22_0_P2<-mean(efficiency[water=="22" & nitrogen=="0" & phosphorus=="P2"]),
  #mean for water==22, nitrogen==130, phosphorus==P2
  celmu_22_130_P2<-mean(efficiency[water=="22" & nitrogen=="130" & phosphorus=="P2"]),
  #mean for water==22, nitrogen==260, phosphorus==P2
  celmu_22_260_P2<-mean(efficiency[water=="22" & nitrogen=="260" & phosphorus=="P2"]),
  #mean for water==28, nitrogen==0, phosphorus==P2
  celmu_28_0_P2<-mean(efficiency[water=="28" & nitrogen=="0" & phosphorus=="P2"]),
  #mean for water==28, nitrogen==130, phosphorus==P2
  celmu_28_130_P2<-mean(efficiency[water=="28" & nitrogen=="130" & phosphorus=="P2"]),
  #mean for water==28, nitrogen==260, phosphorus==P2
  celmu_28_260_P2<-mean(efficiency[water=="28" & nitrogen=="260" & phosphorus=="P2"])
)
##Marginal Means##
#'N*W'
marmuNW.list<-list(
   marmuNW_16_0<-mean(efficiency[water=="16"& nitrogen=="0"]),
   marmuNW_16_130<-mean(efficiency[water=="16"& nitrogen=="130"]),
   marmuNW_16_260<-mean(efficiency[water=="16"& nitrogen=="260"]),
   marmuNW_22_0<-mean(efficiency[water=="22"& nitrogen=="0"]),
   marmuNW_22_130<-mean(efficiency[water=="22"& nitrogen=="130"]),
   marmuNW_22_260<-mean(efficiency[water=="22"& nitrogen=="260"]),
   marmuNW_28_0<-mean(efficiency[water=="28"& nitrogen=="0"]),
   marmuNW_28_130<-mean(efficiency[water=="28"& nitrogen=="130"]),
   marmuNW_28_260<-mean(efficiency[water=="28"& nitrogen=="260"])
)
#W*P
marmuWP.list<-list(
  marmuWP_16_P1<-mean(efficiency[water=="16"& phosphorus=="P1"]),
  marmuWP_22_P1<-mean(efficiency[water=="22"& phosphorus=="P1"]),
  marmuWP_28_P1<-mean(efficiency[water=="28"& phosphorus=="P1"]),
  marmuWP_16_P2<-mean(efficiency[water=="16"& phosphorus=="P2"]),
  marmuWP_22_P2<-mean(efficiency[water=="22"& phosphorus=="P2"]),
  marmuWP_28_P2<-mean(efficiency[water=="28"& phosphorus=="P2"])
)
#N*P
marmuNP.list<-list(
  marmuNP_16_P1<-mean(efficiency[nitrogen=="0"& phosphorus=="P1"]),
  marmuNP_22_P1<-mean(efficiency[nitrogen=="130"& phosphorus=="P1"]),
  marmuNP_28_P1<-mean(efficiency[nitrogen=="260"& phosphorus=="P1"]),
  marmuNP_16_P2<-mean(efficiency[nitrogen=="0"& phosphorus=="P2"]),
  marmuNP_22_P2<-mean(efficiency[nitrogen=="130"& phosphorus=="P2"]),
  marmuNP_28_P2<-mean(efficiency[nitrogen=="260"& phosphorus=="P2"])
)
#Means of Phosphorus
muphos<-c(mean(efficiency[phosphorus=="P1"]), mean(efficiency[phosphorus=="P2"]))
#Means of Nitrogen
munitro<-c(mean(efficiency[nitrogen=="0"]), mean(efficiency[nitrogen=="130"]),
         mean(efficiency[nitrogen=="260"]))
#Means of Water
muwater<-c(mean(efficiency[water=="16"]), mean(efficiency[water=="22"]),
           mean(efficiency[water=="28"]))

##Standard Errors##
#number of levels of blocks (i.e. replications)
r.prb14_7<-length(levels(block.prb14_7))
#number of phosphorus treatments
a.prb14_7<-length(levels(phosphorus))
#number of nitrogen treatments
b.prb14_7<-length(levels(nitrogen))
#number of water treatments
c.prb14_7<-length(levels(water))
#MSE of the "whole-plot"
MSE1.prb14_7<-summary(prb14_7.aov)[[2]][[1]][['Mean Sq']][2]
#MSE of the "sub-plots
MSE2.prb14_7<-summary(prb14_7.aov)[[3]][[1]][['Mean Sq']][7]
 
#cell means standard errors
secelmu.prb14_7<-sqrt(MSE2.prb14_7/r.prb14_7)
#marginal mean standard errors for NW
semarmuNW.prb14_7<-sqrt(MSE2.prb14_7/(r.prb14_7*a.prb14_7))
#marginal mean standard errors for PW
semarmuPW.prb14_7<-sqrt(MSE2.prb14_7/(r.prb14_7*b.prb14_7))
#marginal mean standard errors for NP
semarmuNP.prb14_7<-sqrt(MSE2.prb14_7/(r.prb14_7*c.prb14_7))
#marginal means for P
semarmuP.prb14_7<-sqrt(MSE1.prb14_7/(b.prb14_7*c.prb14_7*r.prb14_7))
#marginal means for N
semarmuN.prb14_7<-sqrt(MSE2.prb14_7/(a.prb14_7*c.prb14_7*r.prb14_7))
#marginal means for W
semarmuW.prb14_7<-sqrt(MSE2.prb14_7/(a.prb14_7*b.prb14_7*r.prb14_7))

##Standard Errors for the differences between two means
#Phosphorus
sediffPhos.prb14_7<-sqrt(2*MSE1.prb14_7/(b.prb14_7*c.prb14_7*r.prb14_7))
#Nitrogen
sediffNitro.prb14_7<-sqrt(2*MSE2.prb14_7/(a.prb14_7*c.prb14_7*r.prb14_7))
#Water
sediffWater.prb14_7<-sqrt(2*MSE2.prb14_7/(a.prb14_7*b.prb14_7*r.prb14_7))
#Water by nitrogen
sediffWN.prb14_7<-sqrt(2*MSE2.prb14_7/(a.prb14_7*r.prb14_7))

###Part d)###
#***See the ANOVA output from part a)

###Part e)###
##Get the Contrasts##
#apply the contrasts to the "plants" treatment variable
contrasts(prb14_7.tibble$nitrogen)<-contr.poly(3)
contrasts(prb14_7.tibble$water)<-contr.poly(3)

##contrast ANOVA
#ANOVA model
prb14_7contr.aov<-aov(efficiency~nitrogen*water*phosphorus+Error(block.prb14_7/phosphorus),
                      data = prb14_7.tibble)
#partition sum of squares for the main effect of 'plants' and its interaction with 'hybrid'
summary(prb14_7contr.aov, split=list(nitrogen=list("Linear"=1, "Quadratic"=2),
                                     water=list("Linear"=1, "Quadratic"=2),
                                     `nitrogen:water`=list("Linear"=1, "Quadratic"=2),
                                     `nitrogen:phosphorus`=list("Linear"=1, "Quadratic"=2),
                                     `water:phosphorus`=list("Linear"=1, "Quadratic"=2),
                                     `nitrogen:water:phosphorus`=list("L:L"=1, "L:Q"=2, "Q:L"=3, "Q:Q"=4)))[[3]][[1]]
                                     