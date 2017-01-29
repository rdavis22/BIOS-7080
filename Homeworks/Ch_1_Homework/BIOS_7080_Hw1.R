#####BIOS 7080: Hw 1####
##install packages##
if(!require(tidyverse))
  install.packages("tidyverse")

####Problem 1.12####
A<-c(7, 5, 10)
B<-c(10, 9, 12)
reps<-10000
results<-numeric(reps)
#concatenate A and B
x<-c(A, B)
for (i in 1:reps){
  #randomly sample the concatenated vector
  tmp<-sample(x)
  #take the difference of the sampled 'A' from the sampled 'B' "reps" number of times
  results[i]<-mean(tmp[4:6])-mean(temp[1:3])
}

#get a histogram visualization of the results
hist(results)

#get the p-value of the test result
p_val<-sum(results>=0)/reps