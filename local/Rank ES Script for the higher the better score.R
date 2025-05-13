#### R script for calculation of Rank equivalent score ####
#
# reference paper
# https://doi.org/10.1007/s10072-022-06140-6

# second scenario, the higher the better

# Building a non-parametrical dataset 
# This part is useful only for simulation, not necessary for practical use
set.seed(4) 
x <- c(rnorm(220,40,10),runif(80,0,60))

#package tolerance is required, install it only the first time
install.packages("tolerance")

#load package
library(tolerance)

#start from here assigning to x your adjusted score

#check the shape of distribution
shapiro.test(x) #non parametric

#TLs calculation for a variable in which the lower the better
OTL <- nptol.int(x,0.05,0.95,1,method="WILKS")[[3]]
ITL <- nptol.int(x,0.95,0.95,1,method="WILKS")[[3]]

#Find the position of OTL
pES <- which.min(abs(sort(x,decreasing=FALSE) - OTL))

#Position of Median
pES[4] <- ceiling((length(x)+1)/2)

#Range
Step <- floor((pES[4] - pES[1])/3) 
pES[2] <- pES[1]+Step
pES[3] <- pES[1]+2*Step

#Results: observations
print(paste("The cutoff observations are",paste(as.character(pES),collapse=", ")))

#Results: values
print(paste("Outer and inner tolerance limits are:",
            as.character(round(OTL,2)),"and",as.character(round(ITL,2))))
print(paste("Cutoff values are",paste(round(sort(x,decreasing = F)[pES],2), collapse = ", "))) 
