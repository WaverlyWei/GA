#Create a set of individuals, where each individual is a string of 0s and 1s with 1s
#input
##vars: number of possible variables
##pSize: population size for each generation
##minC: min subset size in percentile
##maxC: max subset size in percentile (1 to C)
##output: Initial Matrix with 0s and 1s
Initiation <- function(vars, pSize, minC, maxC){
  initMatrix<-matrix(data=NA, nrow = pSize, ncol = vars)
  prob<-runif(pSize,min=minC, max=maxC)
  for(i in 1:pSize)
    initMatrix[i,]<-sample(c(0,1), vars, replace = TRUE, c(1-prob[i],prob[i]))
  return(initMatrix)
}

## test result
minC<-0.1
maxC<-0.9
pSize<-10
vars<-20

starting<-Initiation(vars, pSize, minC, maxC)
apply(starting, 1, mean)
