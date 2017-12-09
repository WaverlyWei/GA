#Create a set of individuals, where each individual is a string of 0s and 1s with 1s 
#input
##vars: number of possible variables
##pSize: population size for each generation
##minC: min subset size in percentile
##maxC: max subset size in percentile (1 to C)
##output: Initial Matrix with 0s and 1s

data <- matrix(rnorm(5000, sd = 1:5), ncol = 10, byrow = TRUE)
outcome <-1+-1*data[,1]+2*data[,3]+ 1.1*data[,5]+1.2*data[,7]

Initiation <- function(vars, pSize, minC, maxC){
  initMatrix<-matrix(data=NA, nrow = pSize, ncol = vars)
  prob<-runif(pSize,min=minC, max=maxC)
  for(i in 1:pSize)
    initMatrix[i,]<-sample(c(0,1), vars, replace = TRUE, c(1-prob[i],prob[i]))
  intercept<-sample(c(0,1),pSize,replace = TRUE)
  
  return(list("InitMatrix"<-initMatrix, "Intercept"<-intercept))
}

## test result
minC<-0.1
maxC<-0.9
pSize<-15
vars<-dim(data)[2]

starting<-Initiation(vars, pSize, minC, maxC)