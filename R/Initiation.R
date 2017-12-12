#Create a set of individuals, where each individual is a string of 0s and 1s with 1s
#input
##vars: number of possible variables
##pSize: population size for each generation
##minC: min subset size in percent
##maxC: max subset size in percent
##output: Initial Matrix with 0s and 1s

initData <- matrix(rnorm(5000, sd = 1:5), ncol = 10, byrow = TRUE)
initOutcome <-1+-1*initData[,1]+2*initData[,3]+ 1.1*initData[,5]+1.2*initData[,7]

Initiation <- function(data, pSize, minC=0.1, maxC=0.9){
  
  vars<-ncol(data)-1
  numData<-nrow(data)
  if(pSize > numData)
    return("Population size cannot be greater than number of data")
  if(minC>maxC)
    return("minC cannot be greater than maxC")
  if(minC>=1 || minC<=0 || maxC >=1 || maxC <=0)
    return("minC and maxC should be the value between 0 and 1")

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
vars<-dim(initData)[2]

starting<-Initiation(vars, pSize, minC, maxC)
