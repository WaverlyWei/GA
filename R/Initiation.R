#Create a set of individuals, where each individual is a string of 0s and 1s with 1s
#input
##vars: number of possible variables
##pSize: population size for each generation
##minC: min subset size in percent
##maxC: max subset size in percent
##output: Initial Matrix with 0s and 1s

initData <- matrix(rnorm(5000, sd = 1:5), ncol = 10, byrow = TRUE)
initOutcome <-1+-1*initData[,1]+2*initData[,3]+ 1.1*initData[,5]+1.2*initData[,7]

initiation <- function(data, pSize=length(data[1,])*5, minC=0.1, maxC=0.9){

  vars<-ncol(data)-1
  numData<-nrow(data)
  if(minC>maxC)
    stop("minC cannot be greater than maxC")
  if(minC>=1 || minC<=0 || maxC >=1 || maxC <=0)
    stop("minC and maxC should be the value between 0 and 1")

  initMatrix<-matrix(data=NA, nrow = pSize, ncol = vars)
  
  prob<-matrix(runif(pSize,min=minC, max=maxC), nrow=1)
  
  initMatrix<-apply(prob,2,function(x) {
                            v<-sample(c(0,1), vars, replace = TRUE, c(1-x,x))
                            if(sum(v) == 0)
                              v[floor(runif(1, min=1, max=vars))] = 1
                            return(v)})

 initMatrix<-t(initMatrix) 
 
 intercept<-sample(c(0,1),pSize,replace = TRUE)

  return(list("InitMatrix"<-initMatrix, "Intercept"<-intercept))
}

