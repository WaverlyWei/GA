#Create a set of individuals, where each individual is a string of 0s and 1s with 1s
#input
##C: number of possible variables
##P: population size for each generation
##output: Initial Matrix with 0s and 1s

initiation <- function( C , P ){

  init_parents <- matrix( NA , nrow = P , ncol = C )
  prob <- runif( P , min = 0.1 , max = 0.9 )
  init_parents<-apply(prob,2,function(x) {
                            v<-sample(c(0,1), vars, replace = TRUE, c(1-x,x))
                            if(sum(v) == 0)
                              v[floor(runif(1, min=1, max=vars))] = 1
                            return(v)})

  return( init_parents )
}
