#Create a set of individuals, where each individual is a string of 0s and 1s with 1s
#input
##vars: number of possible variables
##pSize: population size for each generation
##minC: min subset size in percent
##maxC: max subset size in percent
##output: Initial Matrix with 0s and 1s

initiation <- function( C , P ){

  init_parents <- matrix( NA , nrow = P , ncol = C )
  prob <- runif( P , min = 0.1 , max = 0.9 )
  for( i in 1:P ){
    init_parents[ i , ] <- sample( c( 0 , 1 ) , C , replace = TRUE , c( 1 - prob[ i ] , prob[ i ] ) )
  }

  return( init_parents )
}
