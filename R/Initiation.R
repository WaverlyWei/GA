#' Initiation function
#'
#' Create a set of individuals, where each individual is a string of 0s and 1s with 1s being the variables being used
#' @param P Population size for each generation
#' @param C number of variables
#'

initiation <- function( C , P ){

  init_parents <- matrix( NA , nrow = P , ncol = C )
  prob <- runif( P , min = 0.1 , max = 0.9 )
  prob<-matrix(runif( P , min = 0.1 , max = 0.9 ), nrow=1)

  init_parents<-apply(prob,2,function(x) {
                            v<-sample(c(0,1), C, replace = TRUE, c(1-x,x))
                            if(sum(v) == 0)
                              v[floor(runif(1, min=1, max=C))] = 1
                            return(v)})

  init_parents<-t(init_parents)

  return( init_parents )
}
