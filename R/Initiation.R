#' Initiation Function for Genetic Algorithm for Variable Selection
#'
#' Generates initial parent generation to be used by select()
#' @param C The number of independent variables to be selected from
#' @param P Population size for each generation
#' @keywords genetic algorithm, model selection, initiation
#' @export
#' @examples 
#' # call initiation function
#' init_parents <- initiation( C = 5 , P = 30 )

initiation <- function( C , P ){

  init_parents <- matrix( NA , nrow = P , ncol = C )
  prob<-matrix(runif( P , min = 0.1 , max = 0.15 ), nrow=1)

  init_parents<-apply(prob,2,function(x) {
                            v<-sample(c(0,1), C, replace = TRUE, c(1-x,x))
                            if(sum(v) == 0)
                              v[floor(runif(1, min=1, max=C))] = 1
                            return(v)})

  init_parents<-t(init_parents)

  return( init_parents )
}
