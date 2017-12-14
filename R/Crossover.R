#' Crossover Function for Genetic Algorithm for Variable Selection
#'
#' Implements genetic algorithm crossover at randomly selected locus for two individuals in a generation
#' @param P1 First individual
#' @param P2 Second individual
#' @param C The number of independent variables to be selected from
#' @keywords genetic algorithm, model selection, crossover
#' @export
#' @examples
#' # define initial parameters
#' C <- 5
#'
#' # initiate parent generation
#' parents <- initiation( C = C , P = 30 )
#' 
#' # call crossover function
#' crossover( P1 = parents[ 1 , ] , P2 = parents[ 2 , ] , C = C )

crossover <- function( P1 , P2 , C ){

  # Choose crossover site randomly
  site = floor( runif( 1 , min = 1 , max = C - 1 ) )

  # Cut&Ligate parent strings
  P3 = c( P1[ 1:site ] , P2[ ( site + 1 ):C ] )
  P4 = c( P1[ ( site + 1 ):C ] , P2[ 1:site ] )

  ##Perhaps return all four strings??
  return( list( P3 = P3 , P4 = P4 ) )

}


