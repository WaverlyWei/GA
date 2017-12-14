#' Crossoveer function
#'
#' Crossover of two individuals in GA
#' @param P1 First individual
#' @param P2 Second individual
#' @param C number of variables

crossover <- function( P1 , P2 , C ){

  # Choose crossover site randomly
  site = floor( runif( 1 , min = 1 , max = C - 1 ) )

  # Cut&Ligate parent strings
  P3 = c( P1[ 1:site ] , P2[ ( site + 1 ):C ] )
  P4 = c( P1[ ( site + 1 ):C ] , P2[ 1:site ] )

  ##Perhaps return all four strings??
  return( list( P3 = P3 , P4 = P4 ) )
}


