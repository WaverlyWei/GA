#' Mutation function
#'
#' Mutate an individual in GA
#' @param P_i The individual to be mutated
#' @param mutationProb Probability of mutation
#' @param C number of variables

mutation <-function( P_i , mutationProb , C ){

  # Identifying which sites should be mutated
  mut <- which( runif( C , 0 , 1 ) < mutationProb )

  P_i[ mut ] <- ifelse( P_i[ mut ] == 1 , 0 , 1 )

  return( P_i )

}
