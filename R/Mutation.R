#' Mutation Function for Genetic Algorithm for Variable Selection
#'
#' Implements genetic algorithm mutation by randomly selection, with low probability, some of the independent variables to be excluded in the next generation
#' @param P_i The individual to be mutated
#' @param mutationProb Probability of mutation
#' @param C The number of independent variables to be selected from
#' @keywords genetic algorithm, model selection, mutation
#' @export
#' @examples
#' 
#' # define initial parameters
#' C <- 5
#'
#' # initiate a generation
#' generation <- initiation( C = C , P = 4 )
#' 
#' # apply mutation function to each row in generation
#' mut_gen <- apply( generation , 1 , mutation , mutationProb = 0.01 , C = C )
#' 


#' Mutation function
#'
#' Mutate an individual in GA

mutation <-function( P_i , mutationProb , C ){

  # Identifying which sites should be mutated
  mut <- which( runif( C , 0 , 1 ) < mutationProb )

  P_i[ mut ] <- ifelse( P_i[ mut ] == 1 , 0 , 1 )

  return( P_i )

}
