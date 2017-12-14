#' Genetic Algorithm for Variable Selection
#'
#' A genetic algorithm for variable selection in regression problems
#' @param data The input data in the form of a dataframe. Each row is one entry with columns as different variables and the last column as the outcome.
#' @param model A formula object ( eg. data$y ~ x1 + x2^2 + x2:x3 ). Note: must specify data source for dependent variable
#' @param conv_criterion Convergence criterion
#' @param steps Maximum number of steps to run GA
#' @keywords genetic algorithm, model selection
#' @export
#' @examples
#' # simulate data
#' initData <- matrix( rnorm( 2500 , sd = c(1,  5, 7 , 100 , 40 ) ) , ncol = 5 , byrow = TRUE )
#' initOutcome <- 10 - 15 * initData[ , 1 ] + 2 * initData[ , 3 ] + 1.1 * initData[ , 5 ]
#'
#' # define input parameters
#' data <- data.frame( initData, initOutcome )
#' model <- data$initOutcome ~ X1 + X2 + X3 + X4 + X5
#'
#' # call select function
#' GAresults <- select( data = data , model = model )
#'
#' # plot convergence results
#' plot( GAresults[[ 2 ]] , pch = 16 , cex = 0.75 , xlab = "Step" , ylab = "Convergene Criterion")

select <- function( data , model , conv_criterion = 10e-8 , steps = 50 ){

  # model <- as.formula( model )
  mm <- model.matrix( model , data = data )

  # set C the length of each individual chromosome ,ie the number of vars
  C <- length( attr( mm , "assign" ) )

  # Randomly select P, the number of parents per generation, such that C < P < 2C with increasing probability for larger P
  tmp <- seq( from = 2 * round( ( C + 1 ) / 2 ) , to = 2 * C , by = 2 )
  P <- sample( tmp , 1 , prob = ( 2 / C ^ 2 ) * tmp )

  # Initialize the first generation
  init_parents <- initiation( C = C , P = P )

  # initialize output objects
  convergence <- NULL
  child_minAIC <- matrix( 0 , nrow = 1 , ncol = C )
  regressions <- vector( "list" , 1 )

  # select fittest parents to breed
  tmp <- selection( mm = mm , model = model , parents = init_parents , P = P )
  parents <- tmp$children
  parent_AIC <- tmp$minAIC
  child_minAIC[ 1 , ] <- tmp$child_minAIC
  regressions[[ 1 ]] <- tmp$reg_minAIC

  # print update
  cat( "step 0" , colnames( mm )[ which( tmp$child_minAIC == 1 ) ] , "\n" , sep = " ")

  # breed
  for ( i in 1:steps ){

    # initialize children
    children <- matrix( 0 , nrow = P , ncol = C )

    # crossover
    for ( j in seq( from = 1 , to = P , by = 2 ) ){

      tmp <- crossover( parents[ j , ] , parents[ j + 1 , ] , C = C )
      children[ j , ] <- tmp$P3
      children[ j + 1 , ] <- tmp$P4

    }

    # mutation
    children <- apply( children , 1 , mutation , mutationProb = 1/C , C = C )
    children <- t(children)

    # run regression with next generation
    tmp <- selection( mm = mm , model = model , parents = children , P = P )
    children <- tmp$children
    children_AIC <- tmp$minAIC
    child_minAIC <- rbind( child_minAIC , tmp$child_minAIC )
    regressions[[ i ]] <- tmp$reg_minAIC

    # print update
    cat( paste( "step" , i ) , colnames( mm )[ which( tmp$child_minAIC == 1 ) ] , "\n" , sep = " ")

    # calculate convergenc criterion
    convergence <- c( convergence , abs( children_AIC - parent_AIC ) )

    # check convergence criterion
    if( i > 5 ){ if( convergence[ i ] <= conv_criterion & convergence[ (i - 1 ) ] <= conv_criterion ){ break() } }

    # New parents
    parents <- children
    parent_AIC <- children_AIC

  }

  solution <- colnames( mm )[ which( tmp$child_minAIC == 1 ) ]

  # Returning the selected variables
  return( list( solution = solution, convergence = convergence , children_minAIC = child_minAIC , regressions = regressions ) )

}
