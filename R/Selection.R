#' Selection Function for Genetic Algorithm for Variable Selection
#'
#' Selects independent variables to be bred by genetic algorithm based on AIC fitness function
#' @param mm Model matrix object with intercept and column for each independent variable specified in model
#' @param model A formula object ( eg. data$y ~ x1 + x2^2 + x2:x3 ). Note: must specify data source for dependent variable
#' @param parents Parents matrix of P rows indicating with 0 or 1 which  variable selection
#' @param P Population size for each generation
#' @keywords genetic algorithm, model selection, selection
#' @export
#' @examples 
#' # simulate data
#' initData <- matrix( rnorm( 2500 , sd = 1:5 ) , ncol = 5 , byrow = TRUE )
#' initOutcome <- 1 - 1 * initData[ , 1 ] + 2 * initData[ , 3 ] + 1.1 * initData[ , 5 ]
#' data <- data.frame( initData, initOutcome )
#' 
#' # define input parameters
#' P <- 30
#' parents <- initiation( C = 5 , P = P )
#' model <- data$initOutcome ~ X1 + X2 + X3 + X4 + X5
#' mm <- model.matrix( model , data = data )
#'
#' # call selection function
#' selection( mm = mm , model = model , parents = parents , P = P )

selection <- function( mm , model , parents, P ){

  parents <- subset( parents , rowSums( parents ) > 0 )
  if ( dim( parents )[ 1 ] < P ){
    for ( i in ( dim( parents )[ 1 ] + 1 ):P ){
      parents <- rbind( parents , parents[ i - 1 , ] )
    }
  }

  # initialize empty vector for AIC
  AIC <- rep( 0 , P )

  for ( i in 1:P ){
    dat <- data.frame( subset( mm , select = colnames( mm )[ which( parents[ i , ] == 1 ) ] ) )
    mod <- reformulate( colnames( dat ) , model[[ 2 ]] )
    AIC[ i ] <- AIC( lm( mod , data = dat ) )
  }

  fit_prob <- 2 / P / ( P + 1 ) * seq( 1:P ) # givens_hoeting fitness formula

  # assign fitness probabilities to calculated AICs, and select (stochastically) parents to keep
  AIC_ord <- order( AIC , decreasing = TRUE )
  select_ind <- sample( AIC_ord , P , prob = fit_prob , replace = TRUE )

  children <- parents[ select_ind , ]
  child_minAIC <- parents[ tail( n = 1 , AIC_ord ) , ]

  return( list( children = children , minAIC = min( AIC ) , child_minAIC = child_minAIC ) )
}



