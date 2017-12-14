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
    mod <- reformulate( colnames( dat ) , model[[2]] )
    AIC[ i ] <- AIC( lm( mod , data = dat ) )
  }

  fit_prob <- 2 / P / ( P + 1 ) * seq( 1:P ) # givens_hoeting fitness formula

  # assign fitness probabilities to calculated AICs, and select (stochastically) parents to keep
  select_ind <- sample( order( AIC , decreasing = TRUE ) , P , prob = fit_prob , replace = TRUE )
  children <- parents[ select_ind , ]

  return( list( children = children , minAIC = min( AIC ) ) )
}