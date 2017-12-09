# need X , Y , parents
selection <- function( X , Y , parents , intercept ){

  # determine number of parents in a population
  P <- dim( parents )[ 1 ]

  # initialize empty vector for AIC
  AIC <- rep( 0 , P )

  # loop through each parent and regression with selected variables, and output AIC
  for ( i in 1:P ){
  	if ( intercept[ i ] == 1 ){
      AIC[ i ] <- AIC( lm( Y ~ . , data = data.frame( Y , X[ , which( parents[ i , ] == 1 ) ] ) ) )
  	} else {
  	  AIC[ i ] <- AIC( lm( Y ~ 0 + . , data = data.frame( Y , X[ , which( parents[ i , ] == 1 ) ] ) ) )
  	}
  }

  # fitness function based on rank, where higher rank gives higher fitness (probability)
  fitness_prob <- 2 / P / ( P + 1 ) * seq( 1:P )

  # assign fitness probabilities to calculated AICs, and select (stochastically) parents to keep
  select_ind <- sample( order( AIC , decreasing = TRUE ) , P , prob = fitness_prob , replace = TRUE )
  parents_selection <- parents[ select_ind , ]

  return(parents_selection)
}


selection( X = X , Y = Y , parents = starting[[ 1 ]] , intercept = starting[[ 2 ]] )

