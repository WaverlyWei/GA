
X <- matrix(rnorm(5000, sd = 1:5), ncol = 10, byrow = TRUE)
Y<-1+-1*X[,1]+2*X[,3]+ 1.1*X[,5]+1.2*X[,7]

# need dataset, model, fitness, parents
selection <- function( X , Y , parents ){

  numInd <- dim( parents )[ 1 ]

  AIC <- rep( 0 , numInd )

  for ( i in 1:numInd ){
    AIC[ i ] <- AIC( lm( Y ~ . , data = data.frame( Y , X[ , which( parents[ i , ] == 1 ) ] ) ) )
  }

  select_ind <- sample( order( AIC ) , numInd , prob = seq( 1 , ( 1 / numInd ) , by = -( 1 / numInd ) ) , replace = TRUE )

  parents_selection <- parents[ select_ind , ]

  return(parents_selection)
}


selection( X = X , Y = Y , parents = parents )