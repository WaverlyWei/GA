# need data, outcome, parents
selection <- function( data , outcome , parents , intercept ){

  X <- data[ , !names( data ) %in% outcome ]
  Y <- data[ , names( data ) %in% outcome ]

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

# simulate data
initData <- matrix(rnorm(5000, sd = 1:5), ncol = 10, byrow = TRUE)
initOutcome <-1+-1*initData[,1]+2*initData[,3]+ 1.1*initData[,5]+1.2*initData[,7]
data <- data.frame( initData, initOutcome )

# set initiation parameters
minC<-0.1
maxC<-0.9
pSize<-15
vars<-dim(initData)[2]

# initiate parents (make sure to define Initiation function previously)
starting<-Initiation(vars, pSize, minC, maxC)

# run selection function
selection( data = data , outcome = "initOutcome" , parents = starting[[ 1 ]] , intercept = starting[[ 2 ]] )

