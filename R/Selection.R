
GHFitness <- function( P ){ 2 / P / ( P + 1 ) * seq( 1:P ) }

calculateAIC <- function(data, outcome, parents, intercept){
  # make X, Y
  X <- data[ , !names( data ) %in% outcome ]
  Y <- data[ , names( data ) %in% outcome ]

  # initialize empty vector for AIC
  P = dim( parents )[ 1 ]
  AIC <- rep( 0 , P )

  # loop through each parent and regression with selected variables, and output AIC
  for ( i in 1:P ){
    if ( intercept[ i ] == 1 ){
      AIC[ i ] <- AIC( lm( Y ~ . , data = data.frame( Y , X[ , which( parents[ i , ] == 1 ) ] ) ) )
    } else {
      AIC[ i ] <- AIC( lm( Y ~ 0 + . , data = data.frame( Y , X[ , which( parents[ i , ] == 1 ) ] ) ) )
    }
  }
  return(AIC)
}

# need data, outcome, parents

selection <- function( data , outcome , parents , intercept , fitness = GHFitness ){

  # determine number of parents in a population
  P <- dim( parents )[ 1 ]

  # check fitness function length
  if( length( fitness( P ) ) != P ) { stop( "fitness function must output vector of length P" ) }

  # run fitness function
  fitness_prob <- fitness( P )

  # check fitness function output is increasing in probability
  if( !all.equal( order( fitness_prob ) , seq( 1:P ) ) ) { stop( "fitness function output must be increasing" ) }
  if( sum( 0 < fitness_prob & fitness_prob < 1 ) != P ) { stop( "fitness function output must be probabilities (between 0 and 1)" ) }

  AIC <- calculateAIC(data, outcome, parents, intercept)

  # assign fitness probabilities to calculated AICs, and select (stochastically) parents to keep
  select_ind <- sample( order( AIC , decreasing = TRUE ) , P , prob = fitness_prob , replace = TRUE )
  parents_selection <- parents[ select_ind , ]
  intercept_selection <- intercept[ select_ind ]

  return( list( parents_selection = parents_selection , intercept_selection = intercept_selection, AIC = AIC ) )
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
#starting<-Initiation(data, pSize, minC, maxC)

# run selection function
#selection( data = data , outcome = "initOutcome" , parents = starting[[ 1 ]] , intercept = starting[[ 2 ]] , fitness = GHFitness )

