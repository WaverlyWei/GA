initData <- matrix(rnorm(5000, sd = 1:5), ncol = 10, byrow = TRUE)
initOutcome <-1+-1*initData[,1]+2*initData[,3]+ 1.1*initData[,5]+1.2*initData[,7]
data <- data.frame( initData, initOutcome )

Initiation <- function(data, pSize=length(data[,1]), minC=0.1, maxC=0.9){

  vars <- length(data[1,])-1
  initMatrix<-matrix(data=NA, nrow = pSize, ncol = vars)
  prob<-runif(pSize,min=minC, max=maxC)
  for(i in 1:pSize)
    initMatrix[i,]<-sample(c(0,1), vars, replace = TRUE, c(1-prob[i],prob[i]))
  intercept<-sample(c(0,1),pSize,replace = TRUE)

  return(list("InitMatrix"<-initMatrix, "Intercept"<-intercept))
}

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

  # make X, Y
  X <- data[ , !names( data ) %in% outcome ]
  Y <- data[ , names( data ) %in% outcome ]


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

  # assign fitness probabilities to calculated AICs, and select (stochastically) parents to keep
  select_ind <- sample( order( AIC , decreasing = TRUE ) , P , prob = fitness_prob , replace = TRUE )
  parents_selection <- parents[ select_ind , ]
  intercept_selection <- intercept[ select_ind ]

  return( list( parents_selection = parents_selection , intercept_selection = intercept_selection )
}

crossover <-function(P1, P2){
  n = length(P1)
  ##Choose crossover site randomly
  site = floor(runif(1,min = 1, max = n-1))
  ##Cut&Ligate parent strings
  P3 = c(P1[1:site], P2[(site+1):n])
  P4 = c(P1[(site+1):n], P2[1:site])
  ##Perhaps return all four strings??
  return(list(P3 = P3, P4=P4))
}

mutation <-function(P,mutationProb){

  n = length(P)

  # Identifying which sites should be mutated
  mut <- which(runif(n, 0, 1)<mutationProb)

  P[mut] <- ifelse(P[mut]==1,0,1)
  return(P)
}

select <- function(data, model, fitness){

  # Initialize the first generation
  starting <- Initiation(data)

  convergenceCriterion = 10e-8 # what should be the convergence criterion?

  while (cvg > convergenceCriterion){

    #select and breed
    children <- selection(data = data, outcome = "initOutcome", parents = starting[[ 1 ]] , intercept = starting[[ 2 ]])

    # oldAIC =

    # crossover
    for (i in seq(from=1, to = length(children[,1]), by = 2)){
      tmp = crossover(children[i,],children[i+1,])
      children[i,] = tmp$P3
      children[i+1,] = tmp$P4
      print(i)
    }

    # mutation
    children <- apply(children,1,mutation,mutationProb = 0.01)

    # cvg = min(AIC(lm(,children))) - oldAIC

    # New parents
    parents <- children

  }

  # Returning the selected variables
  return(dataset[1,which(children[1,]==1)])

}
