initData <- matrix( rnorm( 5000 , sd = 1:5 ) , ncol = 10 , byrow = TRUE )
initOutcome <-1 + -1 * initData[ , 1 ] + 2 * initData[ , 3 ] + 1.1 * initData[ , 5 ] + 1.2 * initData[ , 7 ]
dataSet <- data.frame( initData , initOutcome )

initiation <- function( C , P ){

  init_parents <- matrix( NA , nrow = P , ncol = C )
  prob <- runif( P , min = 0.1 , max = 0.9 )
  for( i in 1:P ){
    init_parents[ i , ] <- sample( c( 0 , 1 ) , C , replace = TRUE , c( 1 - prob[ i ] , prob[ i ] ) )
  }

  return( init_parents )
}

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

crossover <- function( P1 , P2 , C ){

  # Choose crossover site randomly
  site = floor( runif( 1 , min = 1 , max = C - 1 ) )

  # Cut&Ligate parent strings
  P3 = c( P1[ 1:site ] , P2[ ( site + 1 ):C ] )
  P4 = c( P1[ ( site + 1 ):C ] , P2[ 1:site ] )

  ##Perhaps return all four strings??
  return( list( P3 = P3 , P4 = P4 ) )
}

mutation <-function( P_i , mutationProb , C ){

  # Identifying which sites should be mutated
  mut <- which( runif( C , 0 , 1 ) < mutationProb )

  P_i[ mut ] <- ifelse( P_i[ mut ] == 1 , 0 , 1 )

  return( P_i )

}

select <- function( data , model , conv_criterion = 10e-8 , steps = 50 ){

  mm <- model.matrix( model , data = data )

  # set C the length of each individual chromosome ,ie the number of vars
  C <- length( attr( mm , "assign" ) )

  # Randomly select P, the number of parents per generation, such that C < P < 2C with increasing probability for larger P
  tmp <- seq( from = 2 * round( ( C + 1 ) / 2 ) , to = 2 * C , by = 2 )
  P <- sample( tmp , 1 , prob = ( 2 / C ^ 2 ) * tmp )

  # Initialize the first generation
  init_parents <- initiation( C = C , P = P )


  # initialize convergence vector
  convergence <- rep( 0 , steps )

  # select fittest parents to breed
  tmp <- selection( mm = mm , model = model , parents = init_parents , P = P )
  parents <- tmp$children
  parent_AIC <- tmp$minAIC

  # print update
  cat( "step 0" , colnames( mm )[ which( parents[ 1 , ] == 1 ) ] , "\n" , sep = " ")

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
    children <- apply( children , 2 , mutation , mutationProb = 0.01 , C = C )

    # run regression with next generation
    tmp <- selection( mm = mm , model = model , parents = children , P = P )
    children <- tmp$children
    children_AIC <- tmp$minAIC

    # print update
    cat( paste0( "step" , i ) , colnames( mm )[ which( children[ 1 , ] == 1 ) ] , "\n" , sep = " ")

    # calculate convergenc criterion
    convergence[ i ] <- abs(children_AIC - parent_AIC)

    # New parents
    parents <- children
    parent_AIC <- children_AIC

  }

  # Returning the selected variables
  return( list( which( children[ 1 , ] == 1 ), convergence ) )

}

system.time( GAresults <- select( data = dataSet , model = initOutcome ~ X1:X2 + X1:X3 + X4 + X5 + X6 + X7 + X8 + X9 + X10 ) )
variables = GAresults[[1]]
convergence = GAresults[[2]]
variables
convergence
plot(convergence , pch = 16 , cex = 0.75 , xlab = "Step" , ylab = "Convergene Criterion")


