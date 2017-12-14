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
