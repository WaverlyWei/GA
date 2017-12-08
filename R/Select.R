# Stat243 Final Project
# Team Members: Mary Combs, Heejung Kim, Mohammad Soheilypour, Waverly Wei

# Input: dataset, model, fitness function

select <- function(dataset, model, fitness){

  # Initialize the first generation
  parents <- initialize(dataset)

  convergenceFactor = 0.1 # what should be the convergence criterion?

  (while cvg > convergenceFactor){

    #select and breed
    children <- select(parents)

    # should we do a for loop here over children or in the crossover and mutation functions?
    # perform crossover and mutation on children
    children <- crossover(children)
    children <- mutation(children)

    parents <- children
    # calculate cvg
  }

}
