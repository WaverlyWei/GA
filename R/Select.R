# Stat243 Final Project
# Team Members: Mary Combs, Heejung Kim, Mohammad Soheilypour, Waverly Wei

# Input: dataset, model, fitness function

select <- function(dataset, model, fitness){
  
  # Initialize the first generation
  parents <- initiation(dataset)
  
  convergenceCriterion = 10e-8 # what should be the convergence criterion?
  
  while (cvg > convergenceCriterion){
    
    #select and breed
    children <- selection(parents)
    
    # oldAIC = 
    
    # crossover
    for (i in length(children)){
      tmp = crossover(children[i,],children[i+1,])
      children[i,] = tmp$P3
      children[i+1,] = tmp$P4
      i+1
    }
    
    # mutation
    children <- apply(children,1,mutation)
    
    # cvg = min(AIC(lm(,children))) - oldAIC
    
    # New parents
    parents <- children
  
  }
  
  # Returning the selected variables
  return(dataset[1,which(children[1,]==1)])
  
}