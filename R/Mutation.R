# From the Givens and Hoeting book:
#Mutation rates are typically very low, in the neighborhood of 1%. Theoretical
#work and empirical studies have supported a rate of 1/C [464],
# and another investigation suggested that the rate should be nearly proportional
# to 1/(P√C) [571]. Nevertheless, a fixed rate independent of P and C is
# a common choice.
mutation <-function(P,mutationProb){

  n = length(P)

  # Identifying which sites should be mutated
  mut <- which(runif(n, 0, 1)<mutationProb)

  P[mut] <- ifelse(P[mut]==1,0,1)
  return(P)
}

# test
P = sample(c(0,1),10,replace = TRUE)
result = mutation(P)
