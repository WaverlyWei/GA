##Not sure what form of datastrucutre would be passed from selection
##Add intermediate helper function later
##Input: Two parent strings, in the form of binary vector
##Ouput: Crossovered strings P3, P4
crossover <-function(P1, P2){
  n = length(P1)
  ##Choose crossover site randomly
  site = floor(runif(1,min = 1, max = n-1))
  ##Cut&Ligate parent strings
  P3 = c(P1[1:site], P2[site:n])
  P4 = c(P1[site:n], P2[1:site])
  ##Perhaps return all four strings??
  return(list(P3 = P3, P4=P4))
}

#Simple test case:
P1 = sample(c(0,1),11,replace = TRUE)
P2 = sample(c(0,1),11,replace = TRUE)
result = crossover(P1,P2)
#P1
#[1] 0 1 1 0 0 0 0 1 1 1 1
#P2
#[1] 1 1 1 1 1 1 0 0 1 1 1
#$P3
#[1] 1 1 0 1 1 1 1 0 0 1 1 1
#$P4
#[1] 0 0 0 0 0 1 0 1 1 1 1 1

##Next: Add iterations / correct format/ Pass onto Mutation function
