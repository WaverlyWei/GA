library(testthat)

context("GA Algorithm")

#Select
test_that("Input validation", {
  #Number of argument
  #
})

X <- matrix(rnorm(5000, sd = 1:5), ncol = 10, byrow = TRUE)
Y<-1+-1*X[,1]+2*X[,3]+ 1.1*X[,5]+1.2*X[,7]
data <- data.frame( X , Y )

test_that("GA algorithm work correctly", {
  # check if the result from GA is same with the true value? 

})

#Initiation
test_that("Input validation for Initiation", {  
  expect_equal(Initiation(data, 10, 0.1, 1.2), "minC and maxC should be the value between 0 and 1")  
  expect_equal(Initiation(data, 10, 0.5, 0.4), "minC cannot be greater than maxC")  
})

test_that("Initation returns correct result", {
  starting<-Initiation(data, 10,0.01,0.99)
  #Check the size of data is equal to pSize
  expect_equal(nrow(starting[[1]]),10)
  expect_equal(length(starting[[2]]),10)
  #At least one gene has value of 1 in every chromosome
  expect_true(all(apply(starting[[1]],1,sum)>=1))
})

#Selection

parents<-matrix(NA, 3,10)
parents[1,]<-c(1,0,1,0,1,0,1,0,0,0) # best AIC
parents[2,]<-c(1,0,0,0,1,0,0,0,0,0)
parents[3,]<-c(0,0,1,0,0,1,0,0,0,0)
intercept<-c(1,0,1)

test_that("Input validation for selection", {
  # Invaid user fitness function return error. 
  GHFitness <- function( P ){ return(c(0.1, 0.7, 0.2)) }
  expect_error(selection(data,outcome="initOutcome", parents=parents, intercept=intercept, fitness=GHFitness))
  
  GHFitness <- function( P ){ return(c(0.1, 0.8, 1.2)) }
  expect_error(selection(data,outcome="initOutcome", parents=parents, intercept=intercept, fitness=GHFitness))
})

test_that("Selection returns correct result", {
  GHFitness <- function( P ){ 2 / P / ( P + 1 ) * seq( 1:P ) }
  result<-selection(data,outcome="initOutcome", parents=parents, intercept=intercept, fitness=GHFitness)
  #Check the size of result data is equal to pSize
  expect_equal(nrow(result[[1]]),3)
  expect_equal(length(result[[2]]),3)
  #The first parent which has best AIC, should be selected
  expectTrue<-FALSE
  i=1
  while(expectTrue==FALSE && i<=3){
    expectTrue<-all(result[[1]][i,]==parents[1,])
    i=i+1
  }
  expect_true(expectTrue)
})

#Crossover
test_that("Crossover returns correct result", {
  P1 = sample(c(0,1),11,replace = TRUE)
  P2 = sample(c(0,1),11,replace = TRUE)
  result = crossover(P1,P2)
  #The length of children are equal to the length of parents
  expect_equal(length(result$P3), 11)
  expect_equal(length(result$P4), 11)
  #Childern are not equal to parents
  expect_equal(all(P1==result$P3), FALSE)
  expect_equal(all(P1==result$P3), FALSE)
  expect_equal(all(P1==result$P3), FALSE)
  expect_equal(all(P1==result$P3), FALSE)
})

#Mutation
test_that("Mutation returns correct result", {
  P = sample(c(0,1),10,replace = TRUE)
  #If mutationProb=0, then no mutation occurs
  result = mutation(P,0)
  expect_equal(sum(abs(P-result)),0)
  ##If mutationProb=1, then mutation occurs in every gene
  result = mutation(P,1)
  expect_equal(sum(abs(P-result)),10)
  ##If mutationProb=0.5, then the number of total mutation is between 3 and 7? (make sense??) 
  result = mutation(P,0.5)
  expect_true(sum(abs(P-result))%in%seq(3,7))
})
