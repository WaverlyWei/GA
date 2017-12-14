library(testthat)

context("GA Algorithm work correctly for simulated data")
#simulated data:
##initData = matrix(rnorm(2500, sd = 1:5), ncol = 5, byrow = TRUE)
##initOutcome = 1+-1*initData[,1]+2*initData[,3]+ 1.1*initData[,5]
##data <- data.frame( initData, initOutcome )

model<- simulatedData$initOutcome~ X1+X2+X3+X4+X5
GAresults = select(simulatedData, model, step = 300)
variables = GAresults[[1]]
convergence = GAresults[[2]]
plot(convergence)

#Select
test_that("GA algorithm work correctly", {
  # check if the result from GA is same with the true value? 
  expect_equal(variables, c(1,2,4,6))
})

context("GA Algorithm work correctly for real data (white wine quality data)")
# real example: white wine quailty data
##whiteWine<-read.csv('data/winequality-white.csv',header=TRUE,sep=';')
##numCol <-dim(whiteWine)[2]
##quality <- whiteWine[,numCol]
##attrs <- matrix(unlist(whiteWine[,-numCol]), ncol=numCol-1, byrow=FALSE)
##wineData <- data.frame( attrs, quality )

wineModel<- log(whiteWineData$quality)~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11
GAresults = select(whiteWineData, wineModel, step = 200)
variables = GAresults[[1]]
convergence = GAresults[[2]]
plot(convergence)

context("Auxiliary function for GA Algorithm is tested")

context("Testing Initiation")
#Initiation
test_that("Initation returns correct result", {
  starting<-initiation(10, 50)
  #Check the size of data is equal to population size
  expect_equal(nrow(starting),50)
  #At least one gene has value of 1 in every chromosome
  expect_true(all(apply(starting,1,sum)>=1))
})

context("Testing Selection")
#Selection
parents<-matrix(NA, 3,6)
parents[1,]<-c(1, 1,0,1,0,1) # best AIC
parents[2,]<-c(1, 1,0,0,0,1)
parents[3,]<-c(1, 0,0,1,0,0)

mm <- model.matrix( model , data = simulatedData )

test_that("Selection returns correct result", {
  result<-selection( mm , model , parents, 3 )
  #Check the size of result data is equal to pSize
  expect_equal(nrow(result[[1]]),3)

    #The first parent which has best AIC, should be selected
  expectTrue<-FALSE
  i=1
  while(expectTrue==FALSE && i<=3){
    expectTrue<-all(result[[1]][i,]==parents[1,])
    i=i+1
  }
  expect_true(expectTrue)
})

context("Testing Crossover")
#Crossover
test_that("Crossover returns correct result", {
  P1 = sample(c(0,1),10,replace = TRUE)
  P2 = sample(c(0,1),10,replace = TRUE)
  result = crossover(P1,P2,10)
  #The length of children are equal to the length of parents
  expect_equal(length(result$P3), 10)
  expect_equal(length(result$P4), 10)
  #Childern are not equal to parents
  expect_equal(all(P1==result$P3), FALSE)
  expect_equal(all(P1==result$P3), FALSE)
  expect_equal(all(P1==result$P3), FALSE)
  expect_equal(all(P1==result$P3), FALSE)
})

context("Testing Mutation")
#Mutation
test_that("Mutation returns correct result", {
  P = sample(c(0,1),10,replace = TRUE)
  #If mutationProb=0, then no mutation occurs
  result = mutation(P,0,10)
  expect_equal(sum(abs(P-result)),0)
  ##If mutationProb=1, then mutation occurs in every gene
  result = mutation(P,1,10)
  expect_equal(sum(abs(P-result)),10)
  ##If mutationProb=0.5, then the number of total mutation is between 3 and 7? (make sense??) 
  result = mutation(P,0.5, 10)
  expect_true(sum(abs(P-result))%in%seq(2,8))
})

