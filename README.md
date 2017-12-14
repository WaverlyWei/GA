
<!-- README.md is generated from README.Rmd. Please edit that file -->



# GA

GA provides functions to implement the genetic algorithm for variable selection in regression problems.

## Installation

You can install GA from github with:


```r
# install.packages("devtools")
devtools::install_github("WaverlyWei/GA")
#> Skipping install of 'GA' from a github remote, the SHA1 (f9cb45cc) has not changed since last install.
#>   Use `force = TRUE` to force installation
```

## Example

This is a basic example which shows you how to solve a common problem:


```r
library(GA)

# simulate data
initData <- matrix( rnorm( 5000 , sd = 1:5 ) , ncol = 10 , byrow = TRUE )
initOutcome <-1 + -1 * initData[ , 1 ] + 2 * initData[ , 3 ] + 1.1 * initData[ , 5 ] + 2.7 * initData[ , 3] * initData[ , 5 ]
dataSet <- data.frame( initData , initOutcome )

## not run
# call select function
# GAresults <- select( data = dataSet , model = dataSet$initOutcome ~ X1 + X3 + X5 + X3:X5 + X7 + X9)

## not run
# plot convergence results
# plot( GAresults[[ 2 ]] , pch = 16 , cex = 0.75 , xlab = "Step" , ylab = "Convergene Criterion")
```
