\name{GA}
\alias{GA}
\title{Genetic Algorithm for Variable Selection}
\description{
  A genetic algorithm for variable selection in regression problems.
}
\usage{
  select(dataset, model, conv_criterion, steps)
}
\arguments{
  \item{dataset}{The input data in the form of a dataframe. Each row is one entry with columns as different variables and the last column as the outcome.}
  \item{model}{Regression model formula.}
  \item{conv_criterion}{Convergence Criterion}
  \item{steps}{Maximum number of steps to run GA}
}\examples{
  select()
}
