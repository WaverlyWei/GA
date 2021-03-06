% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Mutation.R
\name{mutation}
\alias{mutation}
\title{Mutation Function for Genetic Algorithm for Variable Selection}
\usage{
mutation(P_i, mutationProb, C)
}
\arguments{
\item{P_i}{The individual to be mutated}

\item{mutationProb}{Probability of mutation}

\item{C}{The number of independent variables to be selected from}
}
\description{
Implements genetic algorithm mutation by randomly selection, with low probability, some of the independent variables to be excluded in the next generation
}
\examples{

# define initial parameters
C <- 5

# initiate a generation
generation <- initiation( C = C , P = 4 )

# apply mutation function to each row in generation
mut_gen <- apply( generation , 1 , mutation , mutationProb = 0.01 , C = C )

}
\keyword{algorithm,}
\keyword{genetic}
\keyword{model}
\keyword{mutation}
\keyword{selection,}
