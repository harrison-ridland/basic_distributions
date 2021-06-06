\name{sum_stat}
\alias{sum_stat}
\title{Gives summary statistics of a vector.}
\usage{
sum_stat(vec, confidence, guess = 0)
}
\arguments{
\item{vec}{An vecotr of integers or doubles}

\item{confidence}{A number between 0 and 1 }

\item{guess}{A double}

}
\value{
A data frame that lists mean, stanadrad deviation, lower bound, upper bound, and whether or not the guess is within the bounds
}
\description{
Gives the summary statistics of a vector in a usable form, so that the values can be easily accessed. 
}
