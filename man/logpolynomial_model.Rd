\name{logpolynomial_model}
\alias{logpolynomial_model}

\title{
logpolynomial_model
}
\description{
Evaluation of a polynomial model with an offset (\link{polynomial_model}) for the G' values; returns the log value for x>c0 and penalty negative value for x<=c0. The idea is that if
the polymer concentration is smaller than the critical concentration c0, the suspension is liquid and
we return G'=0, otherwise it's a polynomial function of x-c0 with exponent n and coefficient A. The logarithmic form is for fitting purposes
}
\usage{
logpolynomial_model(x, A, c0, n,penalty_factor_below_offset=10)
}

\arguments{
  \item{x}{
The variable for the polynomial model
}
\item{A}{Proportionality constant for the polynomial relation}


 \item{c0}{Offset for the polynomial model}

  \item{n}{
Exponent for the polynomial relation
}
\item{penalty_factor_below_offset}{Factor for being able to return a value even when x is below c0}

}

\value{
log(A*(x-c0)^n) for x>c0, penalty_factor_below_offset*(A*(-x-c0)^n) for x<c0}



\author{
Thomas Braschler
}

\keyword{ misc }

