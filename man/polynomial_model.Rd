\name{polynomial_model}
\alias{polynomial_model}

\title{
polynomial_model
}
\description{
Evaluation of a polynomial model with an offset for the G' values. The idea is that if
the polymer concentration is smaller than the critical concentration c0, the suspension is liquid and
we return G'=0, otherwise it's a polynomial function of x-c0 with exponent n and coefficient A
}
\usage{
polynomial_model(x, A, c0, n)
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

}

\value{
A*(x-c0)^n}



\author{
Thomas Braschler
}

\keyword{ misc }

