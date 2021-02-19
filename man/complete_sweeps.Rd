\name{complete_sweeps}
\alias{complete_sweeps}

\title{
complete_sweeps
}
\description{
Completes existing sweeps by interpolation so that the sweeps all have a complete set of x-values. This makes graphing and testing at specific x-values easier, but one needs to bear in mind that this function works with interpolation
}
\usage{
complete_sweeps(theSweeps,abscissa="tau_Pa",sweep_identifier_column="file",columns_to_complete=c("Gprime_Pa","Gprimeprime_Pa","Gamma_in_percent"),scale_with_abscissa=c("Gamma_in_percent"))
}

\arguments{
  \item{theSweeps}{
Rheological sweeps in R \link{data.frame} format, that is concatened in a long table with a column as indicated by the argument \code{sweep_identifier_column} where the individual sweeps are uniquely identified. In addition, this argument needs to contain at least: the \code{abscissa} column, the columns enumarated in the \code{columns_to_complete} argument.
}
\item{abscissa}{x-values; this is often, but not necessarily, the controlled parameter during the sweep, as for example the shear stress "tau_Pa".}
\item{columns_to_complete}{List of columns which should be interpolated so that for all individual sweeps, values at the entire collection of x-values are available.}
\items{scale_with_abscissa}{For extrapolation at low x-values (i.e. below the minimum value of the sweep at hand): for columns listed in \code{scale_with_abscissa}, proportionality with the 
x-values is assumed, and extrapolation is from the value at the minimum x-value for the sweep concerned by proportionality with the x-values. Use this only if you are sure from a priori knowledge that proportionality to the abscissa value is better than linear extrapolation in estimation of the missing y-values.}
}

\value{
A \link{data.frame} with the same columns as \code{theSweeps} but potentially more lines if it was necessary to complete the dataframe to have entries for all the x-values for all the sweeps.}



\author{
Thomas Braschler
}

\keyword{ misc }

