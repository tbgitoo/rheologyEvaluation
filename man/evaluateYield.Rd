\name{evaluateYield}
\alias{evaluateYield}

\title{
evaluateYield
}
\description{
Evaluates the yield point (stress and strain) from oscillatory rheology sweeps (stress or strain sweep)
}
\usage{
evaluateYield(sweep,Gprime_column="Gprime_Pa",Gprimeprime_column="Gprimeprime_Pa",abscissa="tau_Pa")
}

\arguments{
  \item{sweep}{
Sweep information, needs to contain at least the columns given by the \code{Gprime_column}, the 
\code{Gprimeprime_column} and the \code{abscissa} column
}
\item{Gprime_column}{Name of the column containing the G' values}


 \item{Gprimeprime_column}{Name of the column containing the G'' values}

  \item{abscissa}{
Name of the column containing the abscissa of the sweep (the variable that was sweeped to 
create the oscillatory sweep
}

}

\value{
A vector of two entries describing the strain and stress of the yield point}



\author{
Thomas Braschler
}

\keyword{ misc }

