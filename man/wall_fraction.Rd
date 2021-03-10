\name{wall_fraction}
\alias{wall_fraction}

\title{
wall_fraction
}
\description{
Function to estimate the wall fraction from the polymer concentration. Needs two constants, the
wall concentration and the osmotic coefficient gamma
}
\usage{
wall_fraction(polymer_concentration=10,wall_concentration=70,gamma=0.15)
}

\arguments{
  \item{polymer_concentration}{
Polymer concentration in mg/mL
}
\item{wall_concentration}{Nominal wall concentration}


 \item{gamma}{Compressibility constant of the walls}

}

\value{
Vector of the same length as \code{polymer_concentration}, indicating the theoretical wall fraction with the coefficients \code{wall_fraction} and \code{gamma}
}



\author{
Thomas Braschler
}

\references{ Pellet, C. and M. Cloitre, The glass and jamming transitions of soft polyelectrolyte microgel suspensions. Soft Matter, 2016. 12(16): p. 3710-20,
see also Supplementary 8 of the manuscript "An injectable meta-biomaterial", particularly for values of the wall concentration and gamma fraction}

\keyword{ misc }

\examples{
conc=0:200
uncompressed_wall_concentration=70 # uncompressed wall concentration spontaneously adapted when the polymer particles are not under force
plot(conc,wall_fraction(conc,wall_concentration=uncompressed_wall_concentration),xlab="Total polymer concentration [mg/mL]",ylab="Space fraction occupied by solid material",main="Theoretical wall fraction estimation")
lines(c(0,200),c(0,200)/uncompressed_wall_concentration,type="l",col="red")
legend("bottomright",legend=c("Pellet & Cloitre","uncompressed wall material"),lty=c(1,1),pch=c(1,-1),col=c("black","red"))
}

