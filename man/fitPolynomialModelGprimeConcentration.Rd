\name{fitPolynomialModelGprimeConcentration}
\alias{fitPolynomialModelGprimeConcentration}

\title{
fitPolynomialModelGprimeConcentration
}
\description{
Fits a polynomial model of the form \code{Gprime=A*(x-c0)^n} to experimental G' (Gprime) data. Here, x and Gprime are known (measured), while we try to find
A, c0 and n.
}
\usage{
fitPolynomialModelGprimeConcentration(Gprime,polymer_conc_mgmL,n=2.5, do_full_fit=TRUE)
}

\arguments{
  \item{Gprime}{
Measured G' values
}
\item{polymer_conc_mgmL}{Known concentration of the suspension, in terms of polymer concentration, typically in mg/mL}


 \item{n}{Initial guess for the exponent in the polynomial model}

  \item{do_full_fit}{
At the end of the fitting procedure, do you want to attempt to fit simultaneously the three parameters \code{A}, \code{c0} and \code{n} after initial fitting? This can provide the best fit, but
often does not converge.
}

}

\value{
List with named entries for \code{A}, \code{n} and \code{c0}
}


\details{Carries out least squares fitting by using \link{nls}; depending on the data and starting values, this can be finicky. Check the values that you obtain.}



\author{
Thomas Braschler
}

\keyword{ misc }

\examples{
x=c(30,35,40,45,50)
Gprime=c(10,400,1500,5000,11000)
plot(x,Gprime,log="y",xlab="Polymer concentration [mg/mL]",ylab="G'")
coeffs=fitPolynomialModelGprimeConcentration(Gprime=Gprime,polymer_conc_mgmL=x,n=2.5, do_full_fit=FALSE)
x_theory=seq(from=0,to=50, length.out=200)
lines(x_theory,polynomial_model(x=x_theory,A=coeffs[["A"]],c0=coeffs[["c0"]],n=coeffs[["n"]]))

}

