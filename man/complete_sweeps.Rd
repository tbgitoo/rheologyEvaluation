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
\item{scale_with_abscissa}{For extrapolation at low x-values (i.e. below the minimum value of the sweep at hand): for columns listed in \code{scale_with_abscissa}, proportionality with the 
x-values is assumed, and extrapolation is from the value at the minimum x-value for the sweep concerned by proportionality with the x-values. Use this only if you are sure from a priori knowledge that proportionality to the abscissa value is better than linear extrapolation in estimation of the missing y-values.}
}

\value{
A \link{data.frame} with the same columns as \code{theSweeps} but potentially more lines if it was necessary to complete the dataframe to have entries for all the x-values for all the sweeps.}

\details{This function anticipates typical rheology plots where the G'/G'' values either vary slowly on a logarithmic scale or show constant slopes in the extreme regions. This is why the interpolation is linear in the log-log plot. Use this function cautiously when the curves are following the general same trend but values at all available x-coordinates for all curves are required but not available through measurement. An example plots using some measured rather than imposed quantity on the x-axis, making it impossible to predict the actual x-values before measurement.}

\author{
Thomas Braschler
}

\examples{
theSweeps=data.frame(file=c(rep("A.txt",7),rep("B.txt",4),rep("C.txt",10)),tau_Pa=c(1,2,4,10,20,40,100,1,10,100,1000,0.1,0.4,1,4,10,40,100,200,400,1000),Gprime_Pa=c(1000,900,950,900,800,400,100,800,900,30,8,1200,1000,1100,800,700,300,200,50,40,5))
theSweepsCompleted = complete_sweeps(theSweeps,abscissa="tau_Pa",sweep_identifier_column="file",columns_to_complete=c("Gprime_Pa"),scale_with_abscissa=c(),main="complete_sweeps")
plot(Gprime_Pa ~ tau_Pa, theSweepsCompleted[theSweepsCompleted$file=="A.txt",],log="xy",type="b")
lines(Gprime_Pa ~ tau_Pa, theSweepsCompleted[theSweepsCompleted$file=="B.txt",],type="b",col="red")
lines(Gprime_Pa ~ tau_Pa, theSweepsCompleted[theSweepsCompleted$file=="C.txt",],type="b",col="green")
lines(Gprime_Pa ~ tau_Pa, theSweeps[theSweeps$file=="A.txt",],type="p",col="black",pch=21,bg="black")
lines(Gprime_Pa ~ tau_Pa, theSweeps[theSweeps$file=="B.txt",],type="p",col="red",pch=21,bg="red")
lines(Gprime_Pa ~ tau_Pa, theSweeps[theSweeps$file=="C.txt",],type="p",col="green",pch=21,bg="green")
legend("bottomleft",legend=c("A.txt","B.txt","C.txt", "Full symbols original data, empty interpolated"),pch=c(21,21,21,-1),col=c("black","red","green"))



}


\keyword{ misc }

