\name{evaluateYield}
\alias{evaluateYield}

\title{
evaluateYield
}
\description{
Evaluates the yield point (stress and strain) from oscillatory rheology sweeps (stress or strain sweep). This is done by finding the intersection point between the G' and G'' curves, 
based on the predominantly solid nature when G'>G'' and inversely predominantly liquid nature when G'<G'' (Schramm, G. A, 1994)
}
\usage{
evaluateYield(sweep, Gprime_column = "Gprime_Pa", Gprimeprime_column = "Gprimeprime_Pa", 
    abscissa = "tau_Pa", additional_columns_to_be_interpolated = "Gamma_in_percent")
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
A vector of two entries describing the strain and stress of the yield point, and additional characteristics when \code{additional_columns_to_be_interpolated} is not empty.}



\author{
Thomas Braschler
}

\examples{

# Loading and plotting of the data
data(rheology_sampleData)
# Evaluation in linear scale
yieldPointLinear=evaluateYield(rheology_sampleData,Gprime_column="Gprime_Pa",Gprimeprime_column="Gprimeprime_Pa",abscissa="tau_Pa")
yieldPointLinear

#Evaluation in log scale
rheology_sampleData$log_Gprime_Pa = log(rheology_sampleData$Gprime_Pa) 
rheology_sampleData$log_Gprimeprime_Pa = log(rheology_sampleData$Gprimeprime_Pa)
rheology_sampleData$log_tau_Pa = log(rheology_sampleData$tau_Pa)
 
yieldPointLog=evaluateYield(rheology_sampleData,Gprime_column="log_Gprime_Pa",Gprimeprime_column="log_Gprimeprime_Pa",abscissa="log_tau_Pa")
yieldPointLog["Gprime_Pa"] = exp(yieldPointLog["log_Gprime_Pa"])
yieldPointLog["tau_Pa"] = exp(yieldPointLog["log_tau_Pa"])

#In log scale, the yield point is the intersection on the log-log G'-shear plot
plot(Gprime_Pa ~ tau_Pa, rheology_sampleData,log="xy",type="b",main="rheology_sampleData")
lines(Gprimeprime_Pa ~ tau_Pa, rheology_sampleData,type="l")
lines(yieldPointLog["tau_Pa"],yieldPointLog["Gprime_Pa"],type="p",pch=21,cex=1.5,col="red",bg="red")
legend("bottomleft",legend=c("Elastic modulus G'","Viscous modulus G''", "Yield point"),lty=c(-1,1,-1),pch=c(21,-1,21),pt.cex=c(1,1,1.5),col=c("black","black","red"), pt.bg=c("white","white","red"))

}

\references{
Schramm, G. A Practical Approach to Rheology and Rheometry. (Gebrueder HAAKE GmbH, 1994).}

\keyword{ misc }

