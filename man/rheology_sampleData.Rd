\name{rheology_sampleData}
\alias{rheology_sampleData}
\docType{data}
\title{
Sample rheological stress sweep
}
\description{
Sample rheological stress sweep, obtained on a commercial sample of Restylane Lidocaine, on a Rheostress 100 instrument, plate-plate geometry, 0.2Hz oscillatory sweep. We used a pre-conditioning step (increase of shear stress at low level followed by decrease), which is removed from the actual sweep
}
\usage{data(rheology_sampleData)}
\format{
  Constains a single variable \code{rheology_sampleData}:
  \describe{
    \item{\code{rheology_sampleData}}{\link{data.frame} with 8 columns: \code{tau_Pa} indicates the shear stress, \code{Gprimeprime_Pa} the viscous modulus G'', \code{Gprime_Pa} the elastic modulus G', \code{Phase_angle_degrees} the phase angle determined by G' and G'', \code{Gamma_in_percent} the shear deformation in percent, \code{f_Hz} the excitation frequency in Hz, \code{t_s} the time from the beginning of the experiment, and \code{t_seg_s} the time in the current acquisition segment.}
    }

\examples{

# Generation of the data
path=system.file("sampleData",package="rheologyEvaluation")
file="Restylane_syringe_1.txt"
colname_info=
	list("tau_Pa"=c("Tau.en.Pa","X..en.Pa"),
	"Gprimeprime_Pa"=c("G..en.Pa"),
	"Gprime_Pa"=c("X.G...en.Pa.","G..en.Pa.1"),
	"Phase_angle_degrees"=c("X..en...1","Da.en.."),
	"Gamma_in_percent"=c("X..en..","Ga.en.."),
	"f_Hz"=c("f.en.Hz"),
	"t_s"=c("t.en.s"),
	"t_seg_s"="t_seg.en.s")
rheology_sampleData=read_Haake_Rheowin_text_file(file=paste(path,file,sep="/"),colname_info=colname_info,header=TRUE,skip=6,sep=c(";"),comment.char="",blank.lines.skip=FALSE,quote="")

# There is a preconditioning step consisting in ramping up and down the applied shear stress in this sample, remove to obtain a single sweep curve. This is the first 12 points
rheology_sampleData=rheology_sampleData[13:length(rheology_sampleData$tau_Pa),]


# Loading and plotting of the data
load(rheology_sampleData)



}

# ===========================================================
# Loading of the data and elementary plotting
data(rheology_sampleData)
plot(rheology_sampleData[[1]]$Distance, rheology_sampleData[[1]]$pressure/1e3,type="l",xlab="Distance [mm]",ylab="Pressure [kPa]",xlim=c(0,3),ylim=c(0,10))

for(ind in 2:length(rheology_sampleData))
{
lines(rheology_sampleData[[ind]]$Distance, rheology_sampleData[[ind]]$pressure/1e3,type="l",col=palette()[ind])
}

lines(CMC_concentration_blank$Distance,CMC_concentration_blank$pressure/1e3,type="l",col=palette()[length(rheology_sampleData)+1])

legend("topleft",legend=c(paste("c(CMC) = ",round(CMC_concentration_file_info$CMC_conc*100)/10, " mg/mL",sep=""),"blank (no gel)"),lty=rep(1,length(rheology_sampleData)+1),col=palette()[1:(length(rheology_sampleData)+1)])

}
\keyword{datasets}
