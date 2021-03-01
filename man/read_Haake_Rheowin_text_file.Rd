\name{read_Haake_Rheowin_text_file}
\alias{read_Haake_Rheowin_text_file}

\title{
read_Haake_Rheowin_text_file
}
\description{
Reads a text file expored from Rheowin into R
}
\usage{
read_Haake_Rheowin_text_file(file,use_C_locale=TRUE,colname_info=
	list("tau_Pa"=c("Tau.en.Pa","X..en.Pa"),
	"Gprime_Pa"=c("G..en.Pa"),
	"Gprimeprime_Pa"=c("X.G...en.Pa.","G..en.Pa.1"),
	"Phase_angle_degrees"=c("X..en...1","Da.en.."),
	"Gamma_in_percent"=c("X..en..","Ga.en.."),
	"f_Hz"=c("f.en.Hz"),
	"t_s"=c("t.en.s"),
	"t_seg_s"="t_seg.en.s"),...)
}

\arguments{
  \item{file}{
The path to the file to be read
}
\item{use_C_locale}{Boolean. If \code{TRUE}, forces to use a standard C locale to standardize encoding, which otherwise might be set differently by default on different systems. Uses \link{Sys.setlocale} internally for this purpose. This function makes an effort at setting the encoding back to your locale, although this may not work correctly in all circumstances.}
\item{colname_info}{List indicating how to handle the column names read from the Rheowin text file. The names of this list will be the output column names, while the elements indicates candidates to be looked for in the text file. They will be tried in order, if several are matching, the first match will be used.}
\item{...}{Arguments to be passed to \link{read.table}, used internally for reading the text. Particular attention needs to be payed to the lines to be skipped since there is typically a text block in text files exported from Rheowin in addition to the desired data.}

}

\value{
A dataframe with names identical to the names of the \code{colname_info} argument. If the corresponding column could not be found, NA entries are provided. 
}



\author{
Thomas Braschler
}

\examples{
# Access path for reading the sample file
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
plot(Gprime_Pa ~tau_Pa, rheology_sampleData,log="xy") # At the beginning, there is a pre-conditioning step, this has little influence on the general curve


}

\keyword{ misc }

