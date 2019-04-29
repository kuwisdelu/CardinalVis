\name{msiVis}
\alias{msiVis}

\title{Interactive visualization of MS imaging experiments}
\description{
	Open a shiny-based GUI for interactive visualization of \code{MSImagingExperiment} datasets.
}
\usage{
## Start GUI
msiVis(dataset)
}
\arguments{
	\item{dataset}{The name of an \code{\linkS4class{MSImagingExperiment}} object. Can be missing. If provided, opens a view into the dataset after starting the web app.}
}
\value{
	A \code{shinyApp} object.
}
\author{
	Kylie A. Bemis
}

\examples{
require(Cardinal)

\dontrun{
# start a new GUI
msiVis()
}

data <- simulateImage(preset=1, dim=c(10,10))

\dontrun{
# start GUI and open dataset
msiVis(data)
}
}

\keyword{iplot}
\keyword{dynamic}
