\name{selectVis}
\alias{selectVis}

\title{Interactively select regions-of-interest of an imaging dataset}
\description{
Manually select multiple regions-of-interest or pixels on an imaging dataset in a shiny-based GUI. Similar to \code{\link{selectROI}}, the method has the same form as the \code{\link{image}} method for plotting imaging datasets.

The results are returned as a list of logical vectors indicating which pixels have been selected. The GUI also allows naming each region in the list. When working with multiple regions, the results can be returned as a factor indicating the region each pixel belongs to. 
}
\usage{
## Start GUI
selectVis(dataset, ..., mode = c("region", "pixels"))
}
\arguments{
	\item{dataset}{The name of an \code{\linkS4class{MSImagingExperiment}} object.}
	\item{\dots}{Addtional arguments to be passed to \code{\link{image}}}
	\item{mode}{What kind of selection to perform: 'region' to select region-of-interest, or 'pixels' to select individual pixels.}}
\value{
A list of logical vectors each of length equal to the number of pixels, for \code{Return as List}.

A factor of the same length as the passed logical vectors, for \code{Return as Factor}.
}
\author{
	Nischal Mahaveer Chand
}

\seealso{
	\code{\link{selectROI}}
}

\examples{
require(Cardinal)

\dontrun{
mse <- simulateImage(preset=1, npeaks=10, nruns=2, baseline=1)

# start GUI for interactive selection
selectVis(mse)

# start GUI, specify m/z, tolerance, and subset values
selectVis(mse, mz=1227, plusminus=0.5, subset=run(mse) == "run0")

# start GUI, specify formula 
selectVis(mse, mz=1227, formula="log1p(intensity) ~ x * y")

# start GUI, specify other image options
selectVis(mse, mz=1227, smooth.image="gaussian")
selectVis(mse, mz=1227, colorscale="magma")
selectVis(mse, mz=1227, fun=mean)
selectVis(mse, mz=1227, contrast.enhance="histogram")

}
}
