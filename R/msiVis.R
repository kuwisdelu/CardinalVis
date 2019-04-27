
msiVis <- function(dataset) {

	if ( is.symbol(substitute(dataset)) )
		dataset <- deparse(substitute(dataset))
	
	data <- try(get(dataset, envir=globalenv()), silent=TRUE)

	if ( inherits(data, get_supported_classes()) ) {
		shinyApp(ui=msiUI(), server=msiServer(dataset))
	} else {
		shinyApp(ui=msiUI(), server=msiServer(NULL))
	}

}

