
selectVis <- function(dataset) {
  
	if ( is.symbol(substitute(dataset)) )
		dataset <- deparse(substitute(dataset))
	
	data <- try(get(dataset, envir=globalenv()), silent=TRUE)

	#if ( inherits(data, get_supported_classes()) ) {
	selectedROI <- runApp(list(ui=selectUI(), server=selectServer(dataset)))
	return(selectedROI)
	#} else {
#		shinyApp(ui=selectViewUI(), server=selectView(NULL))
	#}
  
}