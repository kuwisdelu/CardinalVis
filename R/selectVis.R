
selectVis <- function(dataset, ..., mode = c("region", "pixels")) {
	
	## validate mode
  mode <- match.arg(mode)
  dots <- match.call(expand.dots = FALSE)$...
	
	## get and validate dataset
	if ( is.symbol(substitute(dataset)) )
		dataset <- deparse(substitute(dataset))
	data <- try(get(dataset, envir=globalenv()), silent=TRUE)

	if ( inherits(data, get_supported_classes()) ) {
  	selectedROI <- runApp(list(ui=selectUI(), 
  	                           server=selectServer(dataset, dots, mode = mode)))
  	return(invisible(selectedROI))
	} else {
	  message("unsupported data type")
	}
  
}
