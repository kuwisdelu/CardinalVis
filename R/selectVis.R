
selectVis <- function(dataset, ..., mode = c("region", "pixels")) {
	
	## validate mode
	if ( length(mode) > 1 )
	  mode <- mode[1]
	if ( !(mode %in% c("region", "pixels")) ) {
	  print("Unknown mode, please use region or pixels")
	  return()
	}
	
	## get and validate dataset
	if ( is.symbol(substitute(dataset)) )
		dataset <- deparse(substitute(dataset))
	data <- try(get(dataset, envir=globalenv()), silent=TRUE)

	if ( inherits(data, get_supported_classes()) ) {
  	selectedROI <- runApp(list(ui=selectUI(), server=selectServer(dataset, ..., mode = mode)))
  	return(invisible(selectedROI))
	} else {
	  print("unsupported data type")
	}
  
}
