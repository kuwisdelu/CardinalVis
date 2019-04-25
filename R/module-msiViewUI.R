
msiViewUI <- function(id, label = "Data Viewer") {
	ns <- NS(id)
	verticalLayout(
		actionButton(ns("close"), "Close",
			style="float:right; padding:0px 2px 0px 2px; font-size:90%;"),
		uiOutput(ns("msiView"))
	)
}
