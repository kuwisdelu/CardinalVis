
selectViewUI <- function(id) {
  ns <- NS(id)
 
  ## UI elements
 
  tagList(
    plotOutput(ns("selectROIView"), 
             click = ns("plot_click")),
    actionButton(ns("button_done"), "Done"),
    verbatimTextOutput(ns("info"))
  )
}