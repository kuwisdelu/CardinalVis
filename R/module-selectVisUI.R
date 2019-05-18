
selectVisUI <- function(id) {
  ns <- NS(id)
 
  ## UI elements
 
  tagList(
    plotOutput(ns("selectROIView"), 
             click = ns("plot_click")),
    verbatimTextOutput(ns("info"))
  )
}