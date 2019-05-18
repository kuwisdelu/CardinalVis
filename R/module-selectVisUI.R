
selectVisUI <- function(id) {
  ns <- NS(id)
 
  ## UI elements
 
  tagList(
    plotOutput(ns("selectROIView"), 
             click = "plot_click"),
    verbatimTextOutput(ns("info"))
  )
}