
selectViewUI <- function(id) {
  ns <- NS(id)
 
  ## UI elements
 
  tagList(
    plotOutput(ns("selectROIView"), 
             click = ns("plot_click")),
    fluidRow(
      actionButton(ns("button_select"), "Select"),
      actionButton(ns("button_done"), "Done"))
  )
}