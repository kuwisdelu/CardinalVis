
selectVis <- function(input, output, session, dataset) {
  
  clicks <- reactiveValues(x = c(), y = c())
  
  output$selectROIView <- renderPlot({
    plot(1:10, 1:10)
    if (!is.null(clicks$x))
      lines(clicks$x, clicks$y)
  })
  
  observeEvent(input$plot_click, {
    clicks$x <- c(clicks$x, input$plot_click$x)
    clicks$y <- c(clicks$y, input$plot_click$y)
  })
  
}