
selectVis <- function(input, output, session, dataset) {
  
  output$selectROIView <- renderPlot({
    plot(1:10, 1:10)
  })
  
  output$info <- renderText({"hello world"})
  
}