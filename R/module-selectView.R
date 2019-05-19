
selectView <- function(input, output, session, dataset) {
  
  clicks <- reactiveValues(x = c(), y = c())
  
  
  .selectRegion <- function(loc, pdata, subset, axs = c("x", "y")) {
    roi <- rep(FALSE, nrow(pdata))
    coord <- coord(pdata)[subset,axs,drop=FALSE]
    selected <- sp::point.in.polygon(coord[,1], coord[,2], loc$x, loc$y) > 0
    roi[subset] <- selected
    roi
  }
  
  data <- reactive({
		tryCatch(get(dataset, envir=globalenv()),
			error=function(e) NULL)
	})
  
  output$selectROIView <- renderPlot({
    image(data())
  })
  
  observeEvent(input$plot_click, {
    clicks$x <- c(clicks$x, input$plot_click$x)
    clicks$y <- c(clicks$y, input$plot_click$y)
  })
  
  observeEvent(input$button_done, {

      roi <- .selectRegion(clicks, pixelData(data()),
                           subset = TRUE, axs = c("x", "y"))
      output$info <- renderText({
            paste0(sum(roi))
      }) 
      
      return({ roi })
  })
  
}