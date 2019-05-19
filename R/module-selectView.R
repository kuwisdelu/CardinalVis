
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
  
  plot_image <- reactive({ image(data()) })
  
  ret <- reactiveValues(roi = NULL)
  
  output$selectROIView <- renderPlot({
    plot_image()
  })
  
  observeEvent(input$plot_click, {
    clicks$x <- c(clicks$x, input$plot_click$x)
    clicks$y <- c(clicks$y, input$plot_click$y)
  })
  
  observeEvent(input$button_select, {

      roi <- reactive({ .selectRegion(clicks, pixelData(data()),
                subset = plot_image()$subset, 
                axs = plot_image()$coordnames) })
      
      ret[["roi"]] <- roi()
      
  })
  
  return(reactive({ ret[["roi"]] }))
  
}