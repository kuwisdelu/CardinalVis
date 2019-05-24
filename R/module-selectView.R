
selectView <- function(input, output, session, dataset) {
  
  clicks <- reactiveValues(x = c(), y = c())
  
  
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

      roi <- reactive({
        Cardinal:::.selectRegion(clicks, pixelData(data()),
                subset = plot_image()$subset, 
                axs = plot_image()$coordnames) })
      
      ret[["roi"]] <- roi()
      
  })
  
  return(reactive({ ret[["roi"]] }))
  
}