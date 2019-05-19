
selectView <- function(input, output, session, dataset) {
  
  clicks <- reactiveValues(x = c(), y = c())
  
  data <- reactive({
		tryCatch(get(dataset, envir=globalenv()),
			error=function(e) NULL)
	})
  
  output$selectROIView <- renderPlot({
    image(data())
    #browser()
    #plot(1:5, 1:5)
    #if (!is.null(clicks$x))
    #  lines(clicks$x, clicks$y)
  })
  
  observeEvent(input$plot_click, {
    clicks$x <- c(clicks$x, input$plot_click$x)
    clicks$y <- c(clicks$y, input$plot_click$y)
  })
  
}