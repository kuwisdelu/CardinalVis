
selectView <- function(input, output, session, dataset, ...) {
  
  #### session variables ####
	
	ns <- session$ns

	data <- reactive({
		tryCatch(get(dataset, envir=globalenv()),
			error=function(e) NULL)
	})
	
	sv <- list(
		mz = syncVal(mz(data())[1], function(mz) {
			validate(need(mz, "invalid m/z value"))
			mz(data())[features(data(), mz=mz)]
		}),
		mz_tol = syncVal(0.001),
		xy = syncVal(unname(unlist(coord(data())[1,c(1,2)]))),
		xy_names = syncVal(names(coord(data()))[c(1,2)]),
		coord_names = syncVal(coordnames(data())),
		ionimage_xylim = syncVal(
			c(range(coord(data())[,1]),
				range(coord(data())[,2]))
		),
		spectrum_massrange = syncVal(range(mz(data()))),
		ionimage_intensity_range = syncVal(NULL),
		spectrum_intensity_range = syncVal(NULL),
		ionimage_contrast = syncVal("none"),
		ionimage_smoothing = syncVal("none"),
		ionimage_colorscale = syncVal("viridis"),
		ionimage_function = syncVal("mean"),
		spectrum_plotvar = syncVal(names(imageData(data()))[1]),
		ionimage_plotvar = syncVal(names(imageData(data()))[1]),
		mz_2 = syncVal(NA),
		mz_3 = syncVal(NA),
		xy_2 = syncVal(c(NA, NA)),
		xy_3 = syncVal(c(NA, NA)),
		closed = reactiveVal(FALSE)
	)

	## data ranges

	# mass range
	sv[["mz_range"]] <- reactive({
		range(mz(data()))
	})

	# coord range
	sv[["xy_range"]] <- reactive({
		c(range(coord(data())[,sv$xy_names()[1]]),
			range(coord(data())[,sv$xy_names()[2]]))
	})

	## subsetting

	# all possible subsets
	sv[["subset_choices"]] <- reactive({
		subs <- get_subset_possible(data(), sv$xy_names())
		get_subset_choices(subs)
	})

	# subset options
	sv[["subset"]] <- syncVal(sv$subset_choices()[1])

	# subset logical
	sv[["subset_logical"]] <- reactive({
		get_subset_logical(data(), sv$subset())
	})

	## pixels and features

	# pixel
	sv[["pixel"]] <- reactive({
		coord <- setNames(as.list(sv$xy()), sv$xy_names())
		if ( all(!is.na(sv$xy_2())) ) {
			coord[[1]] <- c(coord[[1]], sv$xy_2()[1])
			coord[[2]] <- c(coord[[2]], sv$xy_2()[2])
		}
		if ( all(!is.na(sv$xy_3())) ) {
			coord[[1]] <- c(coord[[1]], sv$xy_3()[1])
			coord[[2]] <- c(coord[[2]], sv$xy_3()[2])
		}
		subl <- sv$subset_logical()
		pixels(data(), coord=coord, subl, .env=environment())
	})

	# feature
	sv[["feature"]] <- reactive({
		mz <- sv$mz()
		if ( !is.na(sv$mz_2()) )
			mz <- c(mz, sv$mz_2())
		if ( !is.na(sv$mz_3()) )
			mz <- c(mz, sv$mz_3())
		features(data(), mz=mz)
	})

	## mcols variables

	# pixel mcols
	sv[["pixel_vars"]] <- reactive({
		c(names(imageData(data()))[1], names(pixelData(data())))
	})

	# feature mcols
	sv[["feature_vars"]] <- reactive({
		c(names(imageData(data()))[1], names(featureData(data())))
	})
	
	## return region of interest
	sv[["selected_roi"]] <- syncVal(NULL)
  
  clicks <- reactiveValues(x = c(), y = c())
  
  plot_image <- reactive({ image(data(), ...) })
  
  output$selectROIView <- renderPlot({
    print(plot_image())
    if ( all(!is.na(clicks)) ) {
      # add points at clicks
      points(clicks$x, clicks$y, pch=4, lwd=4, cex=2, col="black")
      # add solid lines to clicks
      lines(clicks$x, clicks$y, pch=4, lwd=4, cex=2, col="black")
      # add dashed line to show complete polygon
      lines(c(clicks$x[length(clicks$x)], clicks$x[1]), c(clicks$y[length(clicks$y)], clicks$y[1]),
            pch=4, lwd=4, cex=2, col="black", lty=2)
    }
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
      
      ## updates region of interest
      sv$selected_roi(roi())
      
  })
  
  return(sv$selected_roi)
  
}