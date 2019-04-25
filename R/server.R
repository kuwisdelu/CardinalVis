
msiServer <- function(input, output, session) {

	#### Global variables and utility functions

	views <- list()

	validate_datatab <- function(id) {
		validate(need(id, "No tab selected"))
		validate(need(views[[id]], "Invalid data state"))
		id
	}

	zoom_from_massrange <- function(massrange) {
		id <- validate_datatab(input$datatabs)
		isolate(zoom <- diff(views[[id]]$mz_range()) / diff(massrange))
		100 * zoom
	}

	massrange_from_zoom <- function(zoom) {
		zoom <- zoom / 100
		id <- validate_datatab(input$datatabs)
		isolate(mz <- views[[id]]$mz())
		isolate(dmz <- abs(views[[id]]$mz_range() - mz) / zoom)
		c(mz - dmz[1], mz + dmz[2])
	}

	zoom_from_xylim <- function(xylim) {
		id <- validate_datatab(input$datatabs)
		isolate(xrange <- views[[id]]$xy_range()[c(1,2)])
		isolate(yrange <- views[[id]]$xy_range()[c(3,4)])
		xlim <- xylim[c(1,2)]
		ylim <- xylim[c(3,4)]
		zoom <- (diff(xrange) * diff(yrange)) / 
			(diff(xlim) * diff(ylim))
		100 * zoom
	}

	xylim_from_zoom <- function(zoom) {
		zoom <- zoom / 100
		id <- validate_datatab(input$datatabs)
		isolate(x <- isolate(views[[id]]$xy()[1]))
		isolate(y <- views[[id]]$xy()[2])
		isolate(xrange <- views[[id]]$xy_range()[c(1,2)])
		isolate(yrange <- views[[id]]$xy_range()[c(3,4)])
		dx <- abs(xrange - x) / sqrt(zoom)
		dy <- abs(yrange - y) / sqrt(zoom)
		c(x + c(-1, 1) * dx, y + c(-1, 1) * dy)
	}

	#### Data - tabs

	# open dataset in new tab
	observeEvent(input$open, {
		name <- input$dataset
		validate(need(name, "Invalid dataset"))
		if ( input$open > 0 ) {
			data <- tryCatch(get(name, envir=globalenv()),
				error = function(e) NULL)
			if ( is.null(data) ) return()
			try({
				id <- name
				loaded <- grepl(id, names(views))
				if ( any(loaded) )
					id <- paste0(id, "-", sum(loaded) + 1)
				appendTab("datatabs",
					tabPanel(id,
						msiViewUI(id)
					), select=TRUE)
				views[[id]] <<- callModule(msiView, id, name)
			}, silent=TRUE)
		}
	})

	# close tab
	observe({
		id <- validate_datatab(input$datatabs)
		if ( views[[id]]$closed() ) {
			removeTab("datatabs", id)
			views[[id]] <- NULL
		}
	})

	#### Data - UI

	# render dataset selector
	output$dataset <- renderUI({
		choices <- unlist(eapply(globalenv(), is, "MSImagingExperiment"))
		choices <- c(Choose="", sort(names(choices)[choices]))
		selectInput("dataset", NULL, choices=choices)
	})

	# update dataset input
	observe({
		id <- validate_datatab(input$datatabs)
		updateSelectInput(session, "dataset",
			selected=views[[id]]$name)
	})

	# update m/z ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateNumericInput(session, "mz",
			value=views[[id]]$mz())
	})

	# # update m/z tolerance ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateNumericInput(session, "mz_tol",
			value=views[[id]]$mz_tol())
	})

	# update x name ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateSelectInput(session, "x_name",
			choices=views[[id]]$coord_names(),
			selected=views[[id]]$xy_names()[1])
	})

	# update y name ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateSelectInput(session, "y_name",
			choices=views[[id]]$coord_names(),
			selected=views[[id]]$xy_names()[2])
	})

	# update x ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateNumericInput(session, "x",
			value=views[[id]]$xy()[1])
	})

	# update y ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateNumericInput(session, "y",
			value=views[[id]]$xy()[2])
	})

	# update ion image zoom ui
	observe({
		id <- validate_datatab(input$datatabs)
		xylim <- views[[id]]$ionimage_xylim()
		zoom <- zoom_from_xylim(xylim)
		updateNumericInput(session, "ionimage_zoom", value=zoom)
	})

	# update spectrum zoom ui
	observe({
		id <- validate_datatab(input$datatabs)
		massrange <- views[[id]]$spectrum_massrange()
		zoom <- zoom_from_massrange(massrange)
		updateNumericInput(session, "spectrum_zoom", value=zoom)
	})

	# update subset ui
	observe({
		id <- validate_datatab(input$datatabs)
		choices <- views[[id]]$subset_possible_expr()
		selected <- views[[id]]$subset()
		updateSelectInput(session, "subset",
			choices=choices, selected=selected)
	})

	# update contrast ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateSelectInput(session, "ionimage_contrast",
			selected=views[[id]]$ionimage_contrast())
	})

	# update smoothing ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateSelectInput(session, "ionimage_smoothing",
			selected=views[[id]]$ionimage_smoothing())
	})

	# update colorscale ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateSelectInput(session, "ionimage_colorscale",
			selected=views[[id]]$ionimage_colorscale())
	})

	# update intensity function ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateSelectInput(session, "ionimage_function",
			selected=views[[id]]$ionimage_function())
	})

	# update plot layout ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateSliderInput(session, "plot_layout",
			value=views[[id]]$plot_layout())
	})

	# update ion image height ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateNumericInput(session, "ionimage_height",
			value=views[[id]]$ionimage_height())
	})

	# update spectrum height ui
	observe({
		id <- validate_datatab(input$datatabs)
		updateNumericInput(session, "spectrum_height",
			value=views[[id]]$spectrum_height())
	})

	#### Data - reactive observers

	# change m/z val
	observeEvent(input$mz, {
		id <- validate_datatab(input$datatabs)
		views[[id]]$mz(input$mz)
	})

	# change m/z tolerance val
	observeEvent(input$mz_tol, {
		id <- validate_datatab(input$datatabs)
		views[[id]]$mz_tol(input$mz_tol)
	})

	# change x name val
	observeEvent(input$x_name, {
		id <- validate_datatab(input$datatabs)
		isolate(xy <- views[[id]]$xy_names())
		isolate(coord <- views[[id]]$coord_names())
		xy[1] <- input$x_name
		if ( xy[1] == xy[2] )
			xy[2] <- coord[-which(coord == xy[1])][1]
		views[[id]]$xy_names(xy)
	})

	# change y name val
	observeEvent(input$y_name, {
		id <- validate_datatab(input$datatabs)
		isolate(xy <- views[[id]]$xy_names())
		isolate(coord <- views[[id]]$coord_names())
		xy[2] <- input$y_name
		if ( xy[1] == xy[2] )
			xy[1] <- coord[-which(coord == xy[2])][1]
		views[[id]]$xy_names(xy)
	})

	# change x val
	observeEvent(input$x, {
		id <- validate_datatab(input$datatabs)
		isolate(pos <- views[[id]]$xy())
		pos[1] <- input$x
		views[[id]]$xy(pos)
	})

	# change y val
	observeEvent(input$y, {
		id <- validate_datatab(input$datatabs)
		isolate(pos <- views[[id]]$xy())
		pos[2] <- input$y
		views[[id]]$xy(pos)
	})

	# change ion image zoom val
	observeEvent(input$ionimage_zoom, {
		id <- validate_datatab(input$datatabs)
		cur_xylim <- views[[id]]$ionimage_xylim()
		cur_zoom <- zoom_from_xylim(cur_xylim)
		new_zoom <- input$ionimage_zoom
		if ( !isTRUE(all.equal(cur_zoom, new_zoom)) ) {
			new_xylim <- xylim_from_zoom(new_zoom)
			views[[id]]$ionimage_xylim(new_xylim)
		}
	})

	# change spectrum zoom val
	observeEvent(input$spectrum_zoom, {
		id <- validate_datatab(input$datatabs)
		cur_massrange <- views[[id]]$spectrum_massrange()
		cur_zoom <- zoom_from_massrange(cur_massrange)
		new_zoom <- input$spectrum_zoom
		if ( !isTRUE(all.equal(cur_zoom, new_zoom)) ) {
			new_massrange <- massrange_from_zoom(new_zoom)
			views[[id]]$spectrum_massrange(new_massrange)
		}
	})

	# change subset val
	observeEvent(input$subset, {
		id <- validate_datatab(input$datatabs)
		views[[id]]$subset(input$subset)
	})

	# change contrast val
	observeEvent(input$ionimage_contrast, {
		id <- validate_datatab(input$datatabs)
		views[[id]]$ionimage_contrast(input$ionimage_contrast)
	})

	# change smoothing val
	observeEvent(input$ionimage_smoothing, {
		id <- validate_datatab(input$datatabs)
		views[[id]]$ionimage_smoothing(input$ionimage_smoothing)
	})

	# change colorscale val
	observeEvent(input$ionimage_colorscale, {
		id <- validate_datatab(input$datatabs)
		views[[id]]$ionimage_colorscale(input$ionimage_colorscale)
	})

	# change function val
	observeEvent(input$ionimage_function, {
		id <- validate_datatab(input$datatabs)
		views[[id]]$ionimage_function(input$ionimage_function)
	})

	# change ion image intensity range
	observe({
		id <- validate_datatab(input$datatabs)
		if ( input$ionimage_autoscale ) {
			views[[id]]$ionimage_intensity_range(NULL)
		} else {
			zlim <- c(input$ionimage_intensity_min,
				input$ionimage_intensity_max)
			views[[id]]$ionimage_intensity_range(zlim)
		}
	})

	# change spectrum intensity range
	observe({
		id <- validate_datatab(input$datatabs)
		if ( input$spectrum_autoscale ) {
			views[[id]]$spectrum_intensity_range(NULL)
		} else {
			ylim <- c(input$spectrum_intensity_min,
				input$spectrum_intensity_max)
			views[[id]]$spectrum_intensity_range(ylim)
		}
	})

	# change plot layout
	observeEvent(input$plot_layout, {
		id <- validate_datatab(input$datatabs)
		views[[id]]$plot_layout(input$plot_layout)
	})

	# change ionimage height
	observeEvent(input$ionimage_height, {
		id <- validate_datatab(input$datatabs)
		views[[id]]$ionimage_height(input$ionimage_height)
	})

	# change spectrum height
	observeEvent(input$spectrum_height, {
		id <- validate_datatab(input$datatabs)
		views[[id]]$spectrum_height(input$spectrum_height)
	})

}
