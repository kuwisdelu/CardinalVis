
msiView <- function(input, output, session, dataset) {

	#### session variables ####
	
	ns <- session$ns

	data <- reactive({
		tryCatch(get(dataset, envir=globalenv()),
			error=function(e) NULL)
	})

	sv <- list(
		mz = syncVal(mz(data())[1], function(mz) {
			validate(need(mz, "Invalid m/z value"))
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
		plot_layout = syncVal(50), # 50%, 50%
		ionimage_height = syncVal(350), # 350px
		spectrum_height = syncVal(200), # 200px
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

	sv[["pixel"]] <- reactive({
		coord <- setNames(as.list(sv$xy()), sv$xy_names())
		subl <- sv$subset_logical()
		pixel <- pixels(data(), coord=coord, subl, .env=environment())
		if ( length(pixel) > 1L ) {
			warning("multiple pixels selected")
			pixel <- pixel[1]
		}
		pixel
	})

	sv[["feature"]] <- reactive({
		features(data(), mz=sv$mz())
	})

	# close view
	observeEvent(input$close, {
		if ( input$close > 0 )
			sv$closed(TRUE)
	})

	#### plot output ####

	output$spectrum_plot <- renderUI({
		validate(need(sv$spectrum_height(), "invalid plot height"))
		plotOutput(ns("spectrum"),
			click=clickOpts(id=ns("spectrum_click")),
			dblclick=dblclickOpts(id=ns("spectrum_dblclick")),
			brush=brushOpts(id=ns("spectrum_brush"),
				direction="x", resetOnNew=TRUE),
			height=paste0(sv$spectrum_height(), "px"))
	})

	output$ionimage_plot <- renderUI({
		validate(need(sv$ionimage_height(), "invalid plot height"))
		plotOutput(ns("ionimage"),
			click=clickOpts(id=ns("ionimage_click")),
			dblclick=dblclickOpts(id=ns("ionimage_dblclick")),
			brush=brushOpts(id=ns("ionimage_brush"),
				direction="xy", resetOnNew=TRUE),
			height=paste0(sv$ionimage_height(), "px"))
	})

	plot_null <- function(...) {
		par(mar=c(3,3,3,1), mgp=c(1.5,0.5,0),
			cex.axis=1, cex.lab=1)
		plot(0, 0, type='n', xlab="", ylab="",
			xaxt='n', yaxt='n')
		text(0, 0, "Nothing to plot.")
	}

	plot_spectrum <- reactive({
		validate(
			need(sv$xy(), "invalid x/y position"),
			need(sv$xy_names(), "invalid x/y names")
		)
		plot(data(), pixel=sv$pixel())
	})

	plot_ionimage <- reactive({
		validate(
			need(sv$mz(), "invalid m/z value"),
			need(sv$mz_tol(), "invalid m/z tolerance"),
			need(sv$xy_names(), "invalid x/y names")
		)
		colmap <- color.map(sv$ionimage_colorscale(), 100)
		fm <- paste0("~", paste0(sv$xy_names(), collapse="*"))
		image(data(),
			formula=as.formula(fm),
			feature=sv$feature(),
			plusminus=sv$mz_tol(),
			contrast.enhance=sv$ionimage_contrast(),
			smooth.image=sv$ionimage_smoothing(),
			fun=match.fun(sv$ionimage_function()),
			colorscale=colmap,
			subset=sv$subset_logical())
	})

	plot_mz_marker <- function() {
		mz <- c(sv$mz() - sv$mz_tol(), sv$mz() + sv$mz_tol())
		rect(mz[1], par("usr")[3], mz[2], par("usr")[4],
			col=rgb(1, 0, 0, 0, alpha=0.5), border=NA)
		abline(v=sv$mz(), lty=2, lwd=2, col="darkred")
	}

	plot_pos_marker <- function() {
		points(sv$xy()[1], sv$xy()[2], pch=4, lwd=4, cex=2, col="black")
		points(sv$xy()[1], sv$xy()[2], pch=4, lwd=2, cex=2, col="white")
	}

	#### plot reactivity ####

	output$spectrum <- renderPlot({
		validate(
			need(sv$spectrum_massrange(), "invalid mass range"),
			need(!anyNA(sv$spectrum_intensity_range()), "invalid intensity range")
		)
		tryCatch({
			print(plot_spectrum(),
				xlim=sv$spectrum_massrange(),
				ylim=sv$spectrum_intensity_range())
			plot_mz_marker()
		}, warning=plot_null, error=plot_null)
	}, bg="transparent")

	output$ionimage <- renderPlot({
		validate(
			need(sv$ionimage_xylim(), "invalid x/y limits"),
			need(!anyNA(sv$ionimage_intensity_range()), "invalid intensity range")
		)
		tryCatch({
			print(plot_ionimage(),
				xlim=sv$ionimage_xylim()[c(1,2)],
				ylim=sv$ionimage_xylim()[c(3,4)],
				zlim=sv$ionimage_intensity_range())
			plot_pos_marker()
		}, warning=plot_null, error=plot_null)
	}, bg="transparent")

	# click ionimage
	observe({
		validate(need(input$ionimage_click, "invalid click"))
		return() # do nothing
	})

	# click spectrum
	observe({
		validate(need(input$spectrum_click, "invalid click"))
		return() # do nothing
	})

	# double-click ionimage
	observe({
		validate(need(input$ionimage_dblclick, "invalid double-click"))
		pos <- c(input$ionimage_dblclick$x, input$ionimage_dblclick$y)
		sv$xy(round(pos))
	})

	# double-click spectrum
	observe({
		validate(need(input$spectrum_dblclick, "invalid double-click"))
		sv$mz(input$spectrum_dblclick$x)
	})

	# brush ionimage
	observe({
		validate(need(input$ionimage_brush, "invalid brush"))
		p <- input$ionimage_brush
		xylim <- c(p$xmin, p$xmax, p$ymin, p$ymax)
		pos <- c((p$xmin + p$xmax) / 2, (p$ymin + p$ymax) / 2)
		sv$ionimage_xylim(xylim)
	})

	# brush spectrum
	observe({
		validate(need(input$spectrum_brush, "invalid brush"))
		p <- input$spectrum_brush
		massrange <- c(p$xmin, p$xmax)
		mz <- (p$xmin + p$xmax) / 2
		sv$spectrum_massrange(massrange)
	})

	# change x/y image axes
	observe({
		xylim <- c(range(coord(data())[,sv$xy_names()[1]]),
			range(coord(data())[,sv$xy_names()[2]]))
		sv$ionimage_xylim(xylim)
	})

	#### nav input reactivity ####

	# m/z ui

	output$mz <- renderUI({
		numericInput(ns("mz"), "m/z", value=sv$mz(), step=1)
	})

	# # update m/z tolerance ui
	observe({
		validate(need(sv$mz_tol(), "invalid m/z tolerance"))
		updateNumericInput(session, "mz_tol", value=sv$mz_tol())
	})

	# x-axis name ui
	output$x_name <- renderUI({
		selectInput(ns("x_name"), NULL,
			choices=sv$coord_names(),
			selected=sv$xy_names()[1])
	})

	# y-axis name ui
	output$y_name <- renderUI({
		selectInput(ns("y_name"), NULL,
			choices=sv$coord_names(),
			selected=sv$xy_names()[2])
	})

	# x-value ui
	output$x <- renderUI({
		if ( gridded(pixelData(data())) ) {
			res <- resolution(pixelData(data()))
			i <- which(sv$xy_names()[1] %in% sv$coord_names())
			step <- res[i]
		} else {
			step <- 1
		}
		numericInput(ns("x"), NULL, value=sv$xy()[1], step=step)
	})

	# y-value ui
	output$y <- renderUI({
		if ( gridded(pixelData(data())) ) {
			res <- resolution(pixelData(data()))
			i <- which(sv$xy_names()[2] %in% sv$coord_names())
			step <- res[i]
		} else {
			step <- 1
		}
		numericInput(ns("y"), NULL, value=sv$xy()[2], step=step)
	})

	# change m/z val
	observeEvent(input$mz, {
		validate(
			need(input$mz, "invalid m/z value"),
			need(sv$mz_tol(), "invalid m/z tolerance")
		)
		feature_old <- sv$feature()
		feature_new <- features(data(), mz=input$mz)
		validate(need(feature_new, "invalid m/z value"))
		if ( feature_old != feature_new ) {
			mz_new <- input$mz
		} else {
			if ( abs(input$mz - sv$mz()) < sv$mz_tol() ) {
				return()
			} else if ( input$mz > sv$mz() ) {
				mz_new <- mz(data())[sv$feature() + 1L]
			} else if ( input$mz < sv$mz() ) {
				mz_new <- mz(data())[sv$feature() - 1L]
			}
		}
		sv$mz(mz_new)
	})

	# change m/z tolerance val
	observeEvent(input$mz_tol, {
		validate(need(input$mz_tol, "invalid m/z tolerance"))
		sv$mz_tol(input$mz_tol)
	})

	# change x name val
	observeEvent(input$x_name, {
		validate(need(input$x_name, "invalid x-axis name"))
		xy <- sv$xy_names()
		coord <- sv$coord_names()
		xy[1] <- input$x_name
		if ( xy[1] == xy[2] )
			xy[2] <- coord[-which(coord == xy[1])][1]
		sv$xy_names(xy)
	})

	# change y name val
	observeEvent(input$y_name, {
		validate(need(input$y_name, "invalid y-axis name"))
		xy <- sv$xy_names()
		coord <- sv$coord_names()
		xy[2] <- input$y_name
		if ( xy[1] == xy[2] )
			xy[1] <- coord[-which(coord == xy[2])][1]
		sv$xy_names(xy)
	})

	# change x val
	observeEvent(input$x, {
		validate(need(input$x, "invalid x value"))
		xr <- range(coord(data())[,sv$xy_names()[1]])
		if ( input$x < xr[1] || input$x > xr[2] )
			return()
		pos <- sv$xy()
		pos[1] <- input$x
		sv$xy(pos)
	})

	# change y val
	observeEvent(input$y, {
		validate(need(input$y, "invalid y value"))
		yr <- range(coord(data())[,sv$xy_names()[2]])
		if ( input$y < yr[1] || input$y > yr[2] )
			return()
		pos <- sv$xy()
		pos[2] <- input$y
		sv$xy(pos)
	})

	# subset ui
	output$subset <- renderUI({
		validate(
			need(sv$subset_choices(), "invalid subset list"),
			need(sv$subset(), "invalid subset")
		)
		choices <- sv$subset_choices()
		selected <- sv$subset()
		selectInput(ns("subset"), "Subset",
			choices=choices, selected=selected)
	})

	# change subset
	observeEvent(input$subset, {
		validate(need(input$subset, "invalid subset"))
		sv$subset(input$subset)
	})

	#### ionimage input reactivity ####

	# zoom full ionimage
	observeEvent(input$ionimage_zoom_full, {
		nms <- sv$xy_names()
		xylim <- c(range(coord(data())[,nms[1]]),
			range(coord(data())[,nms[2]]))
		sv$ionimage_xylim(xylim)
	})

	# zoom full x-axis ionimage
	observeEvent(input$ionimage_zoom_full_x, {
		nms <- sv$xy_names()
		xylim <- sv$ionimage_xylim()
		xylim <- c(range(coord(data())[,nms[1]]), xylim[c(3,4)])
		sv$ionimage_xylim(xylim)
	})

	# zoom full y-axis ionimage
	observeEvent(input$ionimage_zoom_full_y, {
		nms <- sv$xy_names()
		xylim <- sv$ionimage_xylim()
		xylim <- c(xylim[c(1,2)], range(coord(data())[,nms[2]]))
		sv$ionimage_xylim(xylim)
	})

	# zoom in ionimage
	observeEvent(input$ionimage_zoom_in, {
		pos <- sv$xy()
		xylim <- sv$ionimage_xylim()
		xlim <- zoom_in(xylim[c(1,2)], pos[1])
		ylim <- zoom_in(xylim[c(3,4)], pos[2])
		sv$ionimage_xylim(c(xlim, ylim))
	})

	# zoom out ionimage
	observeEvent(input$ionimage_zoom_out, {
		nms <- sv$xy_names()
		xr <- range(coord(data())[,nms[1]])
		yr <- range(coord(data())[,nms[2]])
		xylim <- sv$ionimage_xylim()
		xlim <- zoom_out(xylim[c(1,2)], xr)
		ylim <- zoom_out(xylim[c(3,4)], yr)
		sv$ionimage_xylim(c(xlim, ylim))
	})

	#### spectrum input reactivity ####

	# zoom full spectrum
	observeEvent(input$spectrum_zoom_full, {
		sv$spectrum_massrange(sv$mz_range())
		sv$spectrum_intensity_range(NULL)
	})

	# zoom full x-axis spectrum
	observeEvent(input$spectrum_zoom_full_x, {
		sv$spectrum_massrange(sv$mz_range())
	})

	# zoom full y-axis spectrum
	observeEvent(input$spectrum_zoom_full_y, {
		sv$spectrum_intensity_range(NULL)
	})

	# zoom in spectrum
	observeEvent(input$spectrum_zoom_in, {
		mzr <- sv$spectrum_massrange()
		mzr <- zoom_in(mzr, sv$mz())
		sv$spectrum_massrange(mzr)
	})

	# zoom out spectrum
	observeEvent(input$spectrum_zoom_out, {
		mzr <- sv$spectrum_massrange()
		mzr <- zoom_out(mzr, sv$mz_range())
		sv$spectrum_massrange(mzr)
	})

	#### scale input reactivity ####

	# ionimage height
	observeEvent(input$ionimage_height, {
		sv$ionimage_height(input$ionimage_height)
	})

	# spectrum height
	observeEvent(input$spectrum_height, {
		sv$spectrum_height(input$spectrum_height)
	})

	# autoscale ionimage intensity ui
	output$ionimage_intensity_range <- renderUI({
		if ( is.null(sv$ionimage_intensity_range()) ) {
			range <- range(spectra(data())[sv$feature(),])
		} else {
			range <- sv$ionimage_intensity_range()
		}
		step <- diff(range) / 4
		fluidRow(
			column(6, style="padding:0px 5px 0px 20px;",
				numericInput(ns("ionimage_intensity_min"), NULL,
					value=range[1], step=step)
			),
			column(6, style="padding:0px 20px 0px 5px;",
				numericInput(ns("ionimage_intensity_max"), NULL,
					value=range[2], step=step)
			)
		)
	})

	# min ionimage intensity
	observeEvent(input$ionimage_intensity_min, {
		if ( isFALSE(input$ionimage_autoscale) ) {
			min <- input$ionimage_intensity_min
			max <- input$ionimage_intensity_max
			sv$ionimage_intensity_range(c(min, max))
		}
	})

	# max ionimage intensity
	observeEvent(input$ionimage_intensity_max, {
		if ( isFALSE(input$ionimage_autoscale) ) {
			min <- input$ionimage_intensity_min
			max <- input$ionimage_intensity_max
			sv$ionimage_intensity_range(c(min, max))
		}
	})

	# autoscale ionimage intensity
	observe({
		if ( isTRUE(input$ionimage_autoscale) )
			sv$ionimage_intensity_range(NULL)
	})

	# autoscale spectrum intensity ui
	output$spectrum_intensity_range <- renderUI({
		if ( is.null(sv$spectrum_intensity_range()) ) {
			range <- range(spectra(data())[,sv$pixel()])
		} else {
			range <- sv$spectrum_intensity_range()
		}
		step <- diff(range) / 4
		fluidRow(
			column(6, style="padding:0px 5px 0px 20px;",
				numericInput(ns("spectrum_intensity_min"), NULL,
					value=range[1], step=step)
			),
			column(6, style="padding:0px 20px 0px 5px;",
				numericInput(ns("spectrum_intensity_max"), NULL,
					value=range[2], step=step)
			)
		)
	})

	# min spectrum intensity
	observeEvent(input$spectrum_intensity_min, {
		if ( isFALSE(input$spectrum_autoscale) ) {
			min <- input$spectrum_intensity_min
			max <- input$spectrum_intensity_max
			sv$spectrum_intensity_range(c(min, max))
		}
	})

	# max spectrum intensity
	observeEvent(input$spectrum_intensity_max, {
		if ( isFALSE(input$spectrum_autoscale) ) {
			min <- input$spectrum_intensity_min
			max <- input$spectrum_intensity_max
			sv$spectrum_intensity_range(c(min, max))
		}
	})

	# autoscale spectrum intensity
	observe({
		if ( isTRUE(input$spectrum_autoscale) )
			sv$spectrum_intensity_range(NULL)
	})

	#### options input reactivity ####

	# ionimage contrast
	observeEvent(input$ionimage_contrast, {
		sv$ionimage_contrast(input$ionimage_contrast)
	})

	# ionimage smoothing
	observeEvent(input$ionimage_smoothing, {
		sv$ionimage_smoothing(input$ionimage_smoothing)
	})

	# ionimage colorscale
	observeEvent(input$ionimage_colorscale, {
		sv$ionimage_colorscale(input$ionimage_colorscale)
	})

	# ionimage function
	observeEvent(input$ionimage_function, {
		sv$ionimage_function(input$ionimage_function)
	})

	#### return ####

	# reactive data
	sv[["data"]] <- data

	# reactive data name
	sv[["name"]] <- dataset

	return(sv)

}
