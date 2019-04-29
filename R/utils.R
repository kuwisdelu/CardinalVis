
# supported Cardinal classes

get_supported_classes <- function() {
	c("MSImagingExperiment")
}

# dashboard theme

get_dashboard_bg_color <- function() {
	if ( getOption("Cardinal.dark") ) {
		tags$head(tags$style(HTML(
			".content-wrapper, .right-side
			{background-color: #3A4246;}")))
	} else {
		tags$head(tags$style(HTML(
			".content-wrapper, .right-side
			{background-color: #EBF0F5;}")))
	}
}

get_actionButton_style <- function() {
	if ( getOption("Cardinal.dark") ) {
		"color: #AAAAAA;
		background-color: #262626;
		border-color: #636363;"
	} else {
		""
	}
}

get_box_background <- function() {
	if ( getOption("Cardinal.dark") ) {
		"black"
	} else {
		NULL
	}
}

# zoom utilities

zoom_in <- function(values, center) {
	lo <- values[1]
	hi <- values[2]
	d_lo <- (center - lo) / 2
	d_hi <- (hi - center) / 2
	c(lo + d_lo, hi - d_hi)
}

zoom_out <- function(values, range) {
	lo <- values[1]
	hi <- values[2]
	d_lo <- (lo - range[1]) / 2
	d_hi <- (range[2] - hi) / 2
	c(lo - d_lo, hi + d_hi)
}


# subset utilities

get_subset_possible <- function(data, xy_names) {
	pos <- coord(data)
	if ( !"run" %in% names(pos) )
		pos$run <- run(data)
	moredims <- !names(pos) %in% xy_names
	if ( any(moredims) ) {
		subs <- pos[moredims]
		unique(subs)
	} else {
		NULL
	}
}

get_subset_choices <- function(subsets) {
	if ( is.null(subsets) )
		return("")
	sub_expr <- sapply(1:nrow(subsets), function(i) {
		subs <- subsets[i,,drop=FALSE]
		vals <- sapply(subs, function(var) {
			if ( is.numeric(var) ) {
				paste0(var)
			} else {
				paste0("'", var, "'")
			}
		})
		expr <- paste0(names(subs), " == ", vals)
		paste0(expr, collapse=" & ")
	})
	sub_expr
}

get_subset_logical <- function(data, expr) {
	if ( length(expr) > 1 )
		expr <- paste0("(", expr, ")", collapse=" | ")
	eval(parse(text=expr), as.list(pixelData(data)))
}


# image options

ionimage_contrast_options <- function() {
	c("none", "suppression", "histogram")
}

ionimage_smoothing_options <- function() {
	c("none", "gaussian", "adaptive")
}

ionimage_colorscale_options <- function() {
	c("viridis", "cividis", "magma", "inferno", "plasma",
		"grayscale", "jet", "hot", "cool")
}

ionimage_function_options <- function() {
	c("mean", "sum", "max")
}


