
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

get_subset_possible_expr <- function(subsets) {
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
	eval(parse(text=expr), as.env(pixelData(data)))
}
