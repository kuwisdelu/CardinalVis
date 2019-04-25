
msiUI <- function() {
	dashboardPage(skin="red",
		dashboardHeader(title="Cardinal"),
		dashboardSidebar(
			sidebarMenu(id="tabs",
				menuItem("Data",
					startExpanded=TRUE,
					menuSubItem("Datasets", tabName="data", icon=NULL),
					fluidRow(
						column(6, style="padding:5px 5px 0px 20px;", 
							uiOutput("dataset")
						),
						column(6, style="padding:0px 0px 0px 0px;",
							fluidRow(
								actionButton("open", "Open in new tab",
									style="padding:6px 5px 6px 5px; font-size:90%;")
							)
						)
					),
					fluidRow(
						column(6, style="padding:0px 5px 0px 20px;", 
							numericInput("mz", "m/z", value=NA, step=1)
						),
						column(6, style="padding:0px 20px 0px 5px;",
							numericInput("mz_tol", "+/-", value=NA, step=0.001)
						)
					),
					fluidRow(
						style="padding:0px 0px 0px 20px;",
						tags$label("Position")
					),
					fluidRow(
						column(6, style="padding:0px 5px 0px 20px;",
							selectInput("x_name", NULL, choices=c(Choose=""))
						),
						column(6, style="padding:0px 20px 0px 5px;",
							numericInput("x", NULL, value=NA, step=1)
						)
					),
					fluidRow(
						column(6, style="padding:0px 5px 0px 20px;",
							selectInput("y_name", NULL, choices=c(Choose=""))
						),
						column(6, style="padding:0px 20px 0px 5px;",
							numericInput("y", NULL, value=NA, step=1)
						)
					),
					fluidRow(
						column(6, style="padding:0px 5px 0px 20px;",
							numericInput("ionimage_zoom", "Image %",
								value=100, min=100, step=100)
						),
						column(6, style="padding:0px 20px 0px 5px;",
							numericInput("spectrum_zoom", "Spectrum %",
								value=100, min=100, step=100)
						)
					),
					fluidRow(
						style="padding:0px 20px 0px 20px;",
						selectInput("subset", "Subset", choices=c(Choose=""))
					)
				),
				menuItem("Options",
					fluidRow(
						column(6, style="padding:0px 5px 0px 20px;",
							selectInput("ionimage_contrast", "Contrast",
								choices=ionimage_contrast_options())
						),
						column(6, style="padding:0px 20px 0px 5px;",
							selectInput("ionimage_smoothing", "Smoothing",
								choices=ionimage_smoothing_options())
						)
					),
					fluidRow(
						column(6, style="padding:0px 5px 0px 20px;",
							selectInput("ionimage_colorscale", "Colorscale",
								choices=ionimage_colorscale_options())
						),
						column(6, style="padding:0px 20px 0px 5px;",
							selectInput("ionimage_function", "Intensity",
								choices=ionimage_function_options())
						)
					),
					fluidRow(
						style="padding:0px 20px 0px 20px;",
						checkboxInput("ionimage_autoscale",
							"Autoscale image intensity", value=TRUE)
					),
					conditionalPanel(
						condition = "input.ionimage_autoscale == false",
						fluidRow(
							style="padding:0px 0px 0px 20px;",
							tags$label("Image intensity range")
						),
						fluidRow(
							column(6, style="padding:0px 5px 0px 20px;",
								numericInput("ionimage_intensity_min", NULL,
									value=0, step=1)
							),
							column(6, style="padding:0px 20px 0px 5px;",
								numericInput("ionimage_intensity_max", NULL,
									value=100, step=1)
							)
						)
					),
					fluidRow(
						style="padding:0px 20px 0px 20px;",
						checkboxInput("spectrum_autoscale",
							"Autoscale spectrum intensity", value=TRUE)
					),
					conditionalPanel(
						condition = "input.spectrum_autoscale == false",
						fluidRow(
							style="padding:0px 0px 0px 20px;",
							tags$label("Spectrum intensity range")
						),
						fluidRow(
							column(6, style="padding:0px 5px 0px 20px;",
								numericInput("spectrum_intensity_min", NULL,
									value=0, step=1)
							),
							column(6, style="padding:0px 20px 0px 5px;",
								numericInput("spectrum_intensity_max", NULL,
									value=100, step=1)
							)
						)
					),
					fluidRow(
						style="padding:0px 20px 0px 20px;",
						sliderInput("plot_layout", "Plot layout",
							min=0, max=100, value=50)
					),
					fluidRow(
						column(6, style="padding:0px 5px 0px 20px;",
							numericInput("ionimage_height", "Image size",
								value=400, min=100, step=25)
						),
						column(6, style="padding:0px 20px 0px 5px;",
							numericInput("spectrum_height", "Spectrum size",
								value=400, min=100, step=25)
						)
					)
				)
			)
		),
		dashboardBody(
			tags$style(type="text/css",
				".shiny-input-container 
				{padding: 0px !important;}"),
			tabItems(
				tabItem("data",
					tabBox(id="datatabs", width=12)
				)
			)
		)
	)
}

# options

ionimage_contrast_options <- function() {
	c("none", "suppression", "histogram")
}

ionimage_smoothing_options <- function() {
	c("none", "gaussian", "adaptive")
}

ionimage_colorscale_options <- function() {
	c("viridis", "cividis", "magma", "inferno", "plasma")
}

ionimage_function_options <- function() {
	c("mean", "sum", "max")
}


