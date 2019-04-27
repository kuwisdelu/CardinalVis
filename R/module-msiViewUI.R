
msiViewUI <- function(id, label = "Data Viewer") {
	ns <- NS(id)
	verticalLayout(
		fluidRow(
			fluidRow(
				tabBox(width=3,
					id=ns("options"),
					tabPanel("Nav",
						fluidRow(
							column(6, style="padding:0px 5px 0px 20px;", 
								uiOutput(ns("mz"))
							),
							column(6, style="padding:0px 20px 0px 5px;",
								numericInput(ns("mz_tol"), "+/-", value=0.001, step=0.001)
							)
						),
						fluidRow(
							style="padding:0px 0px 0px 20px;",
							tags$label("Position")
						),
						fluidRow(
							column(6, style="padding:0px 5px 0px 20px;",
								uiOutput(ns("x_name"))
							),
							column(6, style="padding:0px 20px 0px 5px;",
								uiOutput(ns("x"))
							)
						),
						fluidRow(
							column(6, style="padding:0px 5px 0px 20px;",
								uiOutput(ns("y_name"))
							),
							column(6, style="padding:0px 20px 0px 5px;",
								uiOutput(ns("y"))
							)
						),
						fluidRow(
							style="padding:0px 20px 0px 20px;",
							uiOutput(ns("subset"))
						)
					),
					tabPanel("Scale",
						fluidRow(
							style="padding:0px 20px 0px 20px;",
							checkboxInput(ns("ionimage_autoscale"),
								"Autoscale image intensity", value=TRUE)
						),
						conditionalPanel(
							ns=ns,
							condition = "input.ionimage_autoscale == false",
							fluidRow(
								style="padding:0px 0px 0px 20px;",
								tags$label("Image intensity range")
							),
							uiOutput(ns("ionimage_intensity_range"))
						),
						fluidRow(
							style="padding:0px 20px 0px 20px;",
							checkboxInput(ns("spectrum_autoscale"),
								"Autoscale spectrum intensity", value=TRUE)
						),
						conditionalPanel(
							ns=ns,
							condition = "input.spectrum_autoscale == false",
							fluidRow(
								style="padding:0px 0px 0px 20px;",
								tags$label("Spectrum intensity range")
							),
							uiOutput(ns("spectrum_intensity_range"))
						),
						fluidRow(
							column(6, style="padding:0px 5px 0px 20px;",
								numericInput(ns("ionimage_height"), "Image height",
									value=350, min=100, step=25)
							),
							column(6, style="padding:0px 20px 0px 5px;",
								numericInput(ns("spectrum_height"), "Spectrum height",
									value=200, min=100, step=25)
							)
						)
					),
					tabPanel("Options",
						fluidRow(
							column(6, style="padding:0px 5px 0px 20px;",
								selectInput(ns("ionimage_contrast"), "Contrast",
									choices=ionimage_contrast_options())
							),
							column(6, style="padding:0px 20px 0px 5px;",
								selectInput(ns("ionimage_smoothing"), "Smoothing",
									choices=ionimage_smoothing_options())
							)
						),
						fluidRow(
							column(6, style="padding:0px 5px 0px 20px;",
								selectInput(ns("ionimage_colorscale"), "Colorscale",
									choices=ionimage_colorscale_options())
							),
							column(6, style="padding:0px 20px 0px 5px;",
								selectInput(ns("ionimage_function"), "Intensity",
									choices=ionimage_function_options())
							)
						)
					)
				),
				box(width=9,
					splitLayout(
						cellWidths=c("92.5%", "7.5%"),
						uiOutput(ns("ionimage_plot")),
						verticalLayout(
							actionButton(ns("ionimage_zoom_full"), "", icon=icon("arrows"), width="100%"),
							actionButton(ns("ionimage_zoom_full_x"), "", icon=icon("arrows-h"), width="100%"),
							actionButton(ns("ionimage_zoom_full_y"), "", icon=icon("arrows-v"), width="100%"),
							actionButton(ns("ionimage_zoom_in"), "", icon=icon("search-plus"), width="100%"),
							actionButton(ns("ionimage_zoom_out"), "", icon=icon("search-minus"), width="100%")
						)
					)
				)
			),
			fluidRow(
				box(width=12,
					splitLayout(
						cellWidths=c("95%", "5%"),
						uiOutput(ns("spectrum_plot")),
						verticalLayout(
							actionButton(ns("spectrum_zoom_full"), "", icon=icon("arrows"), width="100%"),
							actionButton(ns("spectrum_zoom_full_x"), "", icon=icon("arrows-h"), width="100%"),
							actionButton(ns("spectrum_zoom_full_y"), "", icon=icon("arrows-v"), width="100%"),
							actionButton(ns("spectrum_zoom_in"), "", icon=icon("search-plus"), width="100%"),
							actionButton(ns("spectrum_zoom_out"), "", icon=icon("search-minus"), width="100%")
						)
					)
				)
			)
		),
		actionButton(ns("close"), "Close",
			style="float:right; padding:0px 2px 0px 2px; font-size:90%;")
	)
}
