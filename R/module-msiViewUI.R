
msiViewUI <- function(id, label = "Data Viewer") {
	ns <- NS(id)
	fluidRow(
		tags$head(tags$style(HTML(".col-sm-12 {padding: 0px;}"))),
		tags$head(tags$style(HTML(".box {margin: 5px;}"))),
		tags$head(tags$script(HTML(
     "$(document).on('shiny:busy', function(event) {
      timeoutbusy = setTimeout(function() {
        $('#plot_div').css('display', 'none');
        $('#spectrum_div').css('display', 'none');
        $('#spinner1_div').css('display', 'block');
        $('#spinner2_div').css('display', 'block');
      }, 500);
    });
    
    $(document).on('shiny:idle', function(event) {
      clearTimeout(timeoutbusy)
      $('#plot_div').css('display', 'block');
      $('#spectrum_div').css('display', 'block');
      $('#spinner1_div').css('display', 'none');
      $('#spinner2_div').css('display', 'none');
    });"))),
		
		column(width=9,
			verticalLayout(
				tags$div(
					splitLayout(
						cellWidths=c("5%", "95%"),
						verticalLayout(
							actionButton(ns("ionimage_zoom_full"), "", icon=icon("arrows"),
								width="100%", style=get_actionButton_style()),
							actionButton(ns("ionimage_zoom_full_x"), "", icon=icon("arrows-h"),
								width="100%", style=get_actionButton_style()),
							actionButton(ns("ionimage_zoom_full_y"), "", icon=icon("arrows-v"),
								width="100%", style=get_actionButton_style()),
							actionButton(ns("ionimage_zoom_in"), "", icon=icon("search-plus"),
								width="100%", style=get_actionButton_style()),
							actionButton(ns("ionimage_zoom_out"), "", icon=icon("search-minus"),
								width="100%", style=get_actionButton_style())
						),
						uiOutput(ns("ionimage_plot"))
					)
				),
				box(width=12,
					background=get_box_background(),
					splitLayout(
						cellWidths=c("5%", "95%"),
						verticalLayout(
							actionButton(ns("spectrum_zoom_full"), "", icon=icon("arrows"),
								width="100%", style=get_actionButton_style()),
							actionButton(ns("spectrum_zoom_full_x"), "", icon=icon("arrows-h"),
								width="100%", style=get_actionButton_style()),
							actionButton(ns("spectrum_zoom_full_y"), "", icon=icon("arrows-v"),
								width="100%", style=get_actionButton_style()),
							actionButton(ns("spectrum_zoom_in"), "", icon=icon("search-plus"),
								width="100%", style=get_actionButton_style()),
							actionButton(ns("spectrum_zoom_out"), "", icon=icon("search-minus"),
								width="100%", style=get_actionButton_style())
						),
						uiOutput(ns("spectrum_plot"))
					)
				)
			)
		),
		column(width=3,
			box(title="Navigation",
				background=get_box_background(),
				collapsible=TRUE,
				width=12,
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
						uiOutput(ns("y_name"))
					)
				),
				fluidRow(
					column(6, style="padding:0px 5px 0px 20px;",
						uiOutput(ns("x"))
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
			box(title="Overlay",
				background=get_box_background(),
				collapsible=TRUE,
				collapsed=TRUE,
				width=12,
				fluidRow(
					style="padding:0px 0px 0px 20px;",
					tags$label("m/z")
				),
				fluidRow(
					column(2, style="padding:0px 5px 0px 20px;",
						checkboxInput(ns("mz_2_overlay"), NULL, value=FALSE)
					),
					column(10, style="padding:0px 20px 0px 5px;",
						numericInput(ns("mz_2"), NULL, value=NA, step=1)
					)
				),
				fluidRow(
					column(2, style="padding:0px 5px 0px 20px;",
						checkboxInput(ns("mz_3_overlay"), NULL, value=FALSE)
					),
					column(10, style="padding:0px 20px 0px 5px;",
						numericInput(ns("mz_3"), NULL, value=NA, step=1)
					)
				),
				fluidRow(
					style="padding:0px 0px 0px 20px;",
					tags$label("Position")
				),
				fluidRow(
					column(2, style="padding:0px 5px 0px 20px;",
						checkboxInput(ns("xy_2_overlay"), NULL, value=FALSE)
					),
					column(5, style="padding:0px 10px 0px 10px;",
						numericInput(ns("x_2"), NULL, value=NA, step=1)
					),
					column(5, style="padding:0px 20px 0px 5px;",
						numericInput(ns("y_2"), NULL, value=NA, step=1)
					)
				),
				fluidRow(
					column(2, style="padding:0px 5px 0px 20px;",
						checkboxInput(ns("xy_3_overlay"), NULL, value=FALSE)
					),
					column(5, style="padding:0px 10px 0px 10px;",
						numericInput(ns("x_3"), NULL, value=NA, step=1)
					),
					column(5, style="padding:0px 20px 0px 5px;",
						numericInput(ns("y_3"), NULL, value=NA, step=1)
					)
				)
			),
			box(title="Intensity range",
				background=get_box_background(),
				collapsible=TRUE,
				collapsed=TRUE,
				width=12,
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
				)
			),
			box(title="Image options",
				background=get_box_background(),
				collapsible=TRUE,
				collapsed=TRUE,
				width=12,
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
			),
			box(title="Variables",
				background=get_box_background(),
				collapsible=TRUE,
				collapsed=TRUE,
				width=12,
				fluidRow(
					column(6, style="padding:0px 5px 0px 20px;",
						uiOutput(ns("pixel_vars"))
					),
					column(6, style="padding:0px 20px 0px 5px;",
						uiOutput(ns("feature_vars"))
					)
				)
			)
		)
	)
}

