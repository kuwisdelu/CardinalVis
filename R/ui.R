

msiUI <- function() {
	dashboardPage(skin="red",
		dashboardHeader(
			title="Cardinal",
			titleWidth=200
		),
		dashboardSidebar(
			width=200,
			sidebarMenuOutput("tab_menu")
		),
		dashboardBody(
			get_dashboard_bg_color(),
			tags$style(type="text/css",
				".shiny-input-container 
				{padding: 0px !important;}"),
			uiOutput("tab_display")
		)
	)
}

helpUI <- function() {
	fluidRow(
		box(title="Getting started",
			"Welcome to ", tags$i("CardinalVis"), "!",
			br(), br(),
			"Before using ", tags$i("CardinalVis"), " you need to load
				a dataset as a ", tags$code("MSImagingExperiment"),
				"object by using ", tags$code("readMSIData()"), " in R.",
			br(), br(),
			"See ", tags$code("?readMSIData"), " for help loading data into R.",
			br(), br(),
			"Once a dataset is loaded in R, you can use ", tags$i("CardinalVis"),
				" to explore and visualize it.",
			br(), br(),
			"Use the ", tags$strong("Data"), " tab on the left
				to select a loaded dataset, and click ", tags$strong("+"),
				"to open a new view of the data.",
			br(), br(),
			"The dataset will open in a new tab. You can
				open multiple datasets and switch
				between views of them by using the tabs
				in the sidebar.",
			width=4),
		box(title="Navigation",
			"When viewing a ", tags$code("MSImagingExperiment"),
				"you can navigate between images and spectra 
				by brushing and clicking on the plots.",
			br(), br(),
			"Double-click a pixel on the image to see the 
				corresponding mass spectrum.",
			br(), br(),
			"Double-click an m/z value on the spectrum to see the 
				corresponding image.",
				br(), br(),
			"Brush the plots by clicking and dragging
				to zoom in on the selected region.",
			br(), br(),
			"Use the buttons on the side of the plots 
				to zoom in, zoom out, or zoom-to-fit.",
			width=4),
		box(title="Viewing options",
			"Use the ", tags$strong("Navigation"), "box to control
				the current m/z value, pixel coordinates,
				and selected experimental runs. Multiple runs
				can be plotted at the same time.",
			br(), br(),
			"Use the ", tags$strong("Overlay"), "box to overlay
				the current plots with images from additional m/z values
				or spectra from additional pixels. (Use the checkbox
				to toggle which overlays are active.)",
			br(), br(),
			"Use the ", tags$strong("Intensity range"), "box to
				control the intensity range for the current
				image and spectrum. By default, the intensity range
				automatically scales to the current pixel or m/z value.",
			br(), br(),
			"Use the ", tags$strong("Image options"), "box to control
				image processing options such as spatial smoothing
				and contrast enhancement.",
			br(), br(),
			"Use the ", tags$strong("Variables"), "box to display
				a column from the object's", tags$code("pixelData()"),
				"or", tags$code("featureData()"), "instead of the intensity.",
			width=4)
	)
}

selectUI <- function() {
  dashboardPage(skin="red",
                dashboardHeader(
                  title="Region of Interest Selector"
                ),
                dashboardSidebar(disable=TRUE),
                dashboardBody(
                  get_dashboard_bg_color(),
                  tags$style(type="text/css",
                             ".shiny-input-container 
				{padding: 0px !important;}"),
                  uiOutput("plot_display")
                )
  )
}
