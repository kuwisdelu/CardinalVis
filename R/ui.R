

msiUI <- function() {
	dashboardPage(skin="red",
		dashboardHeader(
			title="CardinalVis",
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

