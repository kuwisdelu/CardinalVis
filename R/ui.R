

msiUI <- function() {
	dashboardPage(skin="red",
		dashboardHeader(title="Cardinal"),
		dashboardSidebar(
			width=200,
			sidebarMenuOutput("tab_menu")
		),
		dashboardBody(
			tags$style(type="text/css",
				".shiny-input-container 
				{padding: 0px !important;}"),
			uiOutput("tab_display")
		)
	)
}

