
msiServer <- function(dataset) {

	function(input, output, session) {

		tabs <- reactiveVal(list())

		validate_tab <- function(id) {
			validate(need(id, "No tab selected"))
			validate(need(tabs()[[id]], "Invalid data state"))
			id
		}

		output$tab_menu <- renderMenu({
			ids <- names(tabs())
			mostrecent <- ids[length(ids)]
			datatabs <- lapply(ids, function(id) {
				menuSubItem(id, tabName=id, selected=id==mostrecent)
			})
			menu <- do.call("menuItem", c(list(text="Data", startExpanded=TRUE,
				fluidRow(
					column(8,
						style="padding:5px 5px 0px 20px;", 
						uiOutput("dataset")
					),
					column(4, style="padding:0px 0px 0px 0px;",
						fluidRow(
							actionButton("open", "", icon=icon("plus"))
						)
					)
				)
			), datatabs))
			help <- menuItem("Help", tabName="help")
			sidebarMenu(menu, help)
		})

		output$tab_display <- renderUI({
			ids <- names(tabs())
			mostrecent <- ids[length(ids)]
			datatabs <- lapply(ids, function(id) {
				if ( id == mostrecent ) {
					tabItem(tabName=id, uiOutput(id), class="active")
				} else {
					tabItem(tabName=id, uiOutput(id))
				}
			})
			if ( length(datatabs) > 0L ) {
				help <- tabItem(tabName="help", helpUI())
			} else {
				help <- tabItem(tabName="help", helpUI(), class="active")
			}
			help <- list(help)
			outtabs <- do.call("tabItems", c(datatabs, help))
			outtabs
		})

		# open dataset in new tab
		open_dataset_in_new_tab <- function(name) {
			data <- try(get(name, envir=globalenv()), silent=TRUE)
			if ( !inherits(data, get_supported_classes()) )
				return()
			try({
				id <- gsub("\\.", "", name)
				newtab <- list()
				loaded <- grepl(id, names(tabs()))
				if ( any(loaded) )
					id <- paste0(id, "-", sum(loaded) + 1)
				output[[id]] <- renderUI(msiViewUI(id))
				newtab[[id]] <- callModule(msiView, id, name)
				isolate(newtabs <- c(tabs(), newtab))
				isolate(tabs(newtabs))
				updateTabItems(session, "tabs", selected=id)
			}, silent=TRUE)
		}

		# observe "open"
		observeEvent(input$open, {
			name <- input$dataset
			validate(need(name, "Invalid dataset"))
			if ( input$open > 0 )
				open_dataset_in_new_tab(name)
		})

		# render dataset selector
		output$dataset <- renderUI({
			choices <- unlist(eapply(globalenv(), inherits, get_supported_classes()))
			choices <- c(Choose="", sort(names(choices)[choices]))
			selectInput("dataset", NULL, choices=choices)
		})

		# open initial dataset (if given)
		observeEvent(input$open, {
			if ( is.null(input$open) && !is.null(dataset) )
				open_dataset_in_new_tab(dataset)
		}, ignoreNULL=FALSE)

	}

}

selectServer <- function(dataset) {
  
  function(input, output, session) {
    
    output$plot_display <- renderUI({
      selectViewUI("test_select")
    })
    
    ## callModule from within reactive environment
    observe({
      if (!is.null(dataset))
        roi <- callModule(selectView, "test_select", dataset)
    })
    
    observe({
      if (!is.null(roi()))
        stopApp(roi())
    })
  }
  
}