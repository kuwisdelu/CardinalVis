
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

selectServer <- function(dataset, ...) {
  
  id <- "selectVis"
  
  function(input, output, session) {
    
    ## help menu ##
    imagePath <- paste0(system.file(package = "CardinalVis"), "/www/")
    
    # general help
    observeEvent(input$openGenHelp, {
      showModal(
        modalDialog(
          title = "Region of Interest picker",
          "Use this dashboard to pick a region that might be of interest in an", 
          tags$code("MSImagingExperiment"), "object.", br(), "Use the", 
          tags$strong("Navigation"), "panel to update m/z value or",
          "subset of interest for the current region selection. The m/z value can", 
          "also be updated using the slider at the bottom.", br(), "The", 
          tags$strong("Region of Interest"), "panel can be used to add, clear, rename,",
          "select, and return the selected regions. There is no limit on the number of",
          "selectable regions.", br(), br(),
          "Double-click on the ion-image to start creating a polygon.",
          "You will be able to preview the polygon as you click. There is no",
          "limit to the number of sides of the polygon. The dashed line completes",
          "the polygon when confirmed. The image below gives a preview.",
          br(), br(), 
          tags$img(src=base64enc::dataURI(file = paste0(imagePath, "help_image_one.jpg"), 
                                          mime = "image/jpeg"),
                   style="display: block; margin-left: auto; margin-right: auto; width: 40%;"),
          br(), br(),
          "Each region can be named, by default each region is named as",
          tags$code("regionX"), ", where X is the index of the region in the list.",
          "Once satisfied, use the", tags$code("Add"), "button to confim a region, or", tags$code("Clear"),
          "button to reset the current selection.", br(), br(),
          "You can choose to return a named list of boolean vectors, where the names",
          "are the region name, or a factor containing the region names."
        )
      )
    }, ignoreInit = T)
   
    # multi select help 
    observeEvent(input$openMultiHelp, {
      showModal(
        modalDialog(
          title = "Selecting multiple regions",
          "Once a region is added using", tags$code("Add"), ", you have the option",
          "to add multiple regions. All previous confirmed regions are indicated",
          "on the ion-image with their respective region names. The image below",
          "gives a preview of two confirmed regions", br(), br(),
          tags$img(src=base64enc::dataURI(file = paste0(imagePath, "help_image_two.jpg"), 
                                          mime = "image/jpeg"),
                   style="display: block; margin-left: auto; margin-right: auto; width: 40%;")
        )
      )
    }, ignoreInit = T)
    
    # return help
    observeEvent(input$openReturnHelp, {
      showModal(
        modalDialog(
          title = "Picking what to return",
          "The dropdown menu in", tags$code("Region of Interest"), 
          "can be used to select and deselect which regions are returned. When an",
          tags$code("MSImagingExperiment"), "has multiple runs, subtext specifying",
          "the subset of that region is also shown.", br(), br(),
          tags$img(src=base64enc::dataURI(file = paste0(imagePath, "help_image_three.jpg"), 
                                          mime = "image/jpeg"),
                   style="display: block; margin-left: auto; margin-right: auto; width: 50%;"),
          br(), 
          "Some convenience plot options are provided to declutter the ion-image when working",
          "with a large number of regions."
        )
      )
    }, ignoreInit = T)
    
    # render main UI
    output$plot_display <- renderUI({
      selectViewUI(id)
    })
    
    # ret holds return values, ret$roi is null
    ret <- reactiveValues(roi = NULL)
    
    # run once on start
    observeEvent(input, {
      if ( !is.null(dataset) )
        ret$roi <- callModule(selectView, id, dataset, ...)
    }, ignoreNULL = FALSE)
    
    # return values when ret$roi is not null
    observe({
      if ( !is.null(ret$roi()) ) {
        stopApp(ret$roi())
      }
    })
    
  }
  
}
