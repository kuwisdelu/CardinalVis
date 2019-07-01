

selectViewUI <- function(id) {
  ns <- NS(id)
  
  ## UI elements
  
  tags$div(fluidRow(
    tags$head(tags$style(HTML(
      ".col-sm-12 {padding: 0px;}"
    ))),
    column(
      width = 9,
      tags$div(splitLayout(
        cellWidths = c("5%", "95%"),
        verticalLayout(
          actionButton(
            ns("selectView_zoom_full"),
            "",
            icon = icon("arrows"),
            width = "100%",
            style = get_actionButton_style()
          ),
          actionButton(
            ns("selectView_zoom_full_x"),
            "",
            icon = icon("arrows-h"),
            width = "100%",
            style = get_actionButton_style()
          ),
          actionButton(
            ns("selectView_zoom_full_y"),
            "",
            icon = icon("arrows-v"),
            width = "100%",
            style = get_actionButton_style()
          ),
          actionButton(
            ns("selectView_zoom_in"),
            "",
            icon = icon("search-plus"),
            width = "100%",
            style = get_actionButton_style()
          ),
          actionButton(
            ns("selectView_zoom_out"),
            "",
            icon = icon("search-minus"),
            width = "100%",
            style = get_actionButton_style()
          )
        ),
        uiOutput(ns("selectViewUI"))
      )),
      fluidRow(style = "padding: 20px 20px 0px 20px;",
               uiOutput(ns("mz_slider_ui")))
    ),
    
    column(
      width = 3,
      box(
        title = "Navigation",
        background = get_box_background(),
        collapsible = FALSE,
        width = 12,
        fluidRow(
          column(6, style = "padding:0px 5px 0px 20px;",
                 uiOutput(ns("mz"))),
          column(6, style = "padding:0px 20px 0px 5px;",
                 numericInput(
                   ns("mz_tol"), "+/-", value = 0.001, step = 0.001
                 ))
        ),
        fluidRow(style = "padding:0px 20px 0px 20px;",
                 uiOutput(ns("subset")))
      ),
      box(
        title = "Subset",
        background = get_box_background(),
        collapsible = FALSE,
        width = 12,
        fluidRow(style = "padding: 0px 15px 0px 15px;",
          uiOutput(ns("region_picker_ui"))),
          #column(5, uiOutput(ns("region_picker_ui"))),
        fluidRow(
          column(7, uiOutput(ns("region_name_ui"))),
          column(3, style = "padding: 25px 0px 0px 0px;", ## fix top padding
                 actionButton(ns("button_name_update"), "update")),
          column(2, style = "padding: 25px 0px 0px 0px;", ## fix top padding
                 actionButton(ns("button_plus"), "+"))
        ),
        fluidRow(column(
          6,
          actionButton(ns("button_select"), "Select as list",
                       width = "100%")
        ),
        column(
          6,
          actionButton(ns("button_select_factor"), "Select as factor",
                       width = "100%")
        ))
      ))
  ),
  
  fluidRow(actionButton(ns("button_debug"), "DEBUG")))
}