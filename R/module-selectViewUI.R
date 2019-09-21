

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
      # style slider to remove background blue
      tags$style(HTML(".irs-bar, .irs-bar-edge {
            border-top: 1px solid transparent; 
            border-bottom: 1px solid transparent; 
            background: transparent;  
            border: 1px solid transparent;}")),
      fluidRow(style = "padding: 20px 20px 0px 20px;",
               uiOutput(ns("mz_slider_ui"))),
      actionButton(ns("but_debug"), "Debug")
    ),
    
    column(
      width = 3,
      box(
        title = "Navigation",
        background = get_box_background(),
        collapsible = TRUE,
        collapsed = FALSE,
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
        title = "Region of Interest",
        background = get_box_background(),
        collapsible = TRUE, collapsed = FALSE,
        width = 12,
        div(p("Double-click to select regions", 
              align="center", style="font-size:18px")),
        fluidRow(
          column(7, uiOutput(ns("region_name_ui"))),
          column(2, style = "padding: 25px 0px 0px 0px;", 
                 actionButton(ns("button_add"), "Add")),
          column(3, style = "padding: 25px 0px 0px 0px;", 
                 actionButton(ns("button_discard"), "Clear"))
        ),
        uiOutput(ns("region_picker_ui")),
        checkboxGroupInput(ns("options_checkbox"), label = "Plot options",
                           choices = list("Show region names on plot" = "names",
                                     "Show unseleted regions on plot" = "shapes"),
                           selected = c("names", "shapes")),
        fluidRow(column(6, actionButton(ns("button_select"), 
                        "Return as list", width = "100%")),
                 column(6, actionButton(ns("button_select_factor"), 
                        "Return as factor", width = "100%")
        ))
      ))
  ))
}
