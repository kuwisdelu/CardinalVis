
selectView <- function(input, output, session, dataset, ..., simplify = FALSE) {
  #### session variables ####
  
  ns <- session$ns
  
  data <- reactive({
    tryCatch(
      get(dataset, envir = globalenv()),
      error = function(e)
        NULL
    )
  })
  
  dot_args <- list(...)
  if_else_helper <- function(name, value) {
    if ( is.null(dot_args[[name]]) ) 
      value
    else
      dot_args[[name]]
  }
  sv <- list(
    mz = syncVal(if_else_helper("mz", mz(data())[1]), function(mz) {
      validate(need(mz, "invalid m/z value"))
      mz(data())[features(data(), mz = mz)]
    }),
    mz_tol = syncVal(if_else_helper("plusminus", 0.001)),
    xy = syncVal(unname(unlist(coord(data())[1, c(1, 2)]))),
    xy_names = syncVal(names(coord(data()))[c(1, 2)]),
    coord_names = syncVal(coordnames(data())),
    ionimage_xylim = syncVal(c(range(coord(data())[, 1]),
    range(coord(data())[, 2]))),
    spectrum_massrange = syncVal(range(mz(data()))),
    ionimage_intensity_range = syncVal(NULL),
    spectrum_intensity_range = syncVal(NULL),
    ionimage_contrast = syncVal("none"),
    ionimage_smoothing = syncVal("none"),
    ionimage_colorscale = syncVal("viridis"),
    ionimage_function = syncVal("mean"),
    spectrum_plotvar = syncVal(names(imageData(data()))[1]),
    ionimage_plotvar = syncVal(names(imageData(data()))[1]),
    mz_2 = syncVal(NA),
    mz_3 = syncVal(NA),
    xy_2 = syncVal(c(NA, NA)),
    xy_3 = syncVal(c(NA, NA)),
    closed = reactiveVal(FALSE)
  )
  
  ## data ranges
  
  # mass range
  sv[["mz_range"]] <- reactive({
    range(mz(data()))
  })
  
  # coord range
  sv[["xy_range"]] <- reactive({
    c(range(coord(data())[, sv$xy_names()[1]]),
      range(coord(data())[, sv$xy_names()[2]]))
  })
  
  ## subsetting
  
  # all possible subsets
  sv[["subset_choices"]] <- reactive({
    subs <- get_subset_possible(data(), sv$xy_names())
    get_subset_choices(subs)
  })
  
  # subset options
  sv[["subset"]] <- syncVal(sv$subset_choices()[1])
  
  # subset logical
  sv[["subset_logical"]] <- reactive({
    get_subset_logical(data(), sv$subset())
  })
  
  ## pixels and features
  
  # pixel
  sv[["pixel"]] <- reactive({
    coord <- setNames(as.list(sv$xy()), sv$xy_names())
    if (all(!is.na(sv$xy_2()))) {
      coord[[1]] <- c(coord[[1]], sv$xy_2()[1])
      coord[[2]] <- c(coord[[2]], sv$xy_2()[2])
    }
    if (all(!is.na(sv$xy_3()))) {
      coord[[1]] <- c(coord[[1]], sv$xy_3()[1])
      coord[[2]] <- c(coord[[2]], sv$xy_3()[2])
    }
    subl <- sv$subset_logical()
    pixels(data(), coord = coord, subl, .env = environment())
  })
  
  # feature
  sv[["feature"]] <- reactive({
    mz <- sv$mz()
    if (!is.na(sv$mz_2()))
      mz <- c(mz, sv$mz_2())
    if (!is.na(sv$mz_3()))
      mz <- c(mz, sv$mz_3())
    features(data(), mz = mz)
  })
  
  ## mcols variables
  
  # pixel mcols
  sv[["pixel_vars"]] <- reactive({
    c(names(imageData(data()))[1], names(pixelData(data())))
  })
  
  # feature mcols
  sv[["feature_vars"]] <- reactive({
    c(names(imageData(data()))[1], names(featureData(data())))
  })
  
  ionimage <- reactive({
    validate(
      need(sv$mz(), "invalid m/z value"),
      need(sv$mz_tol(), "invalid m/z tolerance"),
      need(sv$xy_names(), "invalid x/y names"),
      need(sv$ionimage_plotvar(), "invalid image plot values")
    )
    lhs <- sv$ionimage_plotvar()
    val <- pixelData(data())[[lhs]]
    if (!is.null(val) && !is.numeric(val)) {
      superpose <- FALSE
      key <- TRUE
    } else if (length(sv$feature()) > 1L) {
      superpose <- TRUE
      key <- TRUE
    } else {
      superpose <- FALSE
      key <- FALSE
    }
    if (length(sv$subset()) >= 7L) {
      layout <- c(2, ceiling(length(sv$subset()) / 2))
    } else {
      layout <- TRUE
    }
    fm <- paste0(lhs, "~", paste0(sv$xy_names(), collapse = "*"))
    image(
      data(),
      formula = as.formula(fm),
      feature = sv$feature(),
      plusminus = sv$mz_tol(),
      contrast.enhance = sv$ionimage_contrast(),
      smooth.image = sv$ionimage_smoothing(),
      fun = match.fun(sv$ionimage_function()),
      key = key,
      superpose = superpose,
      colorscale = col.map(sv$ionimage_colorscale(), 100),
      subset = sv$subset_logical()
    )
  })
  
  ## region of interest variables
  sv[["selected_roi"]] <- syncVal(NULL)
  
  sv[["region_coords"]] <- syncVal({})
  
  sv[["current_region_coords"]] <- syncVal({
    list(x = c(), y = c())
  })
  
  sv[["region_names"]] <- reactive({
    names(sv$region_coords())
  })
  
  sv[["region_selected"]] <- reactive({
    sv$region_names()[sapply(sv$region_coords(), function(region) region$selected)]
  })
  
  sv[["region_subset_names"]] <- reactive({
    sapply(sv$region_coords(), function(region) { region$subset_name })
  })
  
  # plot options
  sv[["plot_options"]] <- syncVal({
    c("names" = T, "shapes" = T)
  })
  
  ## ionimage and interaction ##
  
  output$selectViewUI <- renderUI({
    tags$div(
      tags$style(
        type = "text/css",
        paste0(
          "#",
          ns("selectROIView"),
          " {height: calc(100vh - 300px) !important;}"
        )
      ),
      plotOutput(
        ns("selectROIView"),
        dblclick = dblclickOpts(id = ns("selectView_dbclick")),
        brush=brushOpts(id=ns("selectView_brush"),
                        direction="xy", resetOnNew=TRUE),
        height = "auto"
      )
    )
  })
  
  plot_null <- function(...) {
    par(mar=c(3,3,3,1), mgp=c(1.5,0.5,0),
        cex.axis=1, cex.lab=1)
    plot(0, 0, type='n', xlab="", ylab="",
         xaxt='n', yaxt='n')
    text(0, 0, "Nothing to plot.")
  }
  
  # function to overlay current region selections on plot
  ## centroid calculated by taking mean of all clicked point, 
  ## can cause problem for convex polygons.
  ## alternatively, use https://en.wikipedia.org/wiki/Centroid#Of_a_polygon
  plot_ploygons <- function() {
    # defind colors
    selected_hex = "#bdbdbd"
    unselected_hex = "#525252"
    
    # add current selection
    sel <- sv$current_region_coords()
    points(sel$x, sel$y, pch = 4, lwd = 4, 
           cex = 1.5, col = selected_hex)
    lines(sel$x, sel$y, lwd = 4, col = selected_hex)
    lines(x = c(sel$x[length(sel$x)], sel$x[1]),
          y = c(sel$y[length(sel$y)], sel$y[1]),
          lty = 3, lwd = 4, col = selected_hex)
    
    lapply(sv$region_coords(), function(region) {
      if (region$selected)
        color = selected_hex
      else {
        if ( !sv$plot_options()['shapes'] ) return()  # do not draw
        color = unselected_hex
      }
      
      polygon(region$x, region$y, border = color, lwd = 2)
      if ( sv$plot_option()['names'] ) 
        text(mean(region$x), mean(region$y), region$name,
             cex = 1.5, col = color)
    })
  }
  
  output$selectROIView <- renderPlot({
    validate(
      need(sv$ionimage_xylim(), "invalid x/y limits"),
      need(!anyNA(sv$ionimage_intensity_range()), "invalid intensity range")
    )
    tryCatch({
        print(ionimage(),
          xlim = sv$ionimage_xylim()[c(1, 2)],
          ylim = sv$ionimage_xylim()[c(3, 4)],
          zlim = sv$ionimage_intensity_range())
        plot_ploygons()
       }, warning = plot_null, error = plot_null)
  }, bg="transparent")
    
  observeEvent(input$button_debug, {
    browser()
  })
  
  # store click location on double click
  observeEvent(input$selectView_dbclick, {
    rcods <- sv$current_region_coords()
    rcods$x <- c(rcods$x, input$selectView_dbclick$x)
    rcods$y <- c(rcods$y, input$selectView_dbclick$y)
    sv$current_region_coords(rcods)
  })
  
  # brush selectView
  observe({
    validate(need(input$selectView_brush, "invalid brush"))
    p <- input$selectView_brush
    xylim <- c(p$xmin, p$xmax, p$ymin, p$ymax)
    pos <- c((p$xmin + p$xmax) / 2, (p$ymin + p$ymax) / 2)
    sv$ionimage_xylim(xylim)
  })
  
  # update plot options
  observe({
    # do not validate input$options_checkbox, can be null
    new_options <- c("names", "shapes") %in% input$options_checkbox
    names(new_options) <- c("names", "shapes")
    sv$plot_options(new_options)
  })
  
  #### nav input and mz slider reactivity ####
  
  # m/z slider
  output$mz_slider_ui <- renderUI({
    sliderTextInput(
      inputId = ns("mz_slider"),
      label = "m/z value",
      grid = T, width = "100%", force_edges = T,
      choices = mz(data())
    )
  })
  
  # change m/z slider value
  observeEvent(input$mz_slider, {
    validate(need(input$mz_slider, "invalid m/z value"),
             need(sv$mz_tol(), "invalid m/z tolerance"))
    feature_old <- sv$feature()[1]
    feature_new <- features(data(), mz = input$mz_slider)
    validate(need(feature_new, "invalid m/z value"))
    if (feature_old != feature_new) {
      mz_new <- input$mz_slider
    } else {
      return()
    }
    sv$mz(mz_new)
  })
  
  # update mz slider 
  observe({
    validate(need(sv$mz(), "invalid m/z value"))
    updateSliderTextInput(session, inputId = "mz_slider",
                          selected = sv$mz())
  })
  
  # m/z ui
  output$mz <- renderUI({
    numericInput(ns("mz"), "m/z", value = sv$mz(), step = 1)
  })
  
  # change m/z val
  observeEvent(input$mz, {
    validate(need(input$mz, "invalid m/z value"),
             need(sv$mz_tol(), "invalid m/z tolerance"))
    feature_old <- sv$feature()[1]
    feature_new <- features(data(), mz = input$mz)
    validate(need(feature_new, "invalid m/z value"))
    if (feature_old != feature_new) {
      mz_new <- input$mz
    } else {
      if (abs(input$mz - sv$mz()) < sv$mz_tol()) {
        return()
      } else if (input$mz > sv$mz()) {
        mz_new <- mz(data())[sv$feature()[1] + 1L]
      } else if (input$mz < sv$mz()) {
        mz_new <- mz(data())[sv$feature()[1] - 1L]
      }
    }
    sv$mz(mz_new)
  })
  
  # update m/z tolerance ui
  observe({
    validate(need(sv$mz_tol(), "invalid m/z tolerance"))
    updateNumericInput(session, "mz_tol", value = sv$mz_tol())
  })
  
  # change m/z tolerance val
  observeEvent(input$mz_tol, {
    validate(need(input$mz_tol, "invalid m/z tolerance"))
    sv$mz_tol(input$mz_tol)
  })
  
  # subset ui
  output$subset <- renderUI({
    validate(
      need(sv$subset_choices(), "invalid subset list"),
      need(sv$subset(), "invalid subset")
    )
    choices <- sv$subset_choices()
    selected <- sv$subset()
    selectInput(ns("subset"), "Subset", choices = choices,
      selected = selected, multiple = FALSE
    )
  })
  
  # change subset
  observeEvent(input$subset, {
    validate(need(input$subset, "invalid subset"))
    sv$subset(input$subset)   
  })
    
  #### region of interest interactivity ####
  
  # compute region mask for each coord pair
  compute_rois <- function() {
    regions <- sv$region_coords()
    selected <- sv$region_selected()
    image <- ionimage()
    selected_regions <- regions[selected]
    
    # check and remove null regions
    not_null_regions <- which(!sapply(selected_regions, function(region) {
      any(is.null(region$x))
    }))
    selected_regions <- selected_regions[not_null_regions]
    
    # compute region of interest
    rois <- lapply(selected_regions, function(region) {
      Cardinal:::.selectRegion(
        region,
        pixelData(data()),
        subset = get_subset_logical(data(), region$subset),
        axs = image$coordnames
      )
    })
    
    # try to simplify list to array if needed
    if ( simplify & length(rois) == 1)
      rois <- rois[[1]]
    
    rois  # return
  }
  
  observeEvent(input$button_select, {
    rois <- compute_rois()
    sv$selected_roi(rois)
  })
  
  observeEvent(input$button_select_factor, {
    rois <- compute_rois()
    roi_as_factor <- do.call(makeFactor, rois)
    sv$selected_roi(roi_as_factor)
  })
  
  # region name ui
  output$region_name_ui <- renderUI({
    region_names <- sv$region_names()
    last_name <- paste0("region", length(region_names) + 1)
    textInput(ns("region_name"), label = "Region name",
              value = last_name)
  })
  
  ## add the region to list of regions to return
  observeEvent(input$button_add, {
    rcords <- sv$current_region_coords()
    if ( is.null(rcords$x) ) {
      # do nothing, null region, no points clicked
    }
    
    regions <- sv$region_coords()
    pos <- length(regions) + 1
    regions[[pos]] <- list(
      x = rcords$x,
      y = rcords$y,
      subset = sv$subset(),
      selected = T,
      name = input$region_name
    )
    names(regions)[pos] <- input$region_name
    sv$region_coords(regions)
    
    # reset region
    sv$current_region_coords(list(x = c(), y = c()))
   })
  
  ## clear current region
  observeEvent(input$button_discard, {
    sv$current_region_coords(list(x = c(), y = c()))
  })
  
  # region picker
  output$region_picker_ui <- renderUI({
    if ( length(sv$subset_choices()) > 1 )
      pickerInput(
        inputId = ns("region_picker"), 
        label = "Select regions to return", 
        choices = sv$region_names(), multiple = TRUE,
        selected = sv$region_selected(),
        options = list(`actions-box` = TRUE,
                       size = 5),
        choicesOpt = list(
          subtext = sv$region_subset_names()
        ))
      else 
        pickerInput(
          inputId = ns("region_picker"), 
          label = "Select regions to return", 
          choices = sv$region_names(), multiple = TRUE,
          selected = sv$region_selected(),
          options = list(`actions-box` = TRUE,
                         size = 5)
          )
  })
  
  # update selected regions
  observeEvent(input$region_picker, {
    regions <- sv$region_coords()
    regions <- lapply(seq_along(regions), function(region_idx) {
      region <- regions[[region_idx]]
      if (sv$region_names()[region_idx] %in% input$region_picker) {
        region$selected <- T
      } else {
        region$selected <- F
      }
      region
    })
    names(regions) <- sv$region_names()
    sv$region_coords(regions)
    
  }, ignoreNULL = F)
  
  # update region in picker
  observe({
    if ( length(sv$subset_choices()) > 1 )
      updatePickerInput(session, "region_picker",
        choices = sv$region_names(),
        selected = sv$region_selected(),
        choicesOpt = list(
          subtext = sv$region_subset_names()
        )
      )
    else 
      updatePickerInput(session, "region_picker",
        choices = sv$region_names(),
        selected = sv$region_selected()
      )
  })
  
  #### ionimage zoom reactivity ####
  
  # zoom full ionimage
  observeEvent(input$selectView_zoom_full, {
    nms <- sv$xy_names()
    xylim <- c(range(coord(data())[,nms[1]]),
               range(coord(data())[,nms[2]]))
    sv$ionimage_xylim(xylim)
  })
  
  # zoom full x-axis ionimage
  observeEvent(input$selectView_zoom_full_x, {
    nms <- sv$xy_names()
    xylim <- sv$ionimage_xylim()
    xylim <- c(range(coord(data())[,nms[1]]), xylim[c(3,4)])
    sv$ionimage_xylim(xylim)
  })
  
  # zoom full y-axis ionimage
  observeEvent(input$selectView_zoom_full_y, {
    nms <- sv$xy_names()
    xylim <- sv$ionimage_xylim()
    xylim <- c(xylim[c(1,2)], range(coord(data())[,nms[2]]))
    sv$ionimage_xylim(xylim)
  })
  
  # zoom in ionimage
  observeEvent(input$selectView_zoom_in, {
    pos <- sv$xy()
    xylim <- sv$ionimage_xylim()
    xlim <- zoom_in(xylim[c(1,2)], pos[1])
    ylim <- zoom_in(xylim[c(3,4)], pos[2])
    sv$ionimage_xylim(c(xlim, ylim))
  })
  
  # zoom out ionimage
  observeEvent(input$selectView_zoom_out, {
    nms <- sv$xy_names()
    xr <- range(coord(data())[,nms[1]])
    yr <- range(coord(data())[,nms[2]])
    xylim <- sv$ionimage_xylim()
    xlim <- zoom_out(xylim[c(1,2)], xr)
    ylim <- zoom_out(xylim[c(3,4)], yr)
    sv$ionimage_xylim(c(xlim, ylim))
  })
  
  return(sv$selected_roi)
  
}