library(shiny)

ui <- fluidPage(
  tags$head(tags$style(HTML(".loader,
.loader:before,
.loader:after {
  background: #001cff;
  -webkit-animation: load1 1s infinite ease-in-out;
  animation: load1 1s infinite ease-in-out;
  width: 1em;
  height: 4em;
}
.loader {
  color: #001cff;
  text-indent: -9999em;
  margin: 88px auto;
  position: relative;
  font-size: 11px;
  -webkit-transform: translateZ(0);
  -ms-transform: translateZ(0);
  transform: translateZ(0);
  -webkit-animation-delay: -0.16s;
  animation-delay: -0.16s;
}
.loader:before,
.loader:after {
  position: absolute;
  top: 0;
  content: '';
}
.loader:before {
  left: -1.5em;
  -webkit-animation-delay: -0.32s;
  animation-delay: -0.32s;
}
.loader:after {
  left: 1.5em;
}
@-webkit-keyframes load1 {
  0%,
  80%,
  100% {
    box-shadow: 0 0;
    height: 4em;
  }
  40% {
    box-shadow: 0 -2em;
    height: 5em;
  }
}
@keyframes load1 {
  0%,
  80%,
  100% {
    box-shadow: 0 0;
    height: 4em;
  }
  40% {
    box-shadow: 0 -2em;
    height: 5em;
  }
}
"))),
  tags$script(HTML(
    "$(document).on('shiny:busy', function(event) {
      timeoutbusy = setTimeout(function() {
      $('#plot_class').css('display', 'none');
      $('#holder').css('display', 'block');}, 500);
    });
    
    $(document).on('shiny:idle', function(event) {
      clearTimeout(timeoutbusy)
      $('#plot_class').css('display', 'block');
      $('#holder').css('display', 'none');
    });"
  )),
  
  tags$div(style="height: calc(100vh - 300px);",
    fluidRow(
      tags$div(id="plot_class", plotOutput("test_plot")),#, height="auto")),
      tags$div(id="holder", style="",
               tags$div(class="loader"))
      )
    ),
  fluidRow(actionButton("runner", "runner")),
  fluidRow(actionButton("run_long", "run long"))
)

server <- function(input, output, session) {
  
  n <- reactiveVal(1000)
  s <- reactiveVal(0)
  
  output$other_plot <- renderPlot({
    plot(c(1, 2, 3), c(1, 2, 3))
  })
  
  output$test_plot <- renderPlot({
    Sys.sleep(isolate(s()))
    hist(runif(n()))
  })
  
  observeEvent(input$runner, {
    s(0)
    n(n() + 100)
  })
  
  observeEvent(input$run_long, {
    s(1)
    n(n() + 100)
  })
}

shinyApp(ui, server)

