# Minimal Shiny app used to verify the Tauri -> R plumbing without pulling in
# the real app's dependencies (INLA, epiprocess, simplets are not installable
# until the bundled runtime exists).
library(shiny)

ui <- fluidPage(
  titlePanel("MicroHub smoke test"),
  p("If you can see this, Tauri started R and the webview reached Shiny."),
  verbatimTextOutput("info"),
  sliderInput("n", "n", min = 1, max = 100, value = 10),
  plotOutput("plot")
)

server <- function(input, output, session) {
  output$info <- renderText({
    paste0(
      "R ", getRversion(), "\n",
      "shiny ", packageVersion("shiny"), "\n",
      "pid ", Sys.getpid(), "\n",
      "wd ", getwd()
    )
  })

  output$plot <- renderPlot({
    plot(seq_len(input$n), main = "plumbing works")
  })
}

shinyApp(ui, server)
