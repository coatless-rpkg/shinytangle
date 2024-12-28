library(shiny)
library(shinytangle)

ui <- fluidPage(
  titlePanel("shinytangle: Demo Inline"),
  p(
    "When you have", 
    inlineNumericInput("amount", 5, min = 0, max = 10), 
    "items, the total cost is",
    inlineOutput("cost")
  )
)

server <- function(input, output) {
  output$cost <- renderInline({
    input$amount * 9.99
  })
}

shinyApp(ui, server)
