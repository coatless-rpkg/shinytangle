# Required dependencies ----
library(shiny)
library(shinytangle)

# UI ----
ui <- fluidPage(
  ## Title ----
  titlePanel("shinytangle: Inline Interactive Slider"),
  
  ## Cookies Demo ----
  fluidRow(
    column(8, offset = 2,
           h3("Cookies"),
           p(
             "When you eat",
             inlineNumericInput("cookies", value = 2, min = 2, max = 100, step = 1),
             "cookies, you will consume",
             inlineOutput("calories"),
             "calories.",
             "That's ",
             inlineOutput("dailyPercent"),
             "% of your recommended daily calories."
           )
    )
  ),
  
  ## Travel Time Demo ----
  fluidRow(
    column(8, offset = 2,
           h3("Travel Time"),
           p(
             "A car traveling at",
             inlineNumericInput("speed", value = 49, min = 0, max = 200),
             "km/h for",
             inlineNumericInput("time", value = 3.1, min = 0, max = 24),
             "hours will travel",
             inlineOutput("distance"),
             "kilometers."
           )
    )
  )
)

# Server ----
server <- function(input, output) {
  
  ## Cookies to calories ----
  cookie_calories <- reactive({
    req(input$cookies)
    input$cookies * 50
  })
  
  ## Update calorie output ----
  output$calories <- renderInline({
    cookie_calories()
  })
  
  ## Update daily percent output ----
  output$dailyPercent <- renderInline({
    req(input$cookies)
    100 * cookie_calories() / 2100
  })
  
  ## Travel distance ----
  output$distance <- renderInline({
    req(input$speed, input$time)
    input$speed * input$time
  })
}

# Run app ----
shinyApp(ui = ui, server = server)
