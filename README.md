
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinytangle

<!-- badges: start -->

[![R-CMD-check](https://github.com/coatless-rpkg/shinytangle/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coatless-rpkg/shinytangle/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

shinytangle provides inline interactive widgets for Shiny applications,
allowing you to embed interactive numeric inputs and outputs directly
within text. This makes it easy to create dynamic, interactive
narratives and explanations where users can adjust values and see
results update in real-time.

## Installation

You can install the development version of shinytangle from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("coatless-rpkg/shinytangle")
```

## Usage

Here’s an example of how to use shinytangle:

``` r
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
```
