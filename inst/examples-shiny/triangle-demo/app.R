# Required dependencies ----
library(shiny)
library(shinytangle)
library(ggplot2)

# Calculate triangle coordinates given side lengths ----
calculate_triangle_coords <- function(a, b, c) {
  # First vertex at origin
  x1 <- 0
  y1 <- 0
  
  # Second vertex along x-axis
  x2 <- a
  y2 <- 0
  
  # Third vertex using law of cosines
  cos_C <- (a^2 + b^2 - c^2) / (2*a*b)
  x3 <- b * cos_C
  y3 <- b * sqrt(1 - cos_C^2)
  
  list(
    x = c(x1, x2, x3, x1),  # Close the polygon
    y = c(y1, y2, y3, y1)
  )
}

# UI ----
ui <- fluidPage(
  ## Title ----
  titlePanel("shinytangle: Triangle Area"),
  
  ## Row ----
  fluidRow(
    column(8, offset = 2,
           
           ### Plot output ----
           plotOutput("triangle_plot", height = "400px"),
           
           ### Well panel ----
           wellPanel(
             h4("Interactive Triangle Calculator"),
             p(
               "Consider a triangle with sides of length ",
               inlineNumericInput("side_a", 5, min = 0.1, max = 20, step = 0.1),
               ", ",
               inlineNumericInput("side_b", 4, min = 0.1, max = 20, step = 0.1),
               ", and ",
               inlineNumericInput("side_c", 3, min = 0.1, max = 20, step = 0.1),
               " units."
             ),
             
             hr(),
             
             p(
               "The triangle's perimeter is ",
               inlineOutput("perimeter"),
               " units"
             ),
             
             p(
               "Its semi-perimeter is ",
               inlineOutput("semi_perimeter"),
               " units"
             ),
             
             p(
               "Using Heron's formula, the area is ",
               inlineOutput("area"),
               " square units"
             )
           )
    )
  )
)

# Server logic ----
server <- function(input, output) {
  
  ## Check if a valid triangle is possible ----
  is_valid_triangle <- reactive({ 
    req(input$side_a, input$side_b, input$side_c)
    
    # Validate that a triangle is possible with these side lengths
    a <- input$side_a
    b <- input$side_b
    c <- input$side_c
    
    # Triangle inequality theorem
    valid_triangle <- (a + b > c) && (b + c > a) && (a + c > b)
  })
  
  ## Calculate perimeter ----
  output$perimeter <- renderInline({
    req(is_valid_triangle())
    input$side_a + input$side_b + input$side_c
  })
  
  ## Calculate semi-perimeter ----
  semi_perimeter <- reactive({
    (input$side_a + input$side_b + input$side_c) / 2
  })
  
  ## Display semi-perimeter ----
  output$semi_perimeter <- renderInline({
    req(is_valid_triangle())
    semi_perimeter()
  })
  
  ## Calculate area using Heron's formula ----
  output$area <- renderInline({
    req(is_valid_triangle())
    s <- semi_perimeter()
    sqrt(s * (s - input$side_a) * (s - input$side_b) * (s - input$side_c))
  })
  
  ## Render the triangle plot ----
  output$triangle_plot <- renderPlot({
    req(input$side_a, input$side_b, input$side_c)
    # Validate that a triangle is possible with these side lengths
    a <- input$side_a
    b <- input$side_b
    c <- input$side_c
    
    if (!is_valid_triangle()) {
      # Show error plot if triangle is impossible
      ggplot() + 
        annotate("text", x = 0.5, y = 0.5, 
                 label = "Invalid triangle:\nSides do not satisfy triangle inequality",
                 size = 6) +
        theme_void() +
        xlim(0, 1) + ylim(0, 1)
    } else {
      # Calculate coordinates
      coords <- calculate_triangle_coords(a, b, c)
      
      # Calculate centroid for area label
      centroid_x <- mean(coords$x[1:3])
      centroid_y <- mean(coords$y[1:3])
      
      # Calculate midpoints for side labels
      mid_ab_x <- (coords$x[1] + coords$x[2]) / 2
      mid_ab_y <- (coords$y[1] + coords$y[2]) / 2
      mid_bc_x <- (coords$x[2] + coords$x[3]) / 2
      mid_bc_y <- (coords$y[2] + coords$y[3]) / 2
      mid_ca_x <- (coords$x[3] + coords$x[1]) / 2
      mid_ca_y <- (coords$y[3] + coords$y[1]) / 2
      
      # Create the plot
      ggplot() +
        # Draw triangle
        geom_polygon(aes(x = coords$x, y = coords$y), 
                     fill = "lightblue", alpha = 0.3, color = "blue") +
        # Add side labels
        annotate("text", x = mid_ab_x, y = mid_ab_y - 0.3, 
                 label = sprintf("a = %.1f", a)) +
        annotate("text", x = mid_bc_x + 1, y = mid_bc_y, 
                 label = sprintf("b = %.1f", b)) +
        annotate("text", x = mid_ca_x - 1, y = mid_ca_y, 
                 label = sprintf("c = %.1f", c)) +
        # Add area label in center
        annotate("text", x = centroid_x, y = centroid_y,
                 label = sprintf("Area = %.1f", 
                                 sqrt(semi_perimeter() * 
                                        (semi_perimeter() - a) * 
                                        (semi_perimeter() - b) * 
                                        (semi_perimeter() - c)))) +
        # Make the plot look nice
        coord_equal() +
        theme_minimal() +
        theme(axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank())
    }
  })
}

# Run the application ----
shinyApp(ui = ui, server = server)
