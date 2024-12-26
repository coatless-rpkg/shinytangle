#' Create an inline numeric input field
#'
#' Creates a numeric input field that can be embedded inline within text. The field
#' supports mouse drag and scroll wheel for value changes.
#'
#' @param inputId The input identifier used to access the value in server logic
#' @param value Initial value
#' @param min Minimum value allowed (NA for no minimum)
#' @param max Maximum value allowed (NA for no maximum)
#' @param step Step size for increments/decrements
#' @param sensitivity Drag sensitivity multiplier
#'
#' @return 
#' A Shiny tag list containing the input element and its dependencies
#' 
#' @examples
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   p("Let x = ", inlineNumericInput("x"))
#' )
#' 
#' server <- function(input, output) {
#'   # Empty
#' }
#' 
#' if (interactive()) {
#'  shinyApp(ui, server)
#' }
#' @export
inlineNumericInput <- function(inputId,
                           value = 0,
                           min = NA,
                           max = NA,
                           step = 0.1,
                           sensitivity = 0.1) {
  
  # Determine if we should use integer formatting
  is_integer_input <- is.numeric(step) && step == floor(step) && 
    is.numeric(value) && value == floor(value)
  
  # Format value based on whether it's integer or decimal
  formatted_value <- if (is_integer_input) {
    format(round(value))
  } else {
    format(value, nsmall = 1)
  }
  
  # Create container with dependencies and input
  shiny::tagList(
    .ensure_inline_dependencies(sensitivity),
    shiny::tags$input(
      type = "number",
      id = inputId,
      class = "inline-interactive-number",
      value = formatted_value,
      min = if (!is.na(min)) min,
      max = if (!is.na(max)) max,
      step = step
    )
  )
}

#' Create an inline output span
#'
#' Creates an output element that can be embedded inline within text.
#'
#' @param outputId The output identifier used to update the value from server logic
#'
#' @return 
#' A Shiny UI output element configured for inline display
#' 
#' @export
#' @examples
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   p("The result is", inlineOutput("result"))
#' )
#' 
#' server <- function(input, output) {
#'   output$result <- renderInline({
#'     # Will be formatted to one decimal place
#'     42.123
#'   })
#' }
#' 
#' if (interactive()) {
#'  shinyApp(ui, server)
#' }
inlineOutput <- function(outputId) {
  shiny::uiOutput(outputId, inline = TRUE)
}

#' Create an inline render function
#'
#' Creates a render function for inline outputs that automatically formats numeric
#' values appropriately (integers without decimals, other numbers to one decimal place).
#'
#' @param expr Expression to evaluate
#' @param env Environment to evaluate in
#' @param quoted Whether the expression is quoted
#'
#' @return 
#' A Shiny render function that creates inline output elements
#' @export
#' @examples
#' library(shiny)
#' 
#' ui <- fluidPage(
#'   p("The result is", inlineOutput("result"))
#' )
#' 
#' server <- function(input, output) {
#'   output$result <- renderInline({
#'     # Will be formatted to one decimal place
#'     42.123
#'   })
#' }
#' 
#' if (interactive()) {
#'  shinyApp(ui, server)
#' }
renderInline <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  
  func <- shiny::exprToFunction(expr, env)
  
  shiny::renderUI({
    value <- func()
    if (is.null(value)) return(NULL)
    
    formatted_value <- if (is.numeric(value)) {
      if (value == floor(value)) {
        # Integer values
        format(round(value))
      } else {
        # Decimal values
        format(round(value, 1), nsmall = 1)
      }
    } else {
      as.character(value)
    }
    
    shiny::tags$span(
      class = "inline-interactive-output",
      formatted_value
    )
  })
}

# Internal function to add required dependencies
.ensure_inline_dependencies <- function(sensitivity = 0.1) {
  shiny::singleton(
    shiny::tags$head(
      # CSS styling remains the same
      shiny::tags$style("
        input[type='number'].inline-interactive-number {
          background: #f0f0f0;
          padding: 2px 5px;
          border-radius: 3px;
          color: #2196F3;
          font-weight: bold;
          cursor: ew-resize;
          display: inline-block;
          border: none;
          width: 4em;
          text-align: right;
          -moz-appearance: textfield;
          -webkit-appearance: none;
          font-size: inherit;
          font-family: inherit;
        }
        
        input[type='number'].inline-interactive-number::-webkit-inner-spin-button,
        input[type='number'].inline-interactive-number::-webkit-outer-spin-button {
          -webkit-appearance: none;
          margin: 0;
        }
        
        .inline-interactive-number:hover {
          background: #e0e0e0;
        }
        
        .inline-interactive-number.dragging {
          background: #d0d0d0;
        }
        
        .inline-interactive-output {
          background: #f8f8f8;
          padding: 2px 5px;
          border-radius: 3px;
          color: #666;
          display: inline-block;
          min-width: 2em;
          text-align: right;
        }
      "),
      
      # Modified JavaScript to handle integer formatting
      shiny::tags$script(sprintf("$(document).ready(function() {
          var isDragging = false;
          var startY;
          var startValue;
          var sensitivity = %f;
          var $activeInput = null;
          
          $('.inline-interactive-number').on('mousedown touchstart', function(e) {
            isDragging = true;
            $activeInput = $(this);
            startY = e.type === 'mousedown' ? e.pageY : e.touches[0].pageY;
            startValue = parseFloat($activeInput.val()) || 0;
            $('body').css('cursor', 'ew-resize');
            $activeInput.addClass('dragging');
            e.preventDefault();
            return false;
          });
          
          $(document).on('mousemove touchmove', function(e) {
            if (!isDragging || !$activeInput) return;
            
            var currentY = e.type === 'mousemove' ? e.pageY : e.touches[0].pageY;
            var deltaY = startY - currentY;
            var newValue = startValue + (deltaY * sensitivity);
            
            var min = parseFloat($activeInput.attr('min'));
            var max = parseFloat($activeInput.attr('max'));
            var step = parseFloat($activeInput.attr('step')) || 1;
            
            if (!isNaN(min)) newValue = Math.max(min, newValue);
            if (!isNaN(max)) newValue = Math.min(max, newValue);
            newValue = Math.round(newValue / step) * step;
            
            // Format based on step size
            var formattedValue = step === 1 
              ? Math.round(newValue).toString()
              : newValue.toFixed(1);
            
            $activeInput.val(formattedValue);
            Shiny.setInputValue($activeInput.attr('id'), newValue);
            
            e.preventDefault();
            return false;
          });
          
          $(document).on('mouseup touchend', function(e) {
            if (isDragging) {
              isDragging = false;
              $activeInput.removeClass('dragging');
              $activeInput = null;
              $('body').css('cursor', 'default');
              e.preventDefault();
            }
          });
          
          $('.inline-interactive-number').on('wheel', function(e) {
            var $input = $(this);
            var step = parseFloat($input.attr('step')) || 1;
            var delta = e.originalEvent.deltaY < 0 ? step : -step;
            var currentValue = parseFloat($input.val()) || 0;
            var newValue = currentValue + delta;
            
            var min = parseFloat($input.attr('min'));
            var max = parseFloat($input.attr('max'));
            if (!isNaN(min)) newValue = Math.max(min, newValue);
            if (!isNaN(max)) newValue = Math.min(max, newValue);
            
            // Format based on step size
            var formattedValue = step === 1
              ? Math.round(newValue).toString()
              : newValue.toFixed(1);
            
            $input.val(formattedValue);
            Shiny.setInputValue($input.attr('id'), newValue);
            
            e.preventDefault();
            return false;
          });
          
          // Handle direct input changes
          $('.inline-interactive-number').on('input change', function(e) {
            var $input = $(this);
            var value = parseFloat($input.val()) || 0;
            var step = parseFloat($input.attr('step')) || 1;
            
            // Format based on step size
            var formattedValue = step === 1
              ? Math.round(value).toString()
              : value.toFixed(1);
            
            $input.val(formattedValue);
            Shiny.setInputValue($input.attr('id'), value);
          });
        });
      ", sensitivity))
    )
  )
}
