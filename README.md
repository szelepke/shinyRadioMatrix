# shinyRadioMatrix

This package provides one new R Shiny component: `radioMatrixInput`. It allows to create Likert scales, potentially with several scales arranged in a matrix like this:
        
          1 2 3 4
      Bad o o o o Good
    False o o o o True
    
The code is very much based on the base Shiny `radioButton` input, and has similar controller options. For details see the package documentation.

This component returns a named list of values, where each name corresponds to a row ID provided at component creation, and each value - to a value the user has selected. 

The code is in development and has not been tested yet.

## Installation

`devtools::install_github("szelepke/shinyRadioMatrix")`

## Example app

```
library(shiny)

ui <- fluidPage(

  radioMatrixInput(inputId = "rmi", rowIds = letters[1:16],
                   minLabels = letters[1:16], maxLabels = LETTERS[1:16],
                   choices = 1:10,
                   selected = rep(c(1,2), each = 8)),
  verbatimTextOutput('debug')
  )

server <- function(input, output, session) {
  output$debug <- renderPrint({input$rmi})
}

shinyApp(ui, server)


```