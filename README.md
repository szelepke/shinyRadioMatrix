
# shinyRadioMatrix

## Introduction

This package provides one new R Shiny component: radioMatrixInput. It allows to create an assignment matrix which serves to encode relationships between entities of two classes.

There is often a need to encode relationships between entities in two different classes. A certain case of this is when a particular entity of one class can be also belong to multiple entities from another class at the same time, but reversing the assignment is not necessarily true. This tool is dedicated to these cases.

This is a taxon-PFT matrix that is required for implementing the so-called biomization procedure (see Prentice et al. 1996):

             aa aa/bs bs ts sf
    Ambrosia  o    o   o  o  o
      Betula  o    o   o  o  o
       Picea  o    o   o  o  o
      

###Reference

Prentice IC, Guiot J, Huntley B, Jolly D, Cheddadi R (1996) Reconstructing biomes from palaeoecological data: a general method and its application to European pollen data at 0 and 6 ka. Clim Dyn 12(3):185–194. DOI: [10.1007/BF00211617] (https://link.springer.com/article/10.1007%2FBF00211617)

## Installation

radioMatrixInput can be used from your browser with your current R installation. The package can be installed in the following way:

``` r
if (!require(devtools)) install.packages("devtools")
devtools::install_github("szelepke/shinyRadioMatrix")
```


## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(shiny)
library(shinyRadioMatrix)

ui <- fluidPage(

  radioMatrixInput(inputId = "rmi", rowIDs = letters[1:16],
                   rowLLabels = letters[1:16], choices = 1:10,
                   selected = rep(c(1,2), each = 8)),
  verbatimTextOutput('debug')
  )

server <- function(input, output, session) {
  output$debug <- renderPrint({input$rmi})
}

shinyApp(ui, server)
```

