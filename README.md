
# shinyRadioMatrix

## Introduction

This package provides one new R Shiny component: radioMatrixInput. It allows to create an assignment matrix which serves to encode relationships between entities of two classes.

There is often a need to encode relationships between entities in two different classes. A certain case of this is when a particular entity of one class can be also belong to multiple entities from another class at the same time, but reversing the assignment is not necessarily true. This tool is dedicated to these cases.

This is a taxon-PFT matrix that is required for performing a PFT-based climate reconstruction (see Peyron et al. 1998):

               bs bs/aa bec bec/ctc ctc ec ts/bs/aa ts/bs ts ts1 ts2 wte
       *Abies*  o   o    o     o     o   o     o      o    o  o   o   o
       *Alnus*  o   o    o     o     o   o     o      o    o  o   o   o
      *Betula*  o   o    o     o     o   o     o      o    o  o   o   o
    *Castanea*  o   o    o     o     o   o     o      o    o  o   o   o
       *Fagus*  o   o    o     o     o   o     o      o    o  o   o   o
       *Larix*  o   o    o     o     o   o     o      o    o  o   o   o
       *Picea*  o   o    o     o     o   o     o      o    o  o   o   o
      

### Reference

Peyron O, Guiot J, Cheddadi R, Tarasov P, Reille M, de Beaulieu J-L, Bottema S, Andrieu V (1998) Climatic Reconstruction in Europe for 18,000 YR B.P. from Pollen Data. Quat Res 49(2):183–196. DOI: [10.1006/qres.1997.1961](https://www.cambridge.org/core/journals/quaternary-research/article/abs/climatic-reconstruction-in-europe-for-18000-yr-bp-from-pollen-data/DD0EEDC0186456AC8ED1E3937EC9239E)

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
