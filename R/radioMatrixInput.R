#' Creates a single row for radioMatrixInput
#'
#' @param rowID character string. Unique ID for the row. It should be unique within 
#'   a given 'radioMatrixInput', since it is used when identifying the value user
#'   has selected. It will be put into the \code{name} attribute of the
#'   corresponding \code{<tr>} tag, as well as in the \code{name} attributes of
#'   the radio button inputs in this row.
#' @param rowLLabel character string. A label displayed in the leftmost point of the row.
#' @param rowRLabel character string. A label displayed in the rightmost point of the row.
#' @param choiceNames,choiceValues List of names and values, respectively, that 
#'   are displayed to the user in the app and correspond to the each choice (for 
#'   this reason, the objects 'choiceNames' and 'choiceValues' must have the 
#'   same length). If either of these arguments is provided, then the other must 
#'   be provided and choices must not be provided. The advantage of using both of 
#'   these over a named list for choices is that the object 'choiceNames' allows 
#'   any type of UI object to be passed through (tag objects, icons, HTML code, 
#'   ...), instead of just simple text.
#' @param selected The initially selected values (if not specified then defaults 
#'   to \code{NULL}).
#' @param labelsWidth List of two valid values of CSS length unit. Each element 
#'   has to be a properly formatted CSS unit of length (e.g., (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}), specifying the minimum (first value) and 
#'   maximum (second value) width of the labels columns. The valid elements will 
#'   be written to the \code{style} attribute of the labels \code{td} tags.
#'
#' @return HTML markup for a table row with radio buttons inputs inside each
#'   cell
#'
#' @keywords internal
#'
#' @noRd
#'
generateRadioRow <- function(rowID, rowLLabel, rowRLabel, choiceNames, choiceValues,
                             selected = NULL, labelsWidth = list(NULL, NULL)){


  row_dat <- mapply(choiceNames, choiceValues, FUN = function(name, value){

    inputTag <- shiny::tags$input(type = "radio", name = rowID,
                                  title = value, # to provide tooltips with the value
                                  value = value)
    if (value %in% selected)
      inputTag$attribs$checked <- "checked"

    shiny::tags$td(inputTag)
  }, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  style <- NULL

  if(!is.null(labelsWidth[[1]])){
    style <- paste0(style, "min-width:", labelsWidth[[1]],";")
  }
  if(!is.null(labelsWidth[[2]])){
    style <- paste0(style, "max-width:", labelsWidth[[2]],";")
  }

  row_dat <- list(shiny::tags$td(rowID),
                  if (is.null(style)) shiny::tags$td(rowLLabel) else shiny::tags$td(rowLLabel, style = style),
                  row_dat,
                  if (!is.null(rowRLabel)) if (is.null(style)) shiny::tags$td(rowRLabel) else shiny::tags$td(rowRLabel, style = style)
                  )

  shiny::tags$tr(name = rowID,
                 class = "shiny-radiomatrix-row", # used for CSS styling
                 row_dat)
}


#' Generate the header row of radioMatrixInput
#'
#' @param choiceNames character. Names displayed on top of the assignment matrix.
#' @param rowLLabels character. Vector (or a matrix with one column) of labels that 
#'   displayed in the leftmost point of each row. The column name of the matrix 
#'   could be displayed in the header of the assignment matrix.
#' @param rowRLabels character. Vector (or a matrix with one column) of labels that 
#'   displayed in the rightmost point of each row. The column name of the matrix 
#'   could be displayed in the header of the assignment matrix. Using this argument 
#'   is optional. But it allows to create Likert scales, potentially with several 
#'   scales arranged in a matrix.
#'   
#' @return HTML markup for the header table row
#'
#' @keywords internal
#'
#' @noRd
#'
generateRadioMatrixHeader <- function(choiceNames, rowLLabels, rowRLabels){
  if(!is.null(rowRLabels)){
    rRName <- ifelse(is.matrix(rowRLabels), colnames(rowRLabels), "")
    rLName <- ifelse(is.matrix(rowLLabels), colnames(rowLLabels), "")
    header <- lapply(c("ID", rLName, choiceNames, rRName),
                     function(n){ shiny::tags$td(n)})
  } else {
    rLName <- ifelse(is.matrix(rowLLabels), colnames(rowLLabels), "")
    header <- lapply(c("ID", rLName, choiceNames),
                     function(n){ shiny::tags$td(n)})
  }

  shiny::tags$tr(header)
}


#' Generate complete HTML markup for radioMatrixIpnut
#'
#' @param inputId The input slot that will be used to access the value.
#' @param rowIDs character. Vector of row identifiers that will be used to find
#'   values that the user has selected. In the output, the component will return
#'   a named list of values, each name corresponding to the row id, and the
#'   value - to the value user has selected in this row.
#' @param rowLLabels character. Vector (or a matrix with one column) of labels that 
#'   displayed in the leftmost point of each row. The column name of the matrix 
#'   could be displayed in the header of the assignment matrix.
#' @param rowRLabels character. Vector (or a matrix with one column) of labels that 
#'   displayed in the rightmost point of each row. The column name of the matrix 
#'   could be displayed in the header of the assignment matrix. Using this argument 
#'   is optional. But it allows to create Likert scales, potentially with several 
#'   scales arranged in a matrix.
#' @param selected Vector of the initially selected values (if not specified then 
#'   defaults to \code{NULL}).
#' @param choiceNames,choiceValues List of names and values, respectively, that 
#'   are displayed to the user in the app and correspond to the each choice (for 
#'   this reason, the objects 'choiceNames' and 'choiceValues' must have the 
#'   same length). If either of these arguments is provided, then the other must 
#'   be provided and choices must not be provided. The advantage of using both of 
#'   these over a named list for choices is that the object 'choiceNames' allows 
#'   any type of UI object to be passed through (tag objects, icons, HTML code, 
#'   ...), instead of just simple text.
#' @param labelsWidth List of two valid values of CSS length unit. Each element 
#'   has to be a properly formatted CSS unit of length (e.g., (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}), specifying the minimum (first value) and 
#'   maximum (second value) width of the labels columns. The valid elements will 
#'   be written to the \code{style} attribute of the labels \code{td} tags.
#' @param session copied from \code{shiny:::generateOptions}
#'
#' @keywords internal
#'
#' @noRd
#'
generateRadioMatrix <- function (inputId, rowIDs, rowLLabels, rowRLabels = NULL,
                                 choiceNames = NULL, choiceValues = NULL,
                                 selected = NULL,
                                 labelsWidth = list(NULL, NULL),
                                 session = shiny::getDefaultReactiveDomain()){

  header <- generateRadioMatrixHeader(choiceNames, rowLLabels, rowRLabels)
  rows <- lapply(1:length(rowIDs), function(i){
    generateRadioRow(rowID = rowIDs[[i]], rowLLabel = rowLLabels[[i]], rowRLabel = rowRLabels[[i]],
                     choiceNames = choiceNames, choiceValues = choiceValues,
                     selected = if (is.null(selected)) selected else selected[[i]],
                     labelsWidth = labelsWidth)
  })

  table <- shiny::tags$table(header, rows)

  shiny::div(class = "shiny-radiomatrix", table)
}

#' @param inputId The input slot that will be used to access the value.
#' @param rowIDs character. Vector of row identifiers that will be used to find
#'   values that the user has selected. In the output, the component will return
#'   a named list of values, each name corresponding to the row id, and the
#'   value - to the value user has selected in this row.
#' @param rowLLabels character. Vector (or a matrix with one column) of labels that 
#'   displayed in the leftmost point of each row. The column name of the matrix 
#'   could be displayed in the header of the assignment matrix.
#' @param rowRLabels character. Vector (or a matrix with one column) of labels that 
#'   displayed in the rightmost point of each row. The column name of the matrix 
#'   could be displayed in the header of the assignment matrix. Using this argument 
#'   is optional. But it allows to create Likert scales, potentially with several 
#'   scales arranged in a matrix.
#' @param choices List of values to select from (if elements of the list are
#'   named then that name rather than the value is displayed to the user). If
#'   this argument is provided, then choiceNames and choiceValues must not be
#'   provided, and vice-versa. The values should be strings; other types (such
#'   as logicals and numbers) will be coerced to strings.
#' @param selected Vector of the initially selected values (if not specified then 
#'   defaults to \code{NULL}).
#' @param choiceNames,choiceValues List of names and values, respectively, that 
#'   are displayed to the user in the app and correspond to the each choice (for 
#'   this reason, the objects 'choiceNames' and 'choiceValues' must have the 
#'   same length). If either of these arguments is provided, then the other must 
#'   be provided and choices must not be provided. The advantage of using both of 
#'   these over a named list for choices is that the object 'choiceNames' allows 
#'   any type of UI object to be passed through (tag objects, icons, HTML code, 
#'   ...), instead of just simple text.
#' @param labelsWidth List of two valid values of CSS length unit. Each element 
#'   has to be a properly formatted CSS unit of length (e.g., (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}), specifying the minimum (first value) and 
#'   maximum (second value) width of the labels columns. The valid elements will 
#'   be written to the \code{style} attribute of the labels \code{td} tags.
#'
#' @keywords internal
#'
#' @noRd
#'
validateParams <- function(rowIDs, rowLLabels, rowRLabels, selected, choiceNames, labelsWidth){

  cv.inv <- ifelse(!is.null(rowRLabels), c("rowLLabels", "rowRLabels"), c("rowLLabels"))
  for (i_i in 1 : length(cv.inv)){
    if (!any((length(get(cv.inv[i_i])) >= 1 && !is.list(get(cv.inv[i_i]))),
             (length(dim(get(cv.inv[i_i]))) == 2 && dim(get(cv.inv[i_i]))[2L] == 1))) {
      stop("'", cv.inv[i_i], "' must be a a vector or a matrix with at least one column.")
    }
  }

  if (length(dim(rowLLabels)) == 2 && dim(rowLLabels)[2L] == 1) {
    rowLLabels <- array(t(rowLLabels))
  }

  if (length(dim(rowRLabels)) == 2 && dim(rowRLabels)[2L] == 1) {
    rowRLabels <- array(t(rowRLabels))
  }

  if (!is.null(rowRLabels) & !is.null(selected)) {
    checks <- list(rowIDs, rowLLabels, rowRLabels, selected)
  } else {
    if (is.null(rowRLabels) & is.null(selected)) {
      checks <- list(rowIDs, rowLLabels)
    } else {
      if (!is.null(rowRLabels)) {
        checks <- list(rowIDs, rowLLabels, rowRLabels)
      } else {
        checks <- list(rowIDs, rowLLabels, selected)
      }
    }
  }

  lengths <- sapply(checks, length)

  if (length(unique(lengths)) > 1) {
    stop("All elements of the object 'rowIDs', 'rowLabels' and 'selected' must be ", 
         "of the same length!")
  }

  if (length(rowIDs) < 1 ){
    stop("The assignment matrix should contain at least one row. ", 
         "The object 'rowIDs' has to be a vector with at least one element.")
  }

  if(length(unique(rowIDs)) < length(rowIDs)){
    stop(paste("Some elements of the object 'rowIDs' are not unique. ",
               "The following values are duplicated:", rowIDs[duplicated(rowIDs)]), ".")
  }

  if (length(choiceNames) < 1){
    stop("There should be at least one columns in the assignment matrix. ",
         "The object 'choiceNames' has to be a vector with at least one element.")
  }

  if (!(is.list(labelsWidth) & length(labelsWidth) == 2)){
    stop("The object 'labelsWidth' must be a list with two elements.")
  }

  pattern <-
    "^(auto|inherit|calc\\(.*\\)|((\\.\\d+)|(\\d+(\\.\\d+)?))(%|in|cm|mm|ch|em|ex|rem|pt|pc|px|vh|vw|vmin|vmax))$"
  
  is.cssu <- function(x) (is.character(x) && grepl(pattern, x))
  lwNull <- sapply(labelsWidth, is.null)
  lwCssU <- sapply(labelsWidth, is.cssu)
  lwTest <- !(lwNull | lwCssU)
  if (any(lwTest)){
    stop("The object 'labelsWidth' can only contain NULLs or ", 
         "properly formatted CSS units of length!")
  }

}


#' Create radioMatrixInput
#'
#' @param inputId The input slot that will be used to access the value.
#' @param rowIDs character. Vector of row identifiers that will be used to find
#'   values that the user has selected. In the output, the component will return
#'   a named list of values, each name corresponding to the row id, and the
#'   value - to the value user has selected in this row.
#' @param rowLLabels character. Vector (or a matrix with one column) of labels that 
#'   displayed in the leftmost point of each row. The column name of the matrix 
#'   could be displayed in the header of the assignment matrix.
#' @param rowRLabels character. Vector (or a matrix with one column) of labels that 
#'   displayed in the rightmost point of each row. The column name of the matrix 
#'   could be displayed in the header of the assignment matrix. Using this argument 
#'   is optional. But it allows to create Likert scales, potentially with several 
#'   scales arranged in a matrix.
#' @param choices List of values to select from (if elements of the list are
#'   named then that name rather than the value is displayed to the user). If
#'   this argument is provided, then choiceNames and choiceValues must not be
#'   provided, and vice-versa. The values should be strings; other types (such
#'   as logicals and numbers) will be coerced to strings.
#' @param selected Vector of the initially selected values (if not specified then 
#'   defaults to \code{NULL}).
#' @param choiceNames,choiceValues List of names and values, respectively, that 
#'   are displayed to the user in the app and correspond to the each choice (for 
#'   this reason, the objects 'choiceNames' and 'choiceValues' must have the 
#'   same length). If either of these arguments is provided, then the other must 
#'   be provided and choices must not be provided. The advantage of using both of 
#'   these over a named list for choices is that the object 'choiceNames' allows 
#'   any type of UI object to be passed through (tag objects, icons, HTML code, 
#'   ...), instead of just simple text.
#' @param labelsWidth List of two valid values of CSS length unit. Each element 
#'   has to be a properly formatted CSS unit of length (e.g., \code{'10\%'},
#'   \code{'40px'}, \code{'auto'}), specifying the minimum (first value) and 
#'   maximum (second value) width of the labels columns. The valid elements will 
#'   be written to the \code{style} attribute of the labels \code{td} tags.
#'
#' @return HTML markup for radioMatrixInput
#'
#' @examples
#' library(shiny)
#' library(shinyRadioMatrix)
#'
#'
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'   data(exTaxonList)
#'   data(exPftList)
#'
#'   ui <- fluidPage(
#'     radioMatrixInput(inputId = "rmi01", rowIDs = head(exTaxonList$Var),
#'            rowLLabels = head(as.matrix(subset(exTaxonList, select = "VarName"))),
#'            choices = exPftList$ID,
#'            selected = head(exTaxonList$DefPFT)),
#'     verbatimTextOutput('debug01')
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$debug01 <- renderPrint({input$rmi01})
#'   }
#'
#'   shinyApp(ui, server)
#' }
#' 
#' if (interactive()) {
#'
#'   ui <- fluidPage(
#'
#'     radioMatrixInput(inputId = "rmi02", rowIDs = c("Performance", "Statement A"),
#'                      rowLLabels = c("Poor", "Agree"),
#'                      rowRLabels = c("Excellent", "Disagree"),
#'                      choices = 1:5,
#'                      selected = rep(3, 2),
#'                      labelsWidth = list("100px", "100px")),
#'     verbatimTextOutput('debug02')
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$debug02 <- renderPrint({input$rmi02})
#'   }
#'
#'   shinyApp(ui, server)
#'
#' }
#' 
#' @export
#'
radioMatrixInput <- function(inputId, rowIDs, rowLLabels, rowRLabels = NULL, choices = NULL,
                             selected = NULL, choiceNames = NULL, choiceValues = NULL,
                             labelsWidth = list(NULL, NULL)) {

  # check the inputs
  args <- eval(parse(text = "shiny:::normalizeChoicesArgs(choices, choiceNames, choiceValues)"))
  selected <- eval(parse(text = "restoreInput(id = inputId, default = selected)"))
  validateParams(rowIDs, rowLLabels, rowRLabels, selected, args$choiceNames, labelsWidth)

  # generate the HTML for the controller itself
  radiomatrix <- generateRadioMatrix(inputId = inputId, rowIDs = rowIDs,
                                     rowLLabels = rowLLabels, rowRLabels = rowRLabels,
                                     selected = selected,
                                     choiceNames = args$choiceNames, choiceValues = args$choiceValues,
                                     labelsWidth = labelsWidth)

  divClass <- "form-group shiny-radiomatrix-container dataTable-container"

  # Make sure that the js and css files are locatable
  shiny::addResourcePath("radiomatrix", system.file(package="shinyRadioMatrix"))

  shiny::tagList(
    shiny::tags$head(
      shiny::singleton(shiny::tags$script(src = "radiomatrix/inputRadioMatrixBinding.js")),
      shiny::singleton(shiny::tags$link(rel = "stylesheet", type = "text/css",
                                        href = "radiomatrix/inputRadioMatrixCss.css"))

    ),

    shiny::tags$div(id = inputId,
                    class = divClass,
                    radiomatrix
    )
  )

}
