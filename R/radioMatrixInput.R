#' Creates a single row for radioMatrixInput
#'
#' @param rowID character. Arbitrary id for the row. It should be unique within a
#'   given radioMatrixInput, since it is used when identifying the value user
#'   has selected. It will be put into the \code{name} attribute of the
#'   corresponding \code{<tr>} tag, as well as in the \code{name} attributes of
#'   the radio button inputs in this row.
#' @param rowLLabel character. Displayed labels of the leftmost points of the row.
#' @param rowRLabel character. Displayed labels of the rightmost points of the row.
#' @param choiceNames,choiceValues as in radioButtons. Repeated here: List of
#'   names and values, respectively, that are displayed to the user in the app
#'   and correspond to the each choice (for this reason, choiceNames and
#'   choiceValues must have the same length). If either of these arguments is
#'   provided, then the other must be provided and choices must not be provided.
#'   The advantage of using both of these over a named list for choices is that
#'   choiceNames allows any type of UI object to be passed through (tag objects,
#'   icons, HTML code, ...), instead of just simple text.
#' @param labelsWidth - vector of two values, NULL by default. Each value can be
#'   replaced with a character, specifying the minimum (first value) and maximum
#'   (second value) width of the labels columns. The values are assumed contain
#'   the width itself and the unit, e.g. "20px", and will be written to the
#'   \code{style} attribute of the labels \code{td} tags.
#' @param selected either NULL (defualt) or the name of the value which should
#'   be selected when the component is created.
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
#' @param choiceNames character. Names for each option to be displayed on top of the table
#' @param rowLLabel character. Displayed labels of the leftmost points of the row.
#' @param rowRLabel character. Displayed labels of the rightmost points of the row.
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
#' @param rowIDs character. Vector of row identifiers. They will be used to find
#'   values that the user has selected. In the output, the component will return
#'   a named list of values, each name corresponding to the row id, and the
#'   value - to the value user has selected in this row.
#' @param rowLLabels character. Vector (or a matrix with one column) of displayed
#'   labels of the leftmost points of each row. The column name could be displayed
#'   in the header of the matrix.
#' @param rowRLabels character. Vector (or a matrix with one column) of displayed
#'   labels of the rightmost points of each row. The column name could be displayed
#'   in the header of the matrix. Using this argument is optional. But it allows
#'   to create Likert scales, potentially with several scales arranged in a matrix.
#' @param selected either \code{NULL} (default) or a vector of values which
#'   should be selected when the component is created
#' @param choiceNames,choiceValues as in radioButtons. Repeated here: List of
#'   names and values, respectively, that are displayed to the user in the app
#'   and correspond to the each choice (for this reason, choiceNames and
#'   choiceValues must have the same length). If either of these arguments is
#'   provided, then the other must be provided and choices must not be provided.
#'   The advantage of using both of these over a named list for choices is that
#'   choiceNames allows any type of UI object to be passed through (tag objects,
#'   icons, HTML code, ...), instead of just simple text.
#' @param labelsWidth - vector of two values, NULL by default. Each value can be
#'   replaced with a character, specifying the minimum (first value) and maximum
#'   (second value) width of the labels columns. The values are assumed contain
#'   the width itself and the unit, e.g. "20px", and will be written to the
#'   \code{style} attribute of the labels \code{td} tags.
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
#' @param rowIDs character. Vector of row identifiers. They will be used to find
#'   values that the user has selected. In the output, the component will return
#'   a named list of values, each name corresponding to the row id, and the
#'   value - to the value user has selected in this row.
#' @param rowLLabels character. Vector (or a matrix with one column) of displayed
#'   labels of the leftmost points of each row. The column name could be displayed
#'   in the header of the matrix.
#' @param rowRLabels character. Vector (or a matrix with one column) of displayed
#'   labels of the rightmost points of each row. The column name could be displayed
#'   in the header of the matrix. Using this argument is optional. But it allows
#'   to create Likert scales, potentially with several scales arranged in a matrix.
#' @param choices List of values to select from (if elements of the list are
#'   named then that name rather than the value is displayed to the user). If
#'   this argument is provided, then choiceNames and choiceValues must not be
#'   provided, and vice-versa. The values should be strings; other types (such
#'   as logicals and numbers) will be coerced to strings.
#' @param selected either \code{NULL} (default) or a vector of values which
#'   should be selected when the component is created
#' @param choiceNames,choiceValues as in radioButtons. Repeated here: List of
#'   names and values, respectively, that are displayed to the user in the app
#'   and correspond to the each choice (for this reason, choiceNames and
#'   choiceValues must have the same length). If either of these arguments is
#'   provided, then the other must be provided and choices must not be provided.
#'   The advantage of using both of these over a named list for choices is that
#'   choiceNames allows any type of UI object to be passed through (tag objects,
#'   icons, HTML code, ...), instead of just simple text.
#' @param labelsWidth - vector of two values, NULL by default. Each value can be
#'   replaced with a character, specifying the minimum (first value) and maximum
#'   (second value) width of the labels columns. The values are assumed contain
#'   the width itself and the unit, e.g. "20px", and will be written to the
#'   \code{style} attribute of the labels \code{td} tags.
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
      stop(cv.inv[i_i], " must be a a vector or a matrix-like object with at least one column.")
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
    stop("All of rowIDs, rowLabels and selected must be of the same length!")
  }

  if (length(rowIDs) < 1 ){
    stop("The radio matrix should contain at least one row (i.e. at least one rowID must be specified).")
  }

  if(length(unique(rowIDs)) < length(rowIDs)){
    stop(paste("Some of the rowIDs are not unique! The following values are duplicated:",
               rowIDs[duplicated(rowIDs)]), ".")
  }

  if (length(choiceNames) < 2){
    stop("There should be at least two columns in the radio matrix (i.e. at least two choiceNames specified).")
  }

  if (!is.list(labelsWidth)){
    stop("labelsWidth must be a list!")
  }

  lwNull <- sapply(labelsWidth, is.null)
  lwChar <- sapply(labelsWidth, is.character)
  lwTest <- !(lwNull | lwChar)
  if (any(lwTest)){
    stop("labelsWidth can only contain NULLs or characters!")
  }

}


#' Create radioMatrixInput
#'
#' @param inputId The input slot that will be used to access the value.
#' @param rowIDs character. Vector of row identifiers. They will be used to find
#'   values that the user has selected. In the output, the component will return
#'   a named list of values, each name corresponding to the row id, and the
#'   value - to the value user has selected in this row.
#' @param rowLLabels character. Vector (or a matrix with one column) of displayed
#'   labels of the leftmost points of each row. The column name could be displayed
#'   in the header of the matrix.
#' @param rowRLabels character. Vector (or a matrix with one column) of displayed
#'   labels of the rightmost points of each row. The column name could be displayed
#'   in the header of the matrix. Using this argument is optional. But it allows
#'   to create Likert scales, potentially with several scales arranged in a matrix.
#' @param choices List of values to select from (if elements of the list are
#'   named then that name rather than the value is displayed to the user). If
#'   this argument is provided, then choiceNames and choiceValues must not be
#'   provided, and vice-versa. The values should be strings; other types (such
#'   as logicals and numbers) will be coerced to strings.
#' @param selected either \code{NULL} (default) or a vector of values which
#'   should be selected when the component is created
#' @param choiceNames,choiceValues as in radioButtons. Repeated here: List of
#'   names and values, respectively, that are displayed to the user in the app
#'   and correspond to the each choice (for this reason, choiceNames and
#'   choiceValues must have the same length). If either of these arguments is
#'   provided, then the other must be provided and choices must not be provided.
#'   The advantage of using both of these over a named list for choices is that
#'   choiceNames allows any type of UI object to be passed through (tag objects,
#'   icons, HTML code, ...), instead of just simple text.
#' @param labelsWidth - vector of two values, NULL by default. Each value can be
#'   replaced with a character, specifying the minimum (first value) and maximum
#'   (second value) width of the labels columns. The values are assumed contain
#'   the width itself and the unit, e.g. "20px", and will be written to the
#'   \code{style} attribute of the labels \code{td} tags.
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
#'  data(taxon_list)
#'  data(pft_list)
#'
#'  ui <- fluidPage(
#'    radioMatrixInput(inputId = "rmi", rowIDs = taxon_list$Var,
#'           rowLLabels = as.matrix(subset(taxon_list, select = "VarName")),
#'          choices = pft_list$ID,
#'           selected = taxon_list$DefPFT),
#'    verbatimTextOutput('debug')
#'  )
#'
#'  server <- function(input, output, session) {
#'    output$debug <- renderPrint({input$rmi})
#'  }
#'
#'  shinyApp(ui, server)
#' }
#' if (interactive()) {
#'
#'   ui <- fluidPage(
#'
#'     radioMatrixInput(inputId = "rmi", rowIDs = c("Performance", "Statement A"),
#'                      rowLLabels = c("Poor", "Agree"),
#'                      rowRLabels = c("Excellent", "Disagree"),
#'                      choices = 0:10,
#'                      selected = rep(5, 2)),
#'     verbatimTextOutput('debug')
#'   )
#'
#'   server <- function(input, output, session) {
#'     output$debug <- renderPrint({input$rmi})
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
