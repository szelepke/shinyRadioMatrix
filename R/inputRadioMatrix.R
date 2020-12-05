#' Creates a single row for radioMatrixInput
#'
#' @param rowId character Arbitrary id for the row. It should be unique within a
#'   given radioMatrixInput, since it is used when identifying the value user
#'   has selected. It will be put into the \code{name} attribute of the
#'   corresponding \code{<tr>} tag, as well as in the \code{name} attributes of
#'   the radio button inputs in this row.
#' @param rowLabel character. Displayed col label
#' @param rowName character. Displayed col name of the leftmost
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
#'   be selected when the component is created
#'
#' @return HTML markup for a table row with radio buttons inputs inside each
#'   cell
#'
#' @noRd

generateRadioRow <- function(rowId, rowLabel, rowName, choiceNames, choiceValues,
                             selected = NULL, labelsWidth = list(NULL,NULL), headerRow = list("ID", "Basic Evaluation Index")){


  row_dat <- mapply(choiceNames, choiceValues, FUN = function(name, value){

    inputTag <- shiny::tags$input(type = "radio", name = rowId,
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

  row_dat <- list(if (is.null(style)) shiny::tags$td(rowLabel) else shiny::tags$td(rowLabel, style = style),
              if (is.null(style)) shiny::tags$td(rowName) else shiny::tags$td(rowName, style = style),
              row_dat)

  shiny::tags$tr(name = rowId,
                 class = "shiny-radiomatrix-row", # used for CSS styling
    row_dat)
}


#' Generate the header row of radioMatrixInput
#'
#' @param choiceNames character. Names for each option to be displayed on top of the table
#'
#' @return HTML markup for the header table row
#'
#' @noRd

generateRadioMatrixHeader <- function(choiceNames, headerRow = list("ID", "Basic Evaluation Index")){
  if(is.null(headerRow[[1]])){
    headerRowLabelOne = "ID"
  } else {
    headerRowLabelOne = headerRow[[1]]
  }
  if(!is.null(headerRow[[2]])){
    headerRowLabelTwo = "Basic Evaluation Index"
  } else {
    headerRowLabelTwo = headerRow[[2]]
  }
  header <- lapply(c(headerRowLabelOne, headerRowLabelTwo, choiceNames), function(n){
    shiny::tags$td(n)
  })

  shiny::tags$tr(header)
}


#' Generate complete HTML markup for radioMatrixIpnut
#'
#' @param inputId The input slot that will be used to access the value.
#' @param rowIds character. Vector of row identifiers. They will be used to find
#'   values that the user has selected. In the output, the component will return
#'   a named list of values, each name corresponding to the row id, and the
#'   value - to the value user has selected in this row.
#' @param rowLabels,rowNames character. Vectors of displayed labels of the
#'   leftmost and rightmost points of each row
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
#' @return HTML markup for the radioMatrixInput
#'
#' @noRd

generateRadioMatrix <- function (inputId, rowIds, rowLabels, rowNames,
                                 choiceNames = NULL, choiceValues = NULL,
                                 selected = NULL,
                                 labelsWidth = list(NULL,NULL),
                                 session = getDefaultReactiveDomain(),
                                 headerRow = list("ID", "Basic Evaluation Index")){

  header <- generateRadioMatrixHeader(choiceNames, headerRow)
  rows <- lapply(1:length(rowIds), function(i){
    generateRadioRow(rowId = rowIds[[i]], rowLabel = rowLabels[[i]], rowName = rowNames[[i]],
                     choiceNames = choiceNames, choiceValues = choiceValues,
                     selected = if (is.null(selected)) selected else selected[[i]],
                     labelsWidth = labelsWidth, headerRow = headerRow)
  })

  table <- shiny::tags$table(header, rows)

  shiny::div(class = "shiny-radiomatrix", table)
}

validateParams <- function(rowIds, rowLabels, rowNames, selected, choiceNames, labelsWidth){

  if (is.null(selected)){
    checks <- list(rowIds, rowLabels, rowNames)
  } else {
    checks <- list(rowIds, rowLabels, rowNames, selected)
  }

  lengths <- sapply(checks, length)

  if (length(unique(lengths)) > 1) {
    stop("All of rowIds, rowLabels, rowNames, selected should be of the same length!")
  }

  if (length(rowIds) < 1 ){
    stop("The radio matrix should contain at least  one row (i.e. at least one rowId must be specified)")
  }

  if(length(unique(rowIds)) < length(rowIds)){
    stop(paste("Some of the rowIds are not unique! The following values are duplicated:",
               rowIds[duplicated(rowIds)]))
  }

  if (length(choiceNames) < 2){
    stop("There should be at least two columns in the radio matrix (i.e. at least two choiceNames specified)")
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
#' @param rowIds character. Vector of row identifiers. They will be used to find
#'   values that the user has selected. In the output, the component will return
#'   a named list of values, each name corresponding to the row id, and the
#'   value - to the value user has selected in this row.
#' @param rowLabels,rowNames character. Vectors of displayed labels of the
#'   leftmost and rightmost points of each row
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
#' @export

radioMatrixInput <- function(inputId, rowIds, rowLabels, rowNames, choices = NULL,
                             selected = NULL, choiceNames = NULL, choiceValues = NULL,
                             labelsWidth = list(NULL,NULL), headerRow = list("ID", "Basic Evaluation Index")) {

  # check the inputs
  args <- shiny:::normalizeChoicesArgs(choices, choiceNames, choiceValues)
  selected <- shiny:::restoreInput(id = inputId, default = selected)
  validateParams(rowIds, rowLabels, rowNames, selected, args$choiceNames,labelsWidth)

  # generate the HTML for the controller itself
  radiomatrix <- generateRadioMatrix(inputId = inputID, rowIds = rowIds,
                                     rowLabels = rowLabels, rowNames = rowNames,
                                     selected = selected,
                                     choiceNames = args$choiceNames, choiceValues = args$choiceValues,
                                     labelsWidth = labelsWidth, headerRow = headerRow)

  divClass <- "form-group shiny-radiomatrix-container dataTable-container"

  # Make sure that the js and css files are locatable
  shiny::addResourcePath("radiomatrix", system.file(package="shinyRadioMatrix"))

  shiny::tagList(
    shiny::tags$head(
      shiny::singleton(shiny::tags$script(src = "radiomatrix/inputRadioMatrixBinding.js")),
      #shiny::singleton(shiny::tags$script(src = "radiomatrix/inputRadioMatrixUtils.js")),
      shiny::singleton(shiny::tags$link(rel = "stylesheet", type = "text/css",
                                        href = "radiomatrix/inputRadioMatrixCss.css"))

      ),

    # Wrp controllerin a div and return
    shiny::tags$div(id = inputId,
                    class = divClass,
                    radiomatrix
    )
  )

}
