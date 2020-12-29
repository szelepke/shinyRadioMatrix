#' Taxon List
#'
#' @name exTaxonList
#' @aliases exTaxonList
#' @docType data
#'
#' @description A taxon list, which gives the row names in a taxon-PFT matrix.
#'
#' @format A data frame with 31 rows and 2 columns:
#' \describe{
#'   \item{Var}{Unique identifier for the variable.}
#'   \item{VarName}{Name of the pollen type or variable, including Linnean and
#'       non-Linnean names (Acer saccharum-type, Cerealea, Avena/Triticum).}
#'   \item{DefPFT}{Default PFT assignment for the variable.}
#'  }
#'
#' @examples
#' data(exTaxonList)
#' str(exTaxonList)
#'
NULL

#' PFT List
#'
#' @name exPftList
#' @aliases exPftList
#' @docType data
#'
#' @description A PFT list, which gives the column names in a taxon-PFT matrix.
#'
#' @format A data frame with 31 rows and 2 columns:
#' \describe{
#'   \item{ID}{Unique identifier for the plant functional types (PFTs).}
#'   \item{Name}{Name of the PFT.}
#'  }
#'
#' @examples
#' data(exPftList)
#' str(exPftList)
#'
NULL
