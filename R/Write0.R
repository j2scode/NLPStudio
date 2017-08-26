## ---- Write0
#==============================================================================#
#                                   Write0                                     #
#==============================================================================#
#' Write0
#'
#'
#' \code{Write0} Class as in abstract class to the WriteBin, WriteText, WriteCsv, WriteRdata write strategies
#'
#' This class is an abstract class defining the methods to be implemented by
#' the concrete strategy classes: WriteText, WriteBin, WriteCsv, and WriteRdata.
#'
#' @section Parameters:
#' @param document An object of class Document
#' @param content A object of class character, data frame, or quanteda
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Returns an object of the selected concrete write strategy class. This method is overwritten by the concrete write strategy classes}
#'  \item{\code{writeData(content)}}{Writes the content to the document as per the document object definition. This method is overwritten by the concrete write strategy classes}
#' }
#'
#' @docType class
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
Write0 <- R6::R6Class(
  classname = "Write0",
  private = list(
    validate = function(document) stop("Method not available for abstract classes.")
  ),
  public = list(
    writeData = function() stop("Method not available for abstract classes.")
  )
)
