## ---- Read0
#==============================================================================#
#                                   Read0                                      #
#==============================================================================#
#' Read0
#'
#'
#' \code{Read0} Class as in abstract class to the ReadBin, ReadText, ReadCsv, ReadRdata read strategies
#'
#' This class is an abstract class defining the methods to be implemented by
#' the concrete strategy classes: ReadText, ReadBin, ReadCsv, and ReadRdata.
#'
#' @section Parameters:
#' @param document An object of class Document
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Returns an object of the selected concrete read strategy class. This method is overwritten by the concrete strategy classes}
#'  \item{\code{readData(document)}}{Returns a list containing the meta data and content}
#' }
#'
#' @docType class
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
Read0 <- R6::R6Class(
  classname = "Read0",
  private = list(
    validate = function(document) stop("Method not available for abstract classes.")
  ),
  public = list(
    readData = function() stop("Method not available for abstract classes.")
  )
)
