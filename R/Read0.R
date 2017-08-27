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
#' @param path Character string indicating the path to the document to be read
#' @param header Logical indicating whether the csv file contains headers.  Used for the ReadCsv class.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Returns an object of the selected concrete read strategy class. This method is overwritten by the concrete strategy classes}
#'  \item{\code{readData(path)}}{Returns the content of the file with its location designated in the path parameter.}
#' }
#'
#' @docType class
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
Read0 <- R6::R6Class(
  classname = "Read0",
  public = list(
    readData = function(path) stop("Method not available for abstract classes.")
  )
)
