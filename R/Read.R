## ---- Read0
#==============================================================================#
#                                   Read0                                      #
#==============================================================================#
#' Read0
#'
#'
#' \code{Read0} Class as in interface class to the ReadText, ReadCsv, ReadRdata
#'              subclasses
#'
#' This class is an interface to the Read* sub classes.  It defines the core
#' variables and method(s) inherited by the Read* subclasses.
#'
#' @docType class
#'
#' @section Fields:
#' @field directory A character string containing the file directory
#' @field fileName A character string containing the name of the file to read
#'
#' @section Methods:
#' \describe{
#'  \item{\code{readData(directory, fileName)}}{Overwritten by subclasses}
#' }
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
Read0 <- R6::R6Class(
  classname = "Read0",
  public = list(
    readData = function() stop("I'm an abstract interface method")
  )
)
