## ---- WriteRdata
#==============================================================================#
#                                   WriteRdata                                 #
#==============================================================================#
#' WriteRdata
#'
#'
#' \code{WriteRdata} Class contains the data and methods for writing files in RData format.
#'
#' This class inherits from the Write0 abstract class and provides the data
#' and methods for writing files in RData format.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{writeData(directory, fileName)}}{Writes the content in RData format in the file path indicated in the document object}
#' }
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
WriteRdata <- R6::R6Class(
  classname = "WriteRdata",
  inherit = Write0,
  private = list(
    validate = function(document, content) {

      v <- ValidatePathy$new()
      v$validate(cls = "WriteRdata", method = "writeData",
                 fieldName = "path", value = document$path, level = "Error",
                 msg = "Invalid path.", expect = TRUE)
      v$validate(cls = "WriteRdata", method = "writeData",
                 fieldName = "fileName", value = document$fileName, level = "Error",
                 msg = "Invalid file name.", expect = TRUE)
      v <- ValidateNotEmpty$new()
      v$validate(cls = "WriteRdata", method = "writeData",
                 fieldName = "content", value = content, level = "Error",
                 msg = "Content must not be empty.", expect = TRUE)
      rm(v)
    }
  ),
  public = list(
    writeData = function(document, content) {

      document <- document$getDocument(format = "list")
      private$validate(document, content)

      assign(document$objName, content)
      save(list=document$objName,
           file = file.path(document$path, document$fileName))
    }
  )
)
