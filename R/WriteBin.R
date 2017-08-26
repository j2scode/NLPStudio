## ---- WriteBin
#==============================================================================#
#                                   WriteBin                                   #
#==============================================================================#
#' WriteBin
#'
#'
#' \code{WriteBin} Class contains the data and methods for writing files in binary format.
#'
#' This class inherits from the Write0 abstract class and provides the data
#' and methods for writing files in binary format.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{writeData(directory, fileName)}}{Writes the content in binary format in the file path indicated in the document object}
#' }
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
WriteBin <- R6::R6Class(
  classname = "WriteBin",
  inherit = Write0,
  private = list(
    validate = function(document, content) {

      v <- ValidatePathy$new()
      v$validate(cls = "WriteBin", method = "writeData",
                 fieldName = "path", value = document$path, level = "Error",
                 msg = "Invalid path.", expect = TRUE)
      v$validate(cls = "WriteBin", method = "writeData",
                 fieldName = "fileName", value = document$fileName, level = "Error",
                 msg = "Invalid file name.", expect = TRUE)
      v <- ValidateNotEmpty$new()
      v$validate(cls = "WriteBin", method = "writeData",
                 fieldName = "content", value = content, level = "Error",
                 msg = "Content must not be empty.", expect = TRUE)
      rm(v)
    }
  ),
  public = list(
    writeData = function(document, content) {

      document <- document$getDocument(format = "list")
      private$validate(document, content)

      writeBin(content, file.path(document$path, document$fileName))
    }
  )
)
