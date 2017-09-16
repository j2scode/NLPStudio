## ---- ReadBin
#==============================================================================#
#                                   ReadBin                                    #
#==============================================================================#
#' ReadBin
#'
#'
#' \code{ReadBin} Class contains the data and methods for reading files in binary format.
#'
#' This class inherits from the Read0 inteface class and provides the data
#' and methods for reading files in binary format.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{readData(path)}}{Reads data from the location designated in the path parameter}
#' }
#'
#' @param path Character string containing the path to the document to be read.
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
ReadBin <- R6::R6Class(
  classname = "ReadBin",
  inherit = Read0,
  public = list(
    readData = function(path) {

      # Validate parameters
      if (missing(path)) {
        v <- Validate0$new()
        v$notify(class = "ReadBin", method = "readData", fieldName = "path",
        level = "Error", value = path,
        msg = paste("Unable to read document. Path is missing without a default",
                    "See ?ReadBin for assistance."),
        expect = TRUE)
        stop()
      }

      v <- ValidateClass$new()
      if (v$validate(class = "ReadBin", method = "readData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to read document. Path is not a character string.",
                             "See ?ReadBin for assistance."),
                 expect = "character") == FALSE) {
        stop()
      }

      v <- ValidatePath$new()
      if (v$validate(class = "ReadBin", method = "readData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to read document. Path", path, "is invalid.",
                             "See ?ReadBin for assistance."),
                 expect = TRUE) == FALSE) {
        stop()
      }

      content <- readBin(path, raw(), file.info(path)$size)
      return(content)
    }
  )
)
