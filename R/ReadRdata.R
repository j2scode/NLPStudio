## ---- ReadRdata
#==============================================================================#
#                                   ReadRdata                                  #
#==============================================================================#
#' ReadRdata
#'
#'
#' \code{ReadRdata} Class contains the data and methods for reading Rdata files
#'
#' This class inherits from the Read0 inteface class and provides the data
#' and methods for reading Rdata data from files
#'
#' @docType class
#'
#' @section Fields:
#' @field objectName A character string containing the name to assign to the Rdata object
#'
#' @section Methods:
#' \describe{
#'  \item{\code{readData(directory, fileName)}}{Reads Rdata file from the designated  directory and file name and assigns the object name in the global environment }
#' }
#'
#'@param path Character string containing the path to the document to be read.
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
ReadRdata <- R6::R6Class(
  classname = "ReadRdata",
  inherit = Read0,

  public = list(
    readData = function(path) {

      # Validate parameters
      if (missing(path)) {
        v <- Validate0$new()
        v$notify(cls = "ReadRdata", method = "readData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to read document. Path is missing without a default",
                             "See ?ReadRdata for assistance."),
                 expect = TRUE)
        stop()
      }

      v <- ValidateClass$new()
      if (v$validate(cls = "ReadRdata", method = "readData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to read document. Path is not a character string.",
                             "See ?ReadRdata for assistance."),
                 expect = "character") == FALSE) {
        stop()
      }

      v <- ValidatePath$new()
      if (v$validate(cls = "ReadRdata", method = "readData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to read document. Path", path, "is invalid.",
                             "See ?ReadRdata for assistance."),
                 expect = TRUE) == FALSE) {
        stop()
      }

      env <- new.env()
      object <- load(path, envir = env)
      return(env[[object]])
    }
  )
)

