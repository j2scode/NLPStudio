## ---- WriteCsv
#==============================================================================#
#                                   WriteCsv                                   #
#==============================================================================#
#' WriteCsv
#'
#'
#' \code{WriteCsv} Class contains the data and methods for writing files in csv format.
#'
#' This class inherits from the Write0 abstract class and provides the data
#' and methods for writing files in csv format.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{writeData(directory, fileName)}}{Writes the content in csv format in the file path indicated in the document object}
#' }
#'
#' @param content An object of type csv.
#' @param name Character string indicating the name of the object to be written.
#' @param path Character string indicating the location of the file to be written.
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
WriteCsv <- R6::R6Class(
  classname = "WriteCsv",
  inherit = Write0,

  public = list(
    writeData = function(name, path, content) {

      # Validate parameters
      if (missing(path)) {
        v <- Validate0$new()
        v$notify(cls = "WriteCsv", method = "writeData", fieldName = "path",
                 level = "Error", value = "",
                 msg = paste("Unable to write document. Path is missing without a default",
                             "See ?WriteCsv for assistance."),
                 expect = TRUE)
        stop()
      }

      v <- ValidateClass$new()
      if (v$validate(cls = "WriteCsv", method = "writeData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to write document. Path is not a character string.",
                             "See ?WriteCsv for assistance."),
                 expect = "character") == FALSE) {
        stop()
      }

      v <- ValidatePath$new()
      if (v$validate(cls = "WriteCsv", method = "writeData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to write document. Path", path, "is invalid.",
                             "See ?WriteCsv for assistance."),
                 expect = TRUE) == FALSE) {
        stop()
      }

      if (missing(content)) {
        v <- Validate0$new()
        v$notify(cls = "WriteCsv", method = "writeData",
                   fieldName = "content", value = "", level = "Error",
                   msg = paste("Unable to write content. Content is missing with no default",
                               "See ?WriteCsv for assistance."),
                   expect = TRUE)
        stop()
      }

      v <- ValidateNotEmpty$new()
      if (v$validate(cls = "WriteCsv", method = "writeData",
                 fieldName = "content", value = content, level = "Error",
                 msg = paste("Unable to write content. Content must not be empty.",
                             "See ?WriteCsv for assistance."),
                 expect = TRUE) == FALSE) {
        stop()
      }

      write.csv(content, file = path, row.names = FALSE)
    }
  )
)
