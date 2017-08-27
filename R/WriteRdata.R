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
#' @param content An object of type RData to be written.
#' @param name A character string indicating the name of the object to be written.
#' @param path Character string indicating the location of the file to be written.
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
WriteRdata <- R6::R6Class(
  classname = "WriteRdata",
  inherit = Write0,

  public = list(
    writeData = function(name, path, content) {

      # Validate parameters
      if (missing(name)) {
        v <- Validate0$new()
        v$notify(cls = "WriteRdata", method = "writeData", fieldName = "name",
                 level = "Error", value = "",
                 msg = paste("Unable to write document. Name is missing without a default",
                             "See ?WriteRdata for assistance."),
                 expect = TRUE)
      }

      if (missing(path)) {
        v <- Validate0$new()
        v$notify(cls = "WriteRdata", method = "writeData", fieldName = "path",
                 level = "Error", value = "",
                 msg = paste("Unable to write document. Path is missing without a default",
                             "See ?WriteRdata for assistance."),
                 expect = TRUE)
      }
      v <- ValidateName$new()
      v$validate(cls = "WriteRdata", method = "writeData", fieldName = "name",
                 level = "Error", value = name,
                 msg = paste("Unable to write document. Name", name, "is invalid.",
                             "See ?WriteRdata for assistance."),
                 expect = TRUE)

      v <- ValidatePath$new()
      v$validate(cls = "WriteRdata", method = "writeData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to write document. Path", path, "is invalid.",
                             "See ?WriteRdata for assistance."),
                 expect = TRUE)

      v <- ValidateNotEmpty$new()
      v$validate(cls = "WriteRdata", method = "writeData",
                 fieldName = "content", value = content, level = "Error",
                 msg = paste("Unable to write content. Content must not be empty.",
                             "See ?WriteRdata for assistance."),
                 expect = TRUE)

      assign(name, content)
      save(list=name, file = path)
    }
  )
)
