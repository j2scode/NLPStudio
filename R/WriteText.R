## ---- WriteTxt
#==============================================================================#
#                                   WriteTxt                                   #
#==============================================================================#
#' WriteTxt
#'
#'
#' \code{WriteTxt} Class contains the data and methods for writing files in character format.
#'
#' This class inherits from the Write0 abstract class and provides the data
#' and methods for writing files in character format.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{writeData(directory, fileName)}}{Writes the content in character format in the file path indicated in the document object}
#' }
#'
#' @param content Character vector of content to be written.
#' @param name Character string indicating the name of the object to be written.
#' @param path Character string indicating the location of the file to be written.
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
WriteText <- R6::R6Class(
  classname = "WriteText",
  inherit = Write0,

  public = list(
    writeData = function(name, path, content) {

      # Validate parameters
      if (missing(path)) {
        v <- Validate0$new()
        v$notify(cls = "WriteText", method = "writeData", fieldName = "path",
                 level = "Error", value = "",
                 msg = paste("Unable to write document. Path is missing without a default",
                             "See ?WriteText for assistance."),
                 expect = TRUE)
      }
      v <- ValidatePath$new()
      v$validate(cls = "WriteText", method = "writeData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to write document. Path", path, "is invalid.",
                             "See ?WriteText for assistance."),
                 expect = TRUE)

      v <- ValidateNotEmpty$new()
      v$validate(cls = "WriteText", method = "writeData",
                 fieldName = "content", value = content, level = "Error",
                 msg = paste("Unable to write content. Content must not be empty.",
                             "See ?WriteText for assistance."),
                 expect = TRUE)

      con <- file(path)
      on.exit(close(con))
      writeLines(content, con)
    }
  )
)
