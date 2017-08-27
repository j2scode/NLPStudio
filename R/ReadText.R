## ---- ReadText
#==============================================================================#
#                                   ReadText                                   #
#==============================================================================#
#' ReadText
#'
#'
#' \code{ReadText} Class contains the data and methods for reading text
#'
#' This class inherits from the Read0 inteface class and provides the data
#' and methods for reading text.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{readData(directory, fileName)}}{Reads text from the designated  directory and file name}
#' }
#'
#' @param path Character string with location of the file to be read.
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
ReadText <- R6::R6Class(
  classname = "ReadText",
  inherit = Read0,

  public = list(
    readData = function(path) {

      # Validate parameters
      if (missing(path)) {
        v <- Validate0$new()
        v$notify(cls = "ReadText", method = "readData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to read document. Path is missing without a default",
                             "See ?ReadText for assistance."),
                 expect = TRUE)
      }
      v <- ValidatePath$new()
      v$validate(cls = "ReadText", method = "readData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to read document. Path", path, "is invalid.",
                             "See ?ReadText for assistance."),
                 expect = TRUE)

      con <- file(path)
      on.exit(close(con))
      content <- readLines(con)

      return(content)
    }
  )
)






