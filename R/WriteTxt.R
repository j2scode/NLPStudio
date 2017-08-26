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
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
WriteTxt <- R6::R6Class(
  classname = "WriteTxt",
  inherit = Write0,
  private = list(
    validate = function(document, content) {

      v <- ValidateNotEmpty$new()
      v$validate(cls = "WriteTxt", method = "writeData",
                 fieldName = "path", value = document$path, level = "Error",
                 msg = "File path must not be empty.", expect = TRUE)
      v$validate(cls = "WriteTxt", method = "writeData",
                 fieldName = "fileName", value = document$fileName, level = "Error",
                 msg = "File name must not be empty.", expect = TRUE)
      v$validate(cls = "WriteTxt", method = "writeData",
                 fieldName = "content", value = content, level = "Error",
                 msg = "Content must not be empty.", expect = TRUE)
      rm(v)
    }
  ),
  public = list(
    writeData = function(document, content) {

      private$validate(document, content)

      con <- file(file.path(document$path, document$fileName))
      on.exit(close(con))
      writeLines(content, con)
    }
  )
)
