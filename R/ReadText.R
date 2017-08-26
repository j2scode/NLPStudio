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
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
ReadText <- R6::R6Class(
  classname = "ReadText",
  inherit = Read0,

  private = list(
    validate = function(docData) {
      filePath <- file.path(docData$path, docData$fileName)
      v <- ValidatePath$new()
      v$validate(cls = "ReadText", method = "readData",
                 fieldName = "file.path(document$path, document$fileName)",
                 value = filePath, level = "Error",
                 msg = "Invalid file path.", expect = TRUE)
      rm(v)
    }
  ),
  public = list(
    readData = function(document) {

      document <- document$getDocument(format = "list")
      private$validate(document)

      con <- file(file.path(docData$path, docData$fileName))
      on.exit(close(con))
      content <- readLines(con)

      return(content)
    }
  )
)






