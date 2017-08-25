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
    validate = function(document) {
      filePath <- file.path(document$path, document$fileName)
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

      private$validate(document)

      content = list(
        metaData = list(
          parent = document$parent,
          name = document$name,
          desc = document$desc,
          path = document$path,
          fileName = document$fileName,
          objName = document$objName
        ),
        content = ""
      )

      con <- file(file.path(document$path, document$fileName))
      on.exit(close(con))
      content$content <- readLines(con)

      return(content)
    }
  )
)






