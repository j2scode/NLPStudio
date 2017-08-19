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
#'  \item{\code{readData(directory, fileName)}}{Reads data from the designated  directory and file name}
#' }
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
ReadBin <- R6::R6Class(
  classname = "ReadBin",
  inherit = Read0,
  private = list(
    validate = function(document) {
      filePath <- file.path(document$path, document$fileName)
      v <- ValidatePath$new()
      v$validate(cls = "ReadBin", method = "readData",
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

      content$content <- readBin(
        file.path(document$path, document$fileName),raw(),
        file.info(file.path(document$path, document$fileName))$size
      )
      return(content)
    }
  )
)
