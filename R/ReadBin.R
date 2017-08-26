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
    validate = function(docData) {
      filePath <- file.path(docData$path, docData$fileName)
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

      document <- document$getDocument(format = "list")
      private$validate(document)

      filePath <- file.path(docData$path, docData$fileName)

      content <- readBin(
        file.path(docData$path, docData$fileName),raw(),
        file.info(file.path(docData$path, docData$fileName))$size
      )
      return(content)
    }
  )
)
