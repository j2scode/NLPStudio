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
  public = list(
    readData = function(document) {

      docData <- document$getDocument()
      filePath <- file.path(docData$path, docData$fileName)

      content <- readBin(
        file.path(docData$path, docData$fileName),raw(),
        file.info(file.path(docData$path, docData$fileName))$size
      )
      return(content)
    }
  )
)
