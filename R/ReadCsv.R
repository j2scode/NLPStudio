## ---- ReadCsv
#==============================================================================#
#                                   ReadCsv                                    #
#==============================================================================#
#' ReadCsv
#'
#'
#' \code{ReadCsv} Class contains the data and methods for reading csv files
#'
#' This class inherits from the Read0 inteface class and provides the data
#' and methods for reading csv files
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{readData(directory, fileName)}}{Reads data from the designated  directory and file name}
#' }
#'
#' @param document An objectd of the Document class
#' @param header A logical indicating whether the csv file contains headers
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
ReadCsv <- R6::R6Class(
  classname = "ReadCsv",
  inherit = Read0,

  private = list(
    validate = function(document) {
      filePath <- file.path(document$path, document$fileName)
      v <- ValidatePath$new()
      v$validate(cls = "ReadCsv", method = "readData",
                 fieldName = "file.path(document$path, document$fileName)",
                 value = filePath, level = "Error",
                 msg = "Invalid file path.", expect = TRUE)
      rm(v)
    }
  ),
  public = list(
    readData = function(document, header = TRUE) {

      private$validate(document)

      con <- file(file.path(document$path, document$fileName))
      on.exit(close(con))
      content <- read.csv(con, header = header, stringsAsFactors = FALSE,
                                  sep = ",", quote = "\"'")
      return(content)
    }
  )
)
