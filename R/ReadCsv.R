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
#' @param path A character string indicating the location for the file to be read.
#' @param header A logical indicating whether the csv file contains headers. Defaults to TRUE
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
ReadCsv <- R6::R6Class(
  classname = "ReadCsv",
  inherit = Read0,

  public = list(
    readData = function(path, header = TRUE) {

      # Validate parameters
      if (missing(path)) {
        v <- Validate0$new()
        v$notify(cls = "ReadCsv", method = "readData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to read document. Path is missing without a default",
                             "See ?ReadCsv for assistance."),
                 expect = TRUE)
      }
      v <- ValidatePath$new()
      v$validate(cls = "ReadCsv", method = "readData", fieldName = "path",
                 level = "Error", value = path,
                 msg = paste("Unable to read document. Path", path, "is invalid.",
                             "See ?ReadCsv for assistance."),
                 expect = TRUE)

      con <- file(path)
      on.exit(close(con))
      content <- read.csv(con, header = header, stringsAsFactors = FALSE,
                                  sep = ",", quote = "\"'")
      return(content)
    }
  )
)
