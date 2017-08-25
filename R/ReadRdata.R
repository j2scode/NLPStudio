## ---- ReadRdata
#==============================================================================#
#                                   ReadRdata                                  #
#==============================================================================#
#' ReadRdata
#'
#'
#' \code{ReadRdata} Class contains the data and methods for reading Rdata files
#'
#' This class inherits from the Read0 inteface class and provides the data
#' and methods for reading Rdata data from files
#'
#' @docType class
#'
#' @section Fields:
#' @field objectName A character string containing the name to assign to the Rdata object
#'
#' @section Methods:
#' \describe{
#'  \item{\code{readData(directory, fileName)}}{Reads Rdata file from the designated  directory and file name and assigns the object name in the global environment }
#' }
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @family Document i/o classes
#' @export
ReadRdata <- R6::R6Class(
  classname = "ReadRdata",
  inherit = Read0,

  private = list(
    validate = function(document) {
      filePath <- file.path(document$path, document$fileName)
      v <- ValidatePath$new()
      v$validate(cls = "ReadRdata", method = "readData",
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

      content$content <- load(file.path(directory, fileName), envir = .GlobalEnv)
      return(content)
    }
  )
)

