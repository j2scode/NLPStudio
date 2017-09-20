## ---- VWriter
#==============================================================================#
#                                   VWriter                                    #
#==============================================================================#
#' VWriter
#'
#'
#' \code{VWriter} Visitor class responsible for writing content to files of various formats.
#' Current formats include .csv, .Rdata, and .txt files
#'
#' \strong{VWriter Class Overview:}
#' The VWriter class is an implementation of the visitor design pattern,
#' as described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This visitor pattern allows
#' new operations to be defined without changing the classes upon which
#' the visitor method operates.
#'
#' \strong{VWriter Methods:}
#' The VWriter class supports csv, Rdata, and text files through the following methods:
#'  \itemize{
#'   \item{\code{writeCsv(file)}}{Method for writing csv files.}
#'   \item{\code{writeRdata(file)}}{Method for writing Rdata files.}
#'   \item{\code{writeText(file)}}{Method for writing text files.}
#' }
#'
#' @param file Object of the File family of classes
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Read/Write Classes
#' @export
VWriter <- R6::R6Class(
  classname = "VWriter",
  private = list(

    validateFile = function(file, method, class) {

      f <- file$getObject()

      if (lengtH(f$path) == 0) {
        v <- Validate0$new()
        v$notify(class = "VWriter", method = method, fieldName = "path",
                 level = "Error", value = "",
                 msg = paste("Unable to write document.",
                             "Path is missing with no default.",
                             "See ?VWriter for assistance."),
                 expect = TRUE)
        stop()
      }

      if (lengtH(f$fileName) == 0) {
        v <- Validate0$new()
        v$notify(class = "VWriter", method = method, fieldName = "fileName",
                 level = "Error", value = "",
                 msg = paste("Unable to write document.",
                             "File name is missing with no default.",
                             "See ?VWriter for assistance."),
                 expect = TRUE)
        stop()
      }

      if (length(f$content) == 0) {
        v <- Validate0$new()
        v$notify(class = "VWriter", method = method, fieldName = "content",
                 level = "Error", value = "",
                 msg = paste("Unable to write document.",
                             "Content is missing with no default.",
                             "See ?VWriter for assistance."),
                 expect = TRUE)
        stop()
      }

      v <- ValidateClass$new()
      if (v$validate(class = "VWriter", method = method, fieldName = "class(file)",
                     level = "Error", value = class(file)[1],
                     msg = paste("Unable to write document. Object is not a",
                                 class, "class object.",
                                 "See ?VWriter for assistance."),
                     expect = class) == FALSE) {
        stop()
      }

    }
  ),
  public = list(

    writeCsv = function(file) {

      private$validateFile(file, method = "writeCsv", class = "FileCsv")
      f <- file$getObject()
      write.csv(f$content, file = file.path(f$path, f$fileName), row.names = FALSE)
      return(TRUE)

    },

    writeRdata = function(file) {

      private$validateFile(file, method = "writeRdata", class = "FileRdata")
      f <- file$getObject()
      saveRDS(object = f$content, file = file.path(f$path, f$fileName))
      return(TRUE)

    },

    writeText = function(file) {

      private$validateFile(file, method = "writeText", class = "FileText")
      f <- file$getObject()
      con <- file(file.path(f$path, f$fileName))
      on.exit(close(con))
      writeLines(f$content, con)
      return(TRUE)

    }
  )
)
