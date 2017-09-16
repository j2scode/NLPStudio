## ---- ValidatePath
#==============================================================================#
#                             ValidatePath                                     #
#==============================================================================#
#' ValidatePath
#'
#' @description
#' \code{ValidatePath} Class for validating a file path
#'
#' This class provide a methods for validating file paths.
#'
#' @docType class
#'
#' @param class Character string containing the name of the invoking class
#' @param method Character string containing the name of the invoking method
#' @param fieldName Character string containing the name of the field to validate
#' @param value The value of the field being validated
#' @param level Character string c("Error", "Warn", "Info)
#' @param msg Character vector containing the message to be displayed if test fails
#' @param expect Logical c(TRUE, FALSE) indicating expected outcome
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidatePath <- R6::R6Class(
  "ValidatePath",
  inherit = Validate0,
  public = list(

    validate = function(class, method, fieldName, value, level, msg, expect = NULL) {

      if (length(value) == 0) {
        msg <- "Invalid file path. File path variable is empty."
        self$notify(class, method, fieldName, value, level, msg, expect)
        return(FALSE)
      } else if (value == "") {
        msg <- "Invalid file path. File path variable is empty."
        self$notify(class, method, fieldName, value, level, msg, expect)
        return(FALSE)
      } else if (isDirectory(value) & !dir.exists(value) & expect == TRUE) {
        msg <- paste("Invalid directory,", value, "directory does not exist.")
        self$notify(class, method, fieldName, value, level, msg, expect)
        return(FALSE)
      } else if (isDirectory(value) & dir.exists(value) & expect == FALSE) {
        msg <- msg <- paste("Invalid directory,", value, "directory already exists.")
        self$notify(class, method, fieldName, value, level, msg, expect)
        return(FALSE)
      } else if (!file.exists(value) & expect == TRUE) {
        msg <- paste("Invalid file,", value, "does not exist.")
        self$notify(class, method, fieldName, value, level, msg, expect)
        return(FALSE)
      } else if (file.exists(value) & expect == FALSE) {
        msg <- paste("Invalid file,", value, "already exists.")
        self$notify(class, method, fieldName, value, level, msg, expect)
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
  )
)
