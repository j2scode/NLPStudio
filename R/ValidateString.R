## ---- ValidateString
#==============================================================================#
#                             ValidateString                                   #
#==============================================================================#
#' ValidateString
#'
#'
#' \code{ValidateString} Class for validating strings
#'
#' This class provide a methods for validating character strings
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidateString class}
#'  \item{\code{validate(object)}}{Method for validating string}
#' }
#'
#' @return a logical TRUE if valid string, FALSE otherwise
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
ValidateString <- R6::R6Class(
  "ValidateString",
  inherit = Validate0,
  public = list(
    validate = function(cls, method, fieldName, value, level, msg, expect = NULL) {

      if (exists('value') & length(value) != 0) {
        if (is.na(value) | is.null(value) | !is.character(value)
            | is.logical(value) | value == "" ) {
          self$notify(cls, method, fieldName, value, level, msg, expect)
          return(FALSE)
        } else {
          return(TRUE)
        }
      } else {
        self$notify(cls, method, fieldName, value, level, msg, expect)
        return(FALSE)
      }
    }
  )
)

