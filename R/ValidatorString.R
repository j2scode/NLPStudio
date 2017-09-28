## ---- ValidatorString
#==============================================================================#
#                             ValidatorString                                   #
#==============================================================================#
#' ValidatorString
#'
#'
#' \code{ValidatorString} Class for validating strings
#'
#' This class provide a methods for validating character strings
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidatorString class}
#'  \item{\code{validate(object)}}{Method for validating string}
#' }
#'
#' @return a logical TRUE if valid string, FALSE otherwise
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidatorString <- R6::R6Class(
  "ValidatorString",
  inherit = Validator,
  public = list(
    validate = function(class, method, fieldName, value, level, msg, expect = NULL) {

      if (exists('value') & length(value) != 0) {
        if (is.na(value) | is.null(value) | !is.character(value)
            | is.logical(value) | value == "" ) {
          self$notify(class, method, fieldName, value, level, msg, expect)
          return(FALSE)
        } else {
          return(TRUE)
        }
      } else {
        self$notify(class, method, fieldName, value, level, msg, expect)
        return(FALSE)
      }
    }
  )
)

