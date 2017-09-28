## ---- ValidatorNumeric
#==============================================================================#
#                             ValidatorNumeric                                  #
#==============================================================================#
#' ValidatorNumeric
#'
#'
#' \code{ValidatorNumeric} Class for validating numerics
#'
#' This class provide a methods for validating numerics.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidatorNumeric class}
#'  \item{\code{validate(object)}}{Method for validating numeric}
#' }
#'
#' @return A logical TRUE if valid numeric, FALSE otherwise
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidatorNumeric <- R6::R6Class(
  "ValidatorNumeric",
  inherit = Validator,
  public = list(
    validate = function(class, method, fieldName, value, level, msg, expect = NULL) {

      if (exists('value') & length(value) != 0) {
        if (!is.numeric(value)) {
          self$notify(class, method, fieldName, value, level, msg, expect)
          return(FALSE)
        } else {
          return(TRUE)
        }
      } else {
        return(FALSE)
      }
    }
  )
)

