## ---- ValidateNumeric
#==============================================================================#
#                             ValidateNumeric                                  #
#==============================================================================#
#' ValidateNumeric
#'
#'
#' \code{ValidateNumeric} Class for validating numerics
#'
#' This class provide a methods for validating numerics.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidateNumeric class}
#'  \item{\code{validate(object)}}{Method for validating numeric}
#' }
#'
#' @return A logical TRUE if valid numeric, FALSE otherwise
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidateNumeric <- R6::R6Class(
  "ValidateNumeric",
  inherit = Validate0,
  public = list(
    validate = function(cls, method, fieldName, value, level, msg, expect = NULL) {

      if (exists('value') & length(value) != 0) {
        if (!is.numeric(value)) {
          self$notify(cls, method, fieldName, value, level, msg, expect)
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

