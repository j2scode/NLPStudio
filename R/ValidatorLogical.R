## ---- ValidatorLogical
#==============================================================================#
#                             ValidatorLogical                                  #
#==============================================================================#
#' ValidatorLogical
#'
#'
#' \code{ValidatorLogical} Class for validating logicals
#'
#' This class provide a methods for validating logicals.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidatorLogical class}
#'  \item{\code{validate(object)}}{Method for validating logical}
#' }
#'
#' @return A logical TRUE if valid logical, FALSE otherwise
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidatorLogical <- R6::R6Class(
  "ValidatorLogical",
  inherit = Validator0,
  public = list(
    validate = function(class, method, fieldName, value, level, msg, expect = NULL) {
        if (is.logical(value) | value == "TRUE" | value == "FALSE") {
          return(TRUE)
        } else {
          self$notify(class, method, fieldName, value, level, msg, expect)
          return(FALSE)
        }
    }
  )
)

