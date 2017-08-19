## ---- ValidateLogical
#==============================================================================#
#                             ValidateLogical                                  #
#==============================================================================#
#' ValidateLogical
#'
#'
#' \code{ValidateLogical} Class for validating logicals
#'
#' This class provide a methods for validating logicals.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidateLogical class}
#'  \item{\code{validate(object)}}{Method for validating logical}
#' }
#'
#' @return A logical TRUE if valid logical, FALSE otherwise
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
ValidateLogical <- R6::R6Class(
  "ValidateLogical",
  inherit = Validate0,
  public = list(
    validate = function(cls, method, fieldName, value, level, msg, expect = NULL) {
        if (is.logical(value) | value == "TRUE" | value == "FALSE") {
          return(TRUE)
        } else {
          self$notify(cls, method, fieldName, value, level, msg, expect)
          return(FALSE)
        }
    }
  )
)

