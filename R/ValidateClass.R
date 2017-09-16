## ---- ValidateClass
#==============================================================================#
#                               ValidateClass                                  #
#==============================================================================#
#' ValidateClass
#'
#'
#' \code{ValidateClass} Class for validating object class
#'
#' This class provide a methods for validating an object's class.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidateClass class}
#'  \item{\code{validate(object)}}{Method for validating logical}
#' }
#'
#' @return A logical TRUE if class is equal to expected value, FALSE otherwise.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidateClass <- R6::R6Class(
  "ValidateClass",
  inherit = Validate0,
  public = list(
    validate = function(class, method, fieldName, value, level, msg, expect = NULL) {
      if (exists('value') & length(value) != 0) {
        classes <- class(value)
        if (length(intersect(expect, classes)) > 0) {
          return(TRUE)
        } else {
          self$notify(class, method, fieldName, value, level, msg, expect)
          return(FALSE)
        }
      }
    }
  )
)

