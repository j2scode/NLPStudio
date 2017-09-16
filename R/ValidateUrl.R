## ---- ValidateUrl
#==============================================================================#
#                               ValidateUrl                                    #
#==============================================================================#
#' ValidateUrl
#'
#'
#' \code{ValidateUrl} Class for validating website urls
#'
#' This class provide a methods for validating website urls
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidateUrl class}
#'  \item{\code{validate(object)}}{Method for validating urls}
#' }
#'
#' @return A logical TRUE if valid url, FALSE otherwise
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidateUrl <- R6::R6Class(
  "ValidateUrl",
  inherit = Validate0,
  public = list(
    validate = function(class, method, fieldName, value, level, msg, expect = NULL) {
      if (exists('value') & length(value) != 0) {
        if (!url.exists(value)) {
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

