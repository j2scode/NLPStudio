## ---- ValidateUrl
#==============================================================================#
#                               ValidateUrl                                    #
#==============================================================================#
#' ValidateUrl
#'
#'
#' \code{ValidateUrl} Class for validating website urls
#'
#' This class provide a methods for validating logicals.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidateUrl class}
#'  \item{\code{validate(object)}}{Method for validating logical}
#' }
#'
#' @return A logical TRUE if valid logical, FALSE otherwise
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
ValidateUrl <- R6::R6Class(
  "ValidateUrl",
  inherit = Validate0,
  public = list(
    validate = function(cls, method, fieldName, value, level, msg, expect = NULL) {
      if (exists('value') & length(value) != 0) {
        if (!url.exists(value)) {
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

