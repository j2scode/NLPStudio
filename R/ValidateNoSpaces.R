## ---- ValidateNoSpaces
#==============================================================================#
#                             ValidateNoSpaces                                 #
#==============================================================================#
#' ValidateNoSpaces
#'
#'
#' \code{ValidateNoSpaces} Class for validating string has no spaces
#'
#' This class provide a methods for validating character string has no spaces
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidateNoSpaces class}
#'  \item{\code{validate(object)}}{Method for validating string}
#' }
#'
#' @return a logical TRUE if valid string, FALSE otherwise
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidateNoSpaces <- R6::R6Class(
  "ValidateNoSpaces",
  inherit = Validate0,
  public = list(
    validate = function(class, method, fieldName, value, level, msg, expect = NULL) {

      if (exists('value')) {
        if (length(value) > 0) {
          if (grepl(pattern = "^\\S+\\s+", x = value, perl = TRUE)) {
            self$notify(class, method, fieldName, value, level, msg, expect)
            return(FALSE)
          } else {
            return(TRUE)
          }
        } else {
          msg <- "Field is blank"
          self$notify(class, method, fieldName, value, level, msg, expect)
          return(FALSE)
        }
      } else {
        msg <- "Field is blank"
        self$notify(class, method, fieldName, value, level, msg, expect)
        return(FALSE)
      }
    }
  )
)

