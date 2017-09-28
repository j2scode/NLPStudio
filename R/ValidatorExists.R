## ---- ValidatorExists
#==============================================================================#
#                             ValidatorExists                                   #
#==============================================================================#
#' ValidatorExists
#'
#'
#' \code{ValidatorExists} Class for validating whether an object exists
#'
#' This class provide a methods for validating the existence of objects in
#' the global environment
#'
#' @docType class
#'
#'
#' @return A logical  TRUE if the object exists, FALSE otherwise
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidatorExists <- R6::R6Class(
  "ValidatorExists",
  inherit = Validator,
  public = list(
    validate = function(class, method, fieldName, value, level, msg, expect = NULL) {
      if (length(value) != 0 & !is.null(value)) {
        if (exists(value) & (expect == TRUE | expect == "TRUE")) {
          return(TRUE)
        } else if (!exists(value) & (expect == FALSE | expect == "FALSE")){
          return(TRUE)
        } else {
          self$notify(class, method, fieldName, value, level, msg, expect)
          return(FALSE)
        }
      }
    }
  )
)
