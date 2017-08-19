## ---- ValidateExists
#==============================================================================#
#                             ValidateExists                                   #
#==============================================================================#
#' ValidateExists
#'
#'
#' \code{ValidateExists} Class for validating whether an object exists
#'
#' This class provide a methods for validating the existence of objects in
#' the global environment
#'
#' @docType class
#'
#'
#' @return A logical  TRUE if the object exists, FALSE otherwise
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
ValidateExists <- R6::R6Class(
  "ValidateExists",
  inherit = Validate0,
  public = list(
    validate = function(cls, method, fieldName, value, level, msg, expect = NULL) {
      if (length(value) != 0 & !is.null(value)) {
        if (exists(value) & (expect == TRUE | expect == "TRUE")) {
          return(TRUE)
        } else if (!exists(value) & (expect == FALSE | expect == "FALSE")){
          return(TRUE)
        } else {
          self$notify(cls, method, fieldName, value, level, msg, expect)
          return(FALSE)
        }
      }
    }
  )
)
