## ---- ValidateName
#==============================================================================#
#                                 ValidateName                                 #
#==============================================================================#
#' ValidateName
#'
#' @description
#' \code{ValidateName} This class validates names of objects
#'
#' @section Public Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidateName class}
#'  \item{\code{validate(cls, method, name, expect)}}{Validates the name variable}
#' }
#'
#' @section Parameters:
#' @param cls Character string containing the class invoking the validation.
#' @param method Character string containing the name of the invoking method
#' @param name Character string containing the name variable
#' @param expect Logical c(TRUE, FALSE) indicating expected outcome
#'
#' @return Throws an error if the field doesn't pass validation
#' @docType class
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidateName <- R6::R6Class(
  "ValidateName",
  inherit = Validate0,
  public = list(

    validate = function(cls, method, fieldName, value, level, msg, expect = NULL) {

      # Validate name is not missing
      if (missing(value)) {
        v <- Validate0$new()
        v$notify(cls = cls, method = method, fieldName = "name",
                 level = "Error", value = "",
                 msg = "Name is required", expect = NULL)
        return(FALSE)
      }

      # Validation: Name is not empty
      v <- ValidateNotEmpty$new()
      if (v$validate(cls = cls, method = method, fieldName = "name",
                     level = "Error", value = value,
                     msg = "Name must not be empty", expect = TRUE) == FALSE) {
        return(FALSE)
      }

      # Validation: name is character class
      v <- ValidateClass$new()
      if (v$validate(cls = cls, method = method, fieldName = "name",
                     level = "Error", value = class(value),
                     msg = "Name isn't a character class",
                     expect = "character") == FALSE) {
        return(FALSE)
      }

      # Validation: Name's existence is what is expected
      v <- ValidateExists$new()
      if (v$validate(cls = cls, method = method, fieldName = "name",
                     level = "Error", value = value,
                     msg = "Name already exists",
                     expect = expect) == FALSE) {
        return(FALSE)
      }

      # Validation: No spaces
      v <- ValidateNoSpaces$new()
      if (v$validate(cls = cls, method = method, fieldName = "name",
                     level = "Error", value = value,
                     msg = "Name must have no spaces",
                     expect = TRUE) == FALSE) {
        return(FALSE)
      }
      return(TRUE)
    }
  ),
  lock_class = FALSE,
  lock_objects = FALSE
)
