## ---- ValidatorName
#==============================================================================#
#                                 ValidatorName                                 #
#==============================================================================#
#' ValidatorName
#'
#' @description
#' \code{ValidatorName} This class validates names of objects
#'
#' @section Public Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidatorName class}
#'  \item{\code{validate(class, method, name, expect)}}{Validates the name variable}
#' }
#'
#' @section Parameters:
#' @param class Character string containing the class invoking the validation.
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
ValidatorName <- R6::R6Class(
  "ValidatorName",
  inherit = Validator,
  public = list(

    validate = function(class, method, fieldName, value, level, msg, expect = NULL) {

      # Validate name is not missing
      if (missing(value)) {
        v <- Validator0$new()
        v$notify(class = class, method = method, fieldName = "name",
                 level = "Error", value = "",
                 msg = "Name is required", expect = NULL)
        return(FALSE)
      }

      # Validation: Name is not empty
      v <- ValidatorNotEmpty$new()
      if (v$validate(class = class, method = method, fieldName = "name",
                     level = "Error", value = value,
                     msg = "Name must not be empty", expect = TRUE) == FALSE) {
        return(FALSE)
      }

      # Validation: name is character class
      v <- ValidatorClass$new()
      if (v$validate(class = class, method = method, fieldName = "name",
                     level = "Error", value = class(value),
                     msg = "Name isn't a character class",
                     expect = "character") == FALSE) {
        return(FALSE)
      }

      # Validation: Name's existence is what is expected
      v <- ValidatorExists$new()
      if (v$validate(class = class, method = method, fieldName = "name",
                     level = "Error", value = value,
                     msg = "Name already exists",
                     expect = expect) == FALSE) {
        return(FALSE)
      }

      # Validation: No spaces
      v <- ValidatorNoSpaces$new()
      if (v$validate(class = class, method = method, fieldName = "name",
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
