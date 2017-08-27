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
#' @export
ValidateName <- R6::R6Class(
  "ValidateName",
  inherit = Validation0,
  public = list(

    validate = function(cls, method, name, expect = FALSE) {

      # Validate name is not missing
      if (missing(name)) {
        v <- Validate0$new()
        v$notify(cls = cls, method = method, fieldName = "name",
                 level = "Error", value = NULL,
                 msg = "Name is required", expect = NULL)
      }

      # Validation: Name is not empty
      v <- ValidateNotEmpty$new()
      v$validate(cls = cls, method = method, fieldName = "name",
                 level = "Error", value = name,
                 msg = "Name must not be empty", expect = TRUE)

      # Validation: name is character class
      v <- ValidateClass$new()
      v$validate(cls = cls, method = method, fieldName = "name",
                 level = "Error", value = class(name),
                 msg = "Name isn't a character class", expect = "character")

      # Validation: Name's existence is what is expected
      v <- ValidateExists$new()
      v$validate(cls = cls, method = method, fieldName = "name",
                 level = "Error", value = name,
                 msg = "Name already exists", expect = expect)

      # Validation: No spaces
      v <- ValidateNoSpaces$new()
      v$validate(cls = cls, method = method, fieldName = "name",
                 level = "Error", value = name,
                 msg = "Name must have no spaces", expect = TRUE)

      rm(v)
    }
  ),
  lock_class = FALSE,
  lock_objects = FALSE
)
