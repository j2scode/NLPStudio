#==============================================================================#
#                               VValidatorSubmitQuery                          #
#==============================================================================#
#' VValidatorSubmitQuery
#'
#'
#' \code{VValidatorSubmitQuery} Visitor class responsible for validating the parameters for the buildQuery methods
#' of Curator class.
#'
#' \strong{VValidatorSubmitQuery Methods:}
#' The VValidatorSubmitQuery methods are as follows:
#'  \itemize{
#'   \item{\code{curator(object)}}{Method for validating the buildQuery method parameters of the VCurator object}
#' }
#'
#' @param object The curator object
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorSubmitQuery <- R6::R6Class(
  classname = "VValidatorSubmitQuery",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    validateDate = function(object, date) {
      v <- ValidateDate$new()
      if (v$validate(class = class(object)[1], method = "date",
                    fieldName = "date", value = date, level = "Error",
                    msg = paste0("Date variable is not in a parseable format.",
                                 " See ?", class(object)[1],
                                 " for further assistance."),
                    expect = NULL) == FALSE) {
        return(FALSE)
      }
      return(TRUE)
    },

    validate = function(object) {

      # Validate Dates
      from <- object$dateFrom()
      to <- object$dateTo()
      if (!is.null(from) & private$validateDate(object, from) == FALSE) {
        return(FALSE)
      }
      if (!is.null(to) & private$validateDate(object, to) == FALSE) {
        return(FALSE)
      }

      # Validate lastN
      v <- ValidateNumeric$new()
      lastN <- object$lastN()
      if (!is.null(lastN) & v$validate(class = class(object)[1], method = "lastN",
                     fieldName = "lastN", value = lastN, level = "Error",
                     msg = paste0("LastN variable must be numeric.",
                                  " See ?", class(object)[1],
                                  " for further assistance."),
                     expect = NULL) == FALSE) {
        return(FALSE)
      }

      # Validate Unit
      c <- Constants$new()
      vars <- c$getQueryVars()
      unit <- object$unit()
      if (!(is.null(unit)) & !(unit %in% vars$unit)) {
        v <- Validator0$new()
        v$notify(class = class(object)[1], method = "unit",
                     fieldName = "unit", value = unit, level = "Error",
                     msg = paste0("Unit variable invalid for query",
                                  " valid values are 'min', 'hour', ",
                                  " 'day', 'week', 'month'.",
                                  " See ?", class(object)[1],
                                  " for further assistance."),
                     expect = NULL)
        return(FALSE)
      }
      return(TRUE)
    }
  ),

  public = list(

    curator = function(object) {
      return(private$validate(object))
    }
  )
)
