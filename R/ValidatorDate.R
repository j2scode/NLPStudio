## ---- ValidatorDate
#==============================================================================#
#                               ValidatorDate                                  #
#==============================================================================#
#' ValidatorDate
#'
#'
#' \code{ValidatorDate} Class for validating and parsing dates
#'
#' This class provide a methods for validating and parsing dates. This class
#' uses the 'parsedate' package to return a POSIXct object for any
#' recognizable date format.
#'
#' @docType class
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of ValidatorDate class}
#'  \item{\code{validate(date)}}{Method for validating and parsing dates}
#' }
#'
#' @return date A POSIXct date object if the date was recognized or FALSE otherwise.
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Validation Classes
#' @export
ValidatorDate <- R6::R6Class(
  "ValidatorDate",
  inherit = Validator0,
  public = list(
    validate = function(class, method, fieldName, value, level, msg, expect = NULL) {

      date <- parsedate::parse_date(value, approx = TRUE)

      if (is.na(date)) {
        self$notify(class, method, fieldName, value, level, msg, expect)
        return(FALSE)
      } else {
        return(date)
      }
    }
  )
)

