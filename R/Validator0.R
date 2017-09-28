#==============================================================================#
#                                 Validator0                                   #
#==============================================================================#
#' Validator0
#'
#' \code{Validator0} Abstract class for the field validation classes. Defines
#' the interface for field validation, and provides methods for logging
#' and reporting validation errors.
#'
#' \strong{Validator0 Methods:}
#' \describe{
#'  \item{\code{validate(class, method, fieldName, value, level, msg, expect)}}{Field level validation method overwritten by subclasses.}
#'  \item{\code{notify(class, method, fieldName, value, level, msg, expect)}}{Renders notification of field level validation.}
#' }
#'
#' \strong{Validator0 Parameters:}
#' @param class Character string containing the name of the invoking class
#' @param method Character string containing the name of the invoking method
#' @param fieldName Character string containing the name of the field to validate
#' @param value The value of the field being validated
#' @param level Character string c("Error", "Warn", "Info)
#' @param msg Character vector containing the message to be displayed if test fails
#' @param expect Logical c(TRUE, FALSE) indicating expected outcome
#'
#' @return A logical, TRUE if validation criteria are met, FALSE, otherwise.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Validation Classes
#' @export
Validator0 <- R6::R6Class(
  "Validator0",
  private = list(
    notifyInfo  = function(note) futile.logger::flog.info(note, name = "white"),
    notifyWarn  = function(note) futile.logger::flog.warn(note, name = "yellow"),
    notifyError = function(note) {
      futile.logger::flog.error(note, name = "red")
    }
  ),

  public = list(

    validate = function(class, method, fieldName, value, level, msg, expect = NULL)
      stop("This method is not available"),

    notify = function(class, method, fieldName, value, level, msg, expect = NULL) {

      if (exists("class") & exists('method') & exists("value") &
          exists("level") & exists("msg")) {

        level <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
                      level, perl = TRUE)

        note <- paste0(level, " in class '", class, "', method '", method,
                       "' with variable '", fieldName, "'. ", msg)
        switch(level,
               Info  = private$notifyInfo(note),
               Warn  = private$notifyWarn(note),
               Error = private$notifyError(note)
        )
      } else {
        note <- paste("The usage for the Validator class is",
                      "validate(class, method, fieldName, value, level,",
                      " and msg)")
        private$notifyError(note)
      }
    }
  ),
  lock_class = FALSE,
  lock_objects = FALSE
)
