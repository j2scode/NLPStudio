## ---- Validate0
#==============================================================================#
#                                 Validate0                                    #
#==============================================================================#
#' Validate0
#'
#' @description
#' \code{Validate} Interface class for all validation routines in the NLPStudio package.
#'
#' This class provides an interface to the validation routines used in this
#' package.  The purpose of this family of classes is to provide custom handling
#' of warnings and errors allowing the calling methods to control the
#' error handling according to its business rules.
#'
#' @docType class
#'
#' @section Private Methods:
#' \describe{
#'  \item{\code{notifyInfo(note)}}{Renders a notification with level "info"}
#'  \item{\code{notifyWarn(note)}}{Renders a notification with level "warn"}
#'  \item{\code{notifyError(note)}}{Renders a notification with level "error", and stops processing}
#' }
#'
#' @section Public Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of Validate class}
#'  \item{\code{validate(cls, method, fieldName, value, level, msg, expect)}}{Validation method overwritten by subclasses}
#'  \item{\code{notify(cls, method, fieldName, value, level, msg, expect)}}{Renders a notification if outcome is not expected}
#' }
#'
#' @section Parameters:
#' @param cls Character string containing the name of the invoking class
#' @param method Character string containing the name of the invoking method
#' @param fieldName Character string containing the name of the field to validate
#' @param value The value of the field being validated
#' @param level Character string c("Error", "Warn", "Info)
#' @param msg Character vector containing the message to be displayed if test fails
#' @param expect Logical c(TRUE, FALSE) indicating expected outcome
#'
#' @return A logical, TRUE if outcome was expected, FALSE otherwise
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
Validate0 <- R6::R6Class(
  "Validate0",
  private = list(
    notifyInfo  = function(note) futile.logger::flog.info(note, name = "white"),
    notifyWarn  = function(note) futile.logger::flog.warn(note, name = "yellow"),
    notifyError = function(note) {
      futile.logger::flog.error(note, name = "red")
      stop()
    }
  ),

  public = list(
    validate = function(cls, method, fieldName, value, level, msg, expect = NULL)
      stop("This method is not available"),
    notify = function(cls, method, fieldName, value, level, msg, expect = NULL) {

      if (exists("cls") & exists('method') & exists("value") &
          exists("level") & exists("msg")) {

        level <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2",
                      level, perl = TRUE)

        note <- paste0(level, " in class '", cls, "', method '", method,
                       "' with variable '", fieldName, "' with value '",
                       value, "'. ", msg)
        switch(level,
               Info  = private$notifyInfo(note),
               Warn  = private$notifyWarn(note),
               Error = private$notifyError(note)
        )
      } else {
        note <- paste("The usage for the Validate0 class is",
                      "validate(cls, method, fieldName, value, level,",
                      " and msg)")
        private$notifyError(note)
      }
    }
  ),
  lock_class = FALSE,
  lock_objects = FALSE
)
