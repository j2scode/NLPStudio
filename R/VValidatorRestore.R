#==============================================================================#
#                                   VValidatorRestore                          #
#==============================================================================#
#' VValidatorRestore
#'
#'
#' \code{VValidatorRestore} Visitor class responsible for validating the
#' parameters of the restore operation.
#'
#' \strong{VValidatorRestore Methods:}
#' The VValidatorRestore methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object, ...)}}{Method for validating the instantiation of the NLPStudio object}
#'   \item{\code{lab(object, ...)}}{Method for validating the instantiation of the NLPStudio object}
#'   \item{\code{documentCollection(object, ...)}}{Method for validating the instantiation of the NLPStudio object.}
#'   \item{\code{documentText(object, ...)}}{Method for validating the instantiation of the NLPStudio object.}
#'   \item{\code{documentCsv(object, ...)}}{Method for validating the instantiation of the NLPStudio object.}
#'   \item{\code{documentRdata(object, ...)}}{Method for validating the instantiation of the NLPStudio object.}
#'   \item{\code{documentXlsx(object, ...)}}{Method for validating the instantiation of the NLPStudio object.}
#' }
#'
#' @param object The object in its current state
#' @param ... Parameters
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorRestore <- R6::R6Class(
  classname = "VValidatorRestore",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    validate = function(object, ...) {

      if (missing(prior)) {
        v <- Validate0$new()
        v$notify(class = class(object)[1], method = "restore",
                   fieldName = "prior", value = "", level = "Error",
                   msg = paste0("Prior state object parameter missing",
                                " with no default. See ?", class(object)[1],
                                " for further assistance."),
                   expect = NULL)
        return(FALSE)
      }

      v <- ValidateClass$new()
      if (v$validate(class = class(object)[1], method = "restore",
                     fieldName = "prior", value = prior, level = "Error",
                     msg = paste0("Unable to restore an object of the ",
                                  class(object)[1], " class ",
                                  "to the state of an object of class ",
                                  class(prior)[1], ". ",
                                  "Please see ?", class(object)[1],
                                  " for further assistance."),
                     expect = class(object)[1]) == FALSE) {
        return(FALSE)
      }
    }
  ),

  public = list(

    nlpStudio = function(object,...) {
      return(private$validate(object, ...))
    },

    lab = function(object,...) {
      return(private$validate(object, ...))
    },

    documentCollection = function(object,...) {
      return(private$validate(object, ...))
    },

    documentText = function(object,...) {
      return(private$validate(object, ...))
    },

    documentCsv = function(object,...) {
      return(private$validate(object, ...))
    },

    documentRdata = function(object,...) {
      return(private$validate(object, ...))
    },

    documentXlsx = function(object,...) {
      return(private$validate(object, ...))
    }
  )
)
