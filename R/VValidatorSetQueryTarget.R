#==============================================================================#
#                         VValidatorSetQueryTarget                             #
#==============================================================================#
#' VValidatorSetQueryTarget
#'
#'
#' \code{VValidatorSetQueryTarget} Visitor class responsible for validating the request to
#' SetQueryTarget.
#'
#' \strong{VValidatorSetQueryTarget Methods:}
#' The VValidatorSetQueryTarget methods are as follows:
#'  \itemize{
#'  \item{\code{nlpStudio(object, target)}}{Verifies that the requesting object can query an NLPStudio object.}
#'   \item{\code{lab(object, target)}}{Verifies that the requesting object can query an LAB object.}
#'   \item{\code{documentCollection(object, target)}}{Verifies that the requesting object can query a DocumentCollection object.}
#'   \item{\code{stateManager(object, target)}}{Verifies that the requesting object can query an StateManager object.}
#'   \item{\code{historian(object, target)}}{Verifies that the requesting object can query a Historian object.}
#' }
#'
#' @param object Requesting object
#' @param target Target object
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorSetQueryTarget <- R6::R6Class(
  classname = "VValidatorSetQueryTarget",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    ..object = character(0),
    ..target = character(0),
    ..targetClasses = c("NLPStudio", "Lab", "DocumentCollection",
                        "Historian", "StateManager"),

    validate = function(target) {

      # Confirm class of requesting object
      v <- ValidatorClass$new()
      if (v$validate(class = class(private$..object)[1], method = "object",
                     fieldName = "object", value = private$..object, level = "Error",
                     msg = paste0("Object of class ", class(private$..object)[1],
                                  " is not authorized to query objects of class ",
                                  class(target)[1], ". ",
                                 "See ?", class(private$..object)[1],
                                 " for further assistance."),
                     expect = "Curator") == FALSE) {
        return(FALSE)
      }

      # Confirm class of target object
      if (v$validate(class = class(target)[1], method = "target",
                     fieldName = "target", value = target, level = "Error",
                     msg = paste0("Unable to query object of the ",
                                  class(target)[1], " class.",
                                  "See ?", class(private$..object)[1],
                                  " for further assistance."),
                     expect = private$..targetClasses) == FALSE) {
        return(FALSE)
      }

      return(TRUE)
    }
  ),
  public = list(

    initialize = function(object, target) {
      private$..object <- object
      private$..target <- target
      invisible(self)
    },
    nlpStudio = function(target) {
      return(private$validate(target))
    },
    lab = function(target) {
      return(private$validate(target))
    },
    documentCollection = function(target) {
      return(private$validate(target))
    },
    stateManager = function(target) {
      return(private$validate(target))
    },
    historian = function(target) {
      return(private$validate(target))
    }
  )
)
