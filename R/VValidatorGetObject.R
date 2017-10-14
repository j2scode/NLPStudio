#==============================================================================#
#                              VValidatorexposeObject                             #
#==============================================================================#
#' VValidatorexposeObject
#'
#'
#' \code{VValidatorexposeObject} Visitor class responsible for validating requests
#' for object information.  Only permits objects of the State class to invoke
#' exposeObject methods.
#'
#' \strong{VValidatorexposeObject Methods:}
#' The VValidatorexposeObject methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object, ...)}}{Method for validating the instantiation of the NLPStudio object}
#'   \item{\code{lab(object, ...)}}{Method for validating the instantiation of the Lab object}
#'   \item{\code{documentCollection(object, ...)}}{Method for validating the instantiation of the DocumentCollection object.}
#'   \item{\code{documentText(object, ...)}}{Method for validating the instantiation of the DocumentText object.}
#'   \item{\code{documentCsv(object, ...)}}{Method for validating the instantiation of the DocumentCsv object.}
#'   \item{\code{documentRdata(object, ...)}}{Method for validating the instantiation of the DocumentRdata object.}
#'   \item{\code{documentXlsx(object, ...)}}{Method for validating the instantiation of the DocumentXlsx object.}
#' }
#'
#' @param object The object for which the exposeObject method is invoked.
#' @param requester The object invoking the exposeObject method.
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorexposeObject <- R6::R6Class(
  classname = "VValidatorexposeObject",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    validate = function(object, requester) {

      # Confirm required parameters are not missing.
      if (missing(requester)) {
        v <- Validator0$new()
        v$notify(class = class(object)[1], method = "exposeObject", fieldName = "requester",
                 value = "", level = "Error",
                 msg = paste0("Requester parameter is missing with no default. ",
                              "Methods invoking this method must send 'self' as ",
                              "a parameter to the exposeObject() method. ",
                              "See ?", class(object)[1], " for further assistance."),
                 expect = NULL)
        return(FALSE)
      }

      # Confirm class of requester
      v <- ValidatorClass$new()
      if (v$validate(class = class(object)[1], method = "exposeObject",
                     fieldName = "class(requester)", value = requester,
                     level = "Error",
                     msg = paste0("Objects of the ", class(requester)[1],
                                  " class can not invoke the exposeObject()",
                                  " method. ", "See ?", class(object)[1],
                                  " for further assistance."),
                     expect = classes) == FALSE) {
        return(FALSE)
      }
    }
  ),

  public = list(

    nlpStudio = function(object, requester) {
      return(private$validate(object, requester))
    },

    lab = function(object, requester) {
      return(private$validate(object, requester))
    },

    documentCollection = function(object, requester) {
      return(private$validate(object, requester))
    },

    documentText = function(object, requester) {
      return(private$validate(object, requester))
    },

    documentCsv = function(object, requester) {
      return(private$validate(object, requester))
    },

    documentRdata = function(object, requester) {
      return(private$validate(object, requester))
    },

    documentXlsx = function(object, requester) {
      return(private$validate(object, requester))
    }
  )
)
