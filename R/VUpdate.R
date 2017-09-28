#==============================================================================#
#                                   VCurator                                   #
#==============================================================================#
#' VCurator
#'
#'
#' \code{VCurator} Visitor class responsible for restoring an object to a prior stete
#'
#' \strong{VCurator Methods:}
#' The VCurator methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(current, prior)}}{Method for restoring the NLPStudio object a prior state.}
#'   \item{\code{lab(current, prior)}}{Method for restoring a Lab object to a prior state.}
#'   \item{\code{documentCollection(current, prior)}}{Method for restoring a DocumentCollection object to a prior state.}
#'   \item{\code{documentText(current, prior)}}{Method for updating a DocumentText object to a prior state.}
#'   \item{\code{documentCsv(current, prior)}}{Method for updating a DocumentCsv object to a prior state.}
#'   \item{\code{documentRdata(current, prior)}}{Method for updating a DocumentRdata object to a prior state.}
#'   \item{\code{documentXlsx(current, prior)}}{Method for updating a DocumentXlsx object to a prior state.}
#' }
#'
#' @param current The object in its current state
#' @param prior The object restored from a prior state.
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family State Classes
#' @export
VCurator <- R6::R6Class(
  classname = "VCurator",
  private = list(

    validateRequest = function(method, current, prior) {

      v <- ValidatorClass$new()
      if (v$validate(class = "VCurator", method = method, fieldName = "class(prior)",
                     level = "Error", value = prior,
                     msg = paste0("Unable to restore object of class ",
                                 class(current)[1], " to state of ",
                                 "object class ", class(prior)[1], ". ",
                                 "See ?VCurator for assistance."),
                     expect = class(current)[1]) == FALSE) {
        stop()
      }
    }
  ),
  public = list(

    nlpStudio = function(current, prior)  {
      method <- match.call()[[1]]
      private$..validateRequest(method, current, prior)
      current$restore(self, prior)
    },
    lab = function(current, prior)  {
      method <- match.call()[[1]]
      private$..validateRequest(method, current, prior)
      current$restore(self, prior)
    },
    documentCollection = function(current, prior)  {
      method <- match.call()[[1]]
      private$..validateRequest(method, current, prior)
      current$restore(self, prior)
    },
    documentText = function(current, prior)  {
      method <- match.call()[[1]]
      private$..validateRequest(method, current, prior)
      current$restore(self, prior)
    },
    documentCsv = function(current, prior)  {
      method <- match.call()[[1]]
      private$..validateRequest(method, current, prior)
      current$restore(self, prior)
    },
    documentRdata = function(current, prior)  {
      method <- match.call()[[1]]
      private$..validateRequest(method, current, prior)
      current$restore(self, prior)
    },
    documentXlsx = function(current, prior)  {
      method <- match.call()[[1]]
      private$..validateRequest(method, current, prior)
      current$restore(self, prior)
    }
  )
)
