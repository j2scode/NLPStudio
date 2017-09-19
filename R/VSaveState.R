## ---- VSaveState
#==============================================================================#
#                                   VSaveState                                 #
#==============================================================================#
#' VSaveState
#'
#'
#' \code{VSaveState} Visitor class responsible for saving the current state of the object which accepts this visitor.
#'
#' \strong{VSaveState Methods:}
#' The VSaveState methods are as follows:
#'  \itemize{
#'   \item{\code{lab(object)}}{Method for saving the current state of Lab objects.}
#'   \item{\code{documentCollection(object)}}{Method for saving the current state of DocumentCollection objects.}
#'   \item{\code{document(object)}}{Method for saving the current state of Document objects.}
#' }
#'
#' @param object Object to be saved
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @export
VSaveState <- R6::R6Class(
  classname = "VSaveState",
  private = list(

    validateObject = function(object) {

      constants <- Constants$new()

      v <- ValidateClass$new()
      if (v$validate(class = "VSaveState", method = method, fieldName = "class(object)",
                     level = "Error", value = class(object)[1],
                     msg = paste("Unable to save object.",
                                 "Object is not of a serializable class.",
                                 "See ?VSaveState for assistance."),
                     expect = constants$getStateClasses()) == FALSE) {
        stop()
      }
    }
  ),
  public = list(

    lab = function()  {
      private$..validateObject(object)
      stateManager$saveState(object)
    },
    documentCollection = function()  {
      private$..validateObject(object)
      stateManager$saveState(object)
    },
    document = function()  {
      private$..validateObject(object)
      stateManager$saveState(object)
    }
  )
)
