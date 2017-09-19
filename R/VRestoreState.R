## ---- VRestoreState
#==============================================================================#
#                                   VRestoreState                              #
#==============================================================================#
#' VRestoreState
#'
#'
#' \code{VRestoreState} Visitor class responsible for restoring objects to prior states
#'
#' \strong{VRestoreState Methods:}
#' The VRestoreState methods are as follows:
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
VRestoreState <- R6::R6Class(
  classname = "VRestoreState",
  private = list(

    validateObject = function(object) {

      constants <- Constants$new()

      v <- ValidateClass$new()
      if (v$validate(class = "VRestoreState", method = method, fieldName = "class(object)",
                     level = "Error", value = class(object)[1],
                     msg = paste("Unable to restore object.",
                                 "Object is not of a serializable class.",
                                 "See ?VRestoreState for assistance."),
                     expect = constants$getStateClasses()) == FALSE) {
        stop()
      }
    }
  ),
  public = list(

    lab = function()  {
      private$..validateObject(object)

      private$..reinstate
    },
    documentCollection = function()  {
      private$..validateObject(object)
      state <- stateManager$restoreState(object)
    },
    document = function()  {
      private$..validateObject(object)
      state <- stateManager$restoreState(object)
    }
  )
)
