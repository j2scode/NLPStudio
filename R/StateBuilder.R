#==============================================================================#
#                                StateBuilder                                  #
#==============================================================================#
#' StateBuilder
#'
#' \code{StateBuilder} This class is responsible for (re)building objects and
#' restoring  object composite hierarchies to prior states. The statebuilder
#' object takes as its parameters, the current and restored objects. A
#' comparison method determines the elements common to both objects or
#' object composites, as well as the objects only found in the current (which
#' will be removed from the composition), and those found only in the
#' restored state (to be added to composition). The (re)build operation
#' happens in three stages The first stage iterates through the object
#' hierarchy, rebuilding objects one at a time via the VRestorer visitor.
#' In the second stage, objects found only in the current state are removed
#' from the hierarchy.  The last stage, objects found only in the
#' restored object hierarchy are instantiated and added to the composite
#' hierarchy.
#'
#' The StateBuilder class is a participant of the State family of classes.
#'
#' \strong{State Family of Classes Overview:}
#' The State family of classes is responsible for object persistence and
#' data recovery in the NLPStudio package and is comprised of the following
#' participants.
#'
#' \strong(State Class Family Participants:)
#' \itemize{
#'  \item State: This class oversees the process of saving, restoring and
#'  querying states. It takes requests from client applications and dispatches
#'  instantiates the appropriate visitor to fulfill the request. This class
#'  also provides state query functionality.
#'  \item StateManager: This class has a persistent single object that
#'  receives a save/restore request from a visitor, keeps track of the
#'  saved states, and their locations, and dispatches the appropriate
#'  save/restore method within the StateBuilder class to serialize or
#'  deserialize the object.
#'  \item StateBuilder: This class is responsible for (re)building objects from prior states.
#'  \item VRestoreState: This visitor class restores an object to a prior designated state.
#'  }
#'
#' @section Methods:
#' The following methods are defined for this class:
#' \describe{
#'  \item{\code{restore(current, restored)}}{Method for restoring objects to prior states.}
#'  }
#'
#' @param current Object of the Lab, DocumentCollection, or Document classes in its current state.
#' @param restored Object of the Lab, DocumentCollection, or Document classes in a prior state.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family State Classes
#' @export
StateBuilder <- R6::R6Class(
  classname = "StateBuilder",
  lock_objects = FALSE,
  private = list(
    ..currentObjects = list(),
    ..restoredObjects = list(),
    ..comparison = list(),

    compare = function(current, restored) {

      createList = function(object) {
        #TODO: Move to external iterator.
        kids <- object$getChildren()
        lapply(kids, function(k) {
          createList(k)
        })
        o <- object$getObject()
        objectList[[o$name]] <- object
      }

      private$..currentObjects <- createList(current)
      private$..restoredObjects <- createList(restored)

      currentNames <- names(private$..currentObjects)
      restoredNames <- names(private$..restoredObjects)

      private..comparison = list(
        update = intersect(currentNames, restoredNames),
        add = setdiff(restoredNames, currentNames),
        remove = setdiff(currentNames, restoredNames)
      )
    },

    update = function() {

      lapply(private$..comparison$update, function(u) {
        currentObject <- private$..currentObjects[[u]]
        restoredObject <- private$..restoredObjects[[u]]
        currentObject$acceptUpdate(restoredObject)
      })
    },

    add = function() {
      lapply(private$..comparison$add, function(a) {
        currentObject <- private$..currentObjects[[a]]
        restoredObject <- private$..restoredObjects[[a]]
        currentObject$acceptAdd(restoredObject)
      })
    },

    remove = function() {
      lapply(private$..comparison$remove, function(r) {
        currentObject <- private$..currentObjects[[r]]
        restoredObject <- private$..restoredObjects[[r]]
        currentObject$acceptRemove(restoredObject)
      })
    },

    associate = function(current, restored) {
      #TODO: implement via external iterator
      })
    }
  ),

  public = list(

    build = function(current, restored) {
      private$..compare(current, restored)
      private$..update()
      private$..add()
      private$..remove()
      private$..associate(current, restored)
    }
  )
)
