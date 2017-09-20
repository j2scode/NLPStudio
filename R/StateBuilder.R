#==============================================================================#
#                                StateBuilder                                  #
#==============================================================================#
#' StateBuilder
#'
#' \code{StateBuilder} This class is responsible for (re)building objects and
#' restoring  object composite hierarchies to prior states. The statebuilder
#' object takes as its parameters, the current and prior state objects. A
#' comparison method determines the elements common to both objects or
#' object composites, as well as the objects only found in the current (which
#' will be removed from the composition), and those found only in the
#' prior state (to be reinstated and added to composition). The (re)build operation
#' happens in the following four stages.
#'
#' \itemize{
#'  \item{Stage 1: Iterate through the object hierarchy and those objects
#'  in the current state that are also in the prior state are updated with
#'  values from the prior state object.}
#'  \item{State 2: Objects in the prior state object that are no longer in
#'  the current state object are added to the global environment.}
#'  \item{Stage 3: Objects in the prior state object that are no longer in
#'  the current composite are added to the current state object.}
#'  \item{Stage 4: Objects not in the prior state object, that exist in
#'  the current state object are removed.}
#'
#'
#' The StateBuilder class is a participant of the State family of classes.
#'
#' \strong{State Family of Classes Overview:}
#' The State family of classes is responsible for object persistence and
#' data recovery in the NLPStudio package and is comprised of the following
#' participants.
#'
#' \strong{State Class Family Participants:}
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
#'  }
#'
#' \strong{StateBuilder Collaborators:}
#'  \itemize{
#'   \item Lab: The class containing document collections.
#'   \item DocumentCollections: The composite member of the Document0 Composition set of classes.  Contains documents and other collections. StateBuilder invokes restore processes through various accept methods in this class.
#'   \item Document: The leaf member of the Document0 Composition set of classes. StateBuilder invokes restore processes through various accept methods in this class.
#'   \item Vrestore: A visitor class responsible for restoring an existing object to a prior state by updating its members,field-by-field.
#'   \item VAddChild: A visitor class responsible for adding a prior state child object to a prior state parent object.
#'   \item VRemoveChild: A visitor class responsible for removing a child object from a restored object, when the child is not a part of the restored composition.
#'  }
#'
#'
#' \strong{StateBuilder Methods:}
#' The following methods are defined for this class:
#' \describe{
#'  \item{\code{restore(current, prior)}}{Method for restoring objects to prior states.}
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
    ..priorObjects = list(),
    ..comparison = list(),

    compare = function(current, prior) {

      getObjects = function(object) {
        kids <- object$getChildren()
        lapply(kids, function(k) {
          getObjects(k)
        })
        o <- object$getObject()
        objectList[[o$name]] <- object
      }

      private$..currentObjects <- getObjects(current)
      private$..priorObjects <- getObjects(prior)

      currentNames <- names(private$..currentObjects)
      priorNames <- names(private$..priorObjects)

      private..comparison = list(
        restore = intersect(currentNames, priorNames),
        reinstate = setdiff(priorNames, currentNames),
        remove = setdiff(currentNames, priorNames)
      )
    },

    update = function() {

      lapply(private$..comparison$restore, function(u) {
        currentObject <- private$..currentObjects[[u]]
        priorObject <- private$..priorObjects[[u]]
        vUpdate <- VUpdate$new()
        currentObject$acceptVUpdate(vUpdate, priorObject)
      })
    },

    reinstate = function() {
      lapply(private$..comparison$reinstate, function(r) {
        assign(r, private$..currentObjects[[r]], envir = .GlobalEnv)
      })
    },

    addChild = function() {
      lapply(private$..currentObjects, function(c) {
        current <- c$getObject()
        currentName <- current$name
        priorObject <- private$priorObjects[[currentName]]
        parent <- priorObject$getAncestor()
        parent <- parent$getObject()
        parentName <- parent$name
        parent <- private$..currentObjects[[parentName]]
        vAddChild <- VAddChild$new()
        parent$acceptVAddChild(vAddChild, c)
      })
    },

    removeChild = function() {
      lapply(private$..comparison$remove, function(r) {
        currentObject <- private$..currentObjects[[r]]
        parent <- currentObject$getAncestor()
        vRemoveChild <- VRemoveChild$new()
        parent$acceptVRemoveChild(vRemoveChild, currentObject)
      })
      rm(ls = list(private$..comparison$remove), envir = .GlobalEnv)
    }
  ),

  public = list(

    build = function(current, prior) {
      private$..compare(current, prior)
      private$..update()
      private$..reinstate()
      private$..addChild()
      private$..removeChild()
    }
  )
)
