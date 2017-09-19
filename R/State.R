#==============================================================================#
#                                State                                         #
#==============================================================================#
#' State
#'
#' \code{State} This class takes receives query, save, and restore requests
#' from client applications and visitor methods, validates the request, and
#' dispatches the appropriate visitor class for fullfillment.
#'
#' The State class is a participant of the State family of classes.
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
#'  \item StateBuilder: This class oversees the process of restoring a composite
#'  to a prior state.
#'  \item StateManager: This class has a persistent single object that
#'  receives a save/restore request from a visitor, keeps track of the
#'  saved states, and their locations, and dispatches the appropriate
#'  save/restore method within the State class to serialize or
#'  deserialize the object.
#'  \item StateServer: This class is responsible for serializing and deserializing
#'  object states.
#'  }
#'
#' @section Methods:
#' The following methods are defined for this class:
#' \describe{
#' \item{\code{new()}}{Method for instantiating objects of the State class. Obtains the list of serializeable classes for validation puroses.}
#'  \item{\code{query(dateFrom, dateTo, class, objectName)}}{Method for quering the saved states. Returns a list of states matching the query parameters.}
#'  \item{\code{save(object)}}{Method for saving the state of a serializable object. This method invokes the accept method on the object which dispatches the appropriate save visitor.}
#'  \item{\code{restore(object)}}{Method for restoring an the designated object to the state identified by the stateId parameter. The method invokes the accept method on the object, which dispatches the appropriate restore visitor to obtain the prior state, then invokes the StateBuilder to return the composite to the prior state}
#'  }
#'
#' @param dateFrom A parameter of the query method. An ISO 8601 formatted date indicating the date from which states should be returned.
#' @param dateTo A parameter of the query method. An ISO 8601 formatted date indicating the date to which states should be returned.
#' @param class A parameter of the query method. A character string indicating the class of state objects that should be returned.
#' @param objectName A parameter of the query method. A character string indicating the object for which states should be returned.
#' @param object A serializable object to be serialized or deserialized.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family State Classes
#' @export
State <- R6::R6Class(
  classname = "State",
  lock_objects = FALSE,
  private = list(
    ..validClasses = character(0),

    validateRequest = function(object, method) {

      o <- object$getObject()

      v <- ValidateClass$new()
      if (v$validate(class = "State", method = method, fieldName = "class(object)",
                     level = "Error", value = class(object)[1],
                     msg = paste("Unable process the save/restore request.",
                                 "Object", o@name, "is not a serializable object.",
                                 "See ?State for assistance."),
                     expect = private$..validClasses) == FALSE) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }
  ),

  public = list(

    initialize = function() {
      constants <- Constants$new()
      private$..validClasses = constants$getStateClasses()

    },

    save = function(object) {

      method <- "save"

      # Validate Request
      if (private$validateRequest(object, method = method) == FALSE) stop()

      # Obtain object information
      o <- object$getObject()

      # Initiate Request variables
      tools <- Tools$new()
      private$..requestId <- paste0(o$name, "-", tools$makeRandomString())
      private$..request <- method
      private$..objectName <- o$name
      private$..state <- o$state
      private$..stateId <- o$stateId
      private$..requested <- Sys.time()

      # Send accept request to the object
      private$..stateId <- object$acceptVSaveState(object)
      private$..completed <- Sys.time()

      invisible(self)
    },

    restore = function(object) {

      method <- "restoreState"

      # Validate Request
      if (private$validateRequest(object, method = method) == FALSE) stop()

      # Send accept request to the object
      restoredObject <- object$acceptVRestoreState(object)

      # Rebuild Document / Composite
      StateBuilder$new()



      private$..completed <- Sys.time()
    }
  )
)
