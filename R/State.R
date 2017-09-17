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
#'  \item StateManager: This class has a persistent single object that
#'  receives a save/restore request from a visitor, keeps track of the
#'  saved states, and their locations, and dispatches the appropriate
#'  save/restore method within the State class to serialize or
#'  deserialize the object.
#'  \item State: This class is responsible for serializing and deserializing
#'  object states.
#'  \item VSaveState: This visitor class is invoked through the accept method
#'  of the originator object, and dispatches a request to the StateManager
#'  class for logging and fulfillment.
#'  \item VRestoreState: This visitor class is invoked through the accept method
#'  of the originator object, and dispatches a request to the StateManager
#'  class for logging and fulfillment. This class also manages the process
#'  of restoring composite objects to the prior designated state by iterating
#'  through the composite hierarchy.
#'  }
#'
#' @section Methods:
#' The following methods are defined for this class:
#' \describe{
#' \item{\code{new()}}{Method for instantiating objects of the State class. Obtains the list of serializeable classes for validation puroses.}
#'  \item{\code{getObject()}}{Method that returns State object data.}
#'  \item{\code{query(dateFrom, dateTo, class, objectName)}}{Method for quering the saved states. Returns a list of states matching the query parameters.}
#'  \item{\code{saveState(object, stateDesc)}}{Method for saving the state of a serializable object. This method invokes the accept method on the object which dispatches the appropriate save visitor.}
#'  \item{\code{restoreState(object, stateId)}}{Method for restoring an the designated object to the state identified by the stateId parameter. The method invokes the accept method on the object, which dispatches the appropriate restore visitor.}
#'  }
#'
#' @param dateFrom An ISO 8601 formatted date indicating the date from which states should be returned.
#' @param dateTo An ISO 8601 formatted date indicating the date to which states should be returned.
#' @param class A character string indicating the class of state objects that should be returned.
#' @param objectName A character string indicating the object for which states should be returned.
#' @param object A serializable object to be serialized or deserialized.
#' @param stateId Character string which uniquely identifies an object and its state at a specific point in time.
#' @param stateDesc Character string description for the state being saved.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family State Classes
#' @export
State <- R6::R6Class(
  classname = "State",
  lock_objects = FALSE,
  private = list(
    ..requestId = character(0),
    ..request = character(0),
    ..stateId = character(0),
    ..stateDesc = character(0),
    ..objectName = character(0),
    ..requested = character(0),
    ..completed = character(0),
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

    getObject = function() {
      state <- list(
       requestId = private$..requestId,
       request = private$..request,
       stateId = private$..stateId,
       stateDesc = private$..stateDesc,
       objectName = private$..objectName,
       requested = private$..requested,
       completed = private$..completed
      )
      return(state)
    },

    saveState = function(object, stateDesc) {

      method <- "saveState"

      # Validate Request
      if (private$validateRequest(object, method = method) == FALSE) stop()

      # Obtain object information
      o <- object$getObject()

      # Initiate Request variables
      tools <- Tools$new()
      private$..requestId <- paste0(o$name, "-", tools$makeRandomString())
      private$..request <- method
      private$..stateDesc <- stateDesc
      private$..objectName <- o$name
      private$..requested <- Sys.time()

      # Send accept request to the object
      private$..stateId <- object$acceptVSaveState(stateDesc)
      private$..completed <- Sys.time()

      invisible(self)
    },

    restoreState = function(key, stateId) {

      method <- "restoreState"

      # Validate Request
      if (private$validateRequest(method = method, key) == FALSE) stop()

      # Restore Object
      object <- readRDS(file.path(private$..statesPath, paste0(stateId, ".rds")))

      return(object)
    }
  )
)
