#==============================================================================#
#                                  State                                       #
#==============================================================================#
#' State
#'
#' \code{State} This class is responsible for serializing and deserializing
#' object states. The class is comprised of two methods, saveState and
#' restoreState. The saveState method takes as its parameters, a state
#' identifier and the object to be saved, then serializes and saves the object
#' to the States directory. The restoreState method takes a state identifier
#' as its parameter and reads the object, designated by the state identifier,
#' from file.
#'
#' The State class is a participant of the State family of classes.
#'
#' \strong{State Family of Classes Overview:}
#' The State family of classes is responsible for object persistence and
#' data recovery in the NLPStudio package and is comprised of the following
#' participants.
#'
#' \strong(StateSave Class Family Participants:)
#' \itemize{
#'  \item StateManager: This class receives save and retore requests from
#'  serializable objects, maintains the inventory of states, and dispatches
#'  the save/restore method to a State object.
#'  \item State: This class is responsible for serializing and deserializing
#' object states.
#'  }
#'
#' @section Methods:
#' The following methods are defined for this class:
#' \describe{
#'  \item{\code{saveState(stateId, object)}}{Method for serializing and storing the object to disk.}
#'  \item{\code{restoreState(stateId)}}{Method for deserializing and returning the object from disk at state designated by the stateId parameter.}
#'  }
#'
#' @param object A serializable object to be serialized and saved.
#' @param stateId Character string which uniquely identifies an object and its state at a specific point in time.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family State Classes
#' @export
State <- R6::R6Class(
  classname = "State",
  lock_objects = FALSE,
  private = list(
    ..key = character(0),
    ..statesPath = character(0),
    ..stateClasses = character(0),
    ..stateFilePath = character(0),

    validateRequest = function(method, key, object = NULL) {

      if (key != private$..key) {
        v <- Validate0$new()
        v$notify(class = "State", method = method,
                 fieldName = "key", value = key, level = "Error",
                 msg = paste("Unable to save state. The request did not",
                             "originate from the stateManager.",
                             "See ?State for further assistance."),
                 expect = TRUE)
        return(FALSE)
      }

      if (!(is.null(object))) {

        v <- ValidateClass$new()
        if (v$validate(class = "State", method = method,
                   fieldName = "class(object)", value = class(object)[1], level = "Error",
                   msg = paste("Object is not a serializable object.",
                               "See ?State for further assistance."),
                   expect = Constants$getStateClasses()) == FALSE) {
          return(FALSE)
        }
      }
      return(TRUE)
    }
  ),

  public = list(

    initialize = function(key) {
      private$..key <- key
      constants <- Constants$new()
      private$..statesPath <- constants$getStatesPath()
      private$..stateClasses <- constants$getStateClasses()
      return(self)
    },

    saveState = function(key, stateId, object) {

      # Validate Request
      if (private$validateRequest(method = "saveState", key, object) == FALSE) stop()

      # Save object
      private$..stateFilePath <- file.path(private$..statesPath, paste0(stateId, ".rds"))
      base::saveRDS(object, file = private$..stateFilePath)

      return(self)
    },

    restoreState = function(key, stateId) {

      # Validate Request
      if (private$validateRequest(method = "restoreState", key) == FALSE) stop()

      # Restore Object
      object <- readRDS(file.path(private$..statesPath, paste0(stateId, ".rds")))

      return(object)
    }
  )
)
