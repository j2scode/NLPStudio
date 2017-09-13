#==============================================================================#
#                             StateSaveObject                                  #
#==============================================================================#
#' StateSaveObject
#'
#' \code{StateSaveObject} This class saves serializable R6 objects to disk.
#'
#' The StateSaveObject class is a participant of the State family of classes, an
#' implementation of the strategy pattern documented in the book "Design
#' Patterns: Elements of Reusable Object-Oriented Software" by Erich Gamma,
#' Richard Helm, Ralph Johnson and John Vlissides (hence Gang of Four).
#'
#' \strong{Family of StateSave Classes Overview:}
#' The StateSave family of classes is responsible for serializing and saving
#' objects to disk and is comprised of the following participants.
#'
#' \strong(StateSave Class Family Participants:)
#' \itemize{
#'  \item StateSave0: This abstract class ia responsible for defining the common
#'  interface to be used by the participants of the StateSaveObject family of classes.
#'  \item StateSaveObject: This class is responsible for serializing and storing
#'  objects in their current state to disk.
#'  \item StateSaveFile: This class is responsible for serializing and storing
#'  objects of the File class in their current state to disk.
#'  }
#'
#' @section Methods:
#' The following methods are defined for this class:
#' \describe{
#'  \item{\code{validateObject(requestId, stateId, object)}}{Validates the StateSaveObject object and its parameters.}
#'  \item{\code{saveState(requestId, stateId, object)}}{Method for serializing and storing the object and its meta data to disk.}
#'  }
#'
#' @param object A serializable object to be serialized and saved.
#' @param requestId Character string which uniquely identifies a save/restore request.
#' @param stateId Character string which uniquely identifies an object and its state at a specific point in time.
#' @return state List with the details of the save request to be returned to the calling method.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family State Classes
#' @export
StateSaveObject <- R6::R6Class(
  classname = "StateSaveObject",
  lock_objects = FALSE,
  public = list(

    validateObject = function(requestId, stateId, object) {

      if (stateManager$verifyRequestId(requestId) == FALSE) {
        v <- Validate0$new()
        v$notify(cls = "StateSaveObject", method = "saveState",
                 fieldName = "requestId", value = requestId, level = "Error",
                 msg = paste("RequestId parameter was not verified by the StateManager.",
                             "See ?StateSaveObject for further assistance."),
                 expect = TRUE)
        stop()
      }

      if (stateManager$verifyStateId(stateId) == FALSE) {
        v <- Validate0$new()
        v$notify(cls = "StateSaveObject", method = "saveState",
                 fieldName = "stateId", value = stateId, level = "Error",
                 msg = paste("StateId parameter was not verified by the StateManager.",
                             "See ?StateSaveObject for further assistance."),
                 expect = TRUE)
        stop()
      }

      if (stateManager$verifyClass(object) == FALSE) {
        v <- Validate0$new()
        v$notify(cls = "StateSaveObject", method = "saveState",
                 fieldName = "class(object)", value = class(object)[1], level = "Error",
                 msg = paste("Object is not a serializable object.",
                             "See ?StateSaveObject for further assistance."),
                 expect = TRUE)
        stop()
      }
      return(TRUE)
    },
    saveState = function(requestId, stateId, object) {

      # Validate Request
      self$validateObject(requestId, stateId, object)

      # Obtain path and object information
      p <- nlpStudio$getPaths()
      o <- object$getObject()

      # Prepare the object for archiving
      state <- list(
        request = "Save",
        requestId = requestId,
        stateId = stateId,
        class = class(object)[1],
        name = o$name,
        desc = o$desc,
        path = p$stateFile,
        object = object,
        created = Sys.time()
      )

      # Retrieve current list of saved objects, append the list, and save
      states <- base::readRDS(file = p$stateFile)
      states[[stateId]] <- state
      base::saveRDS(states, file = p$stateFile)

      # Remove object from state and return
      state$object <- NULL

      return(state)
    }
  )
)
