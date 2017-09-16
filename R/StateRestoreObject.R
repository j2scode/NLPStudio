#==============================================================================#
#                            StateRestoreObject                                #
#==============================================================================#
#' StateRestoreObject
#'
#' \code{StateRestoreObject} This class restores and returns serialized R6
#' objects from disk. It is a participant of the StateRestore0 family of
#' classes.
#'
#' The StateRestore family of classes is an implementation of the strategy
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows for the
#' definition of a common interface to varying algorithms used to deserialize
#' and restore objects back into the NLPStudio package.
#'
#' \strong{Family of StateRestore0 Classes Overview:}
#' The StateRestoreObject family of classes is responsible for deserializing and
#' restoring, objects from disk and is comprised of the following participants.
#'
#' \strong(StateRestoreO Class Family Participants:)
#' \itemize{
#'  \item StateRestore0: The abstract class responsible for defining the common
#'  interface to be used by the participants of the StateRestore0 family of classes.
#'  \item StateRestoreObject: This class is responsible for deserializing and
#'  returning objects from disk.
#'  \item StateRestoreFile: This class is responsible for deserializing and
#'  returning objects of the File class from disk.
#'  }
#'
#' @section Methods:
#' The following methods are defined for this class:
#' \describe{
#'  \item{\code{validateObject(requestId, stateId)}}{Base method not implemented for this abstract class.}
#'  \item{\code{restoreState(requestId, stateId)}}{Base method not implemented for this abstract class.}
#'  }
#'
#' @param requestId Character string indicating the unique request identifier.
#' @param stateId Character string which uniquely identifies an object and its state at a specific point in time.
#' @return state The object in the state requested.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family State Classes
#' @export
StateRestoreObject <- R6::R6Class(
  classname = "StateRestoreObject",
  lock_objects = FALSE,
  public = list(

    validateObject = function(requestId, stateId) {

      if (stateManager$verifyRequestId(requestId) == FALSE) {
        v <- Validate0$new()
        v$notify(class = "StateRestoreObject", method = "restoreState",
                 fieldName = "requestId", value = requestId, level = "Error",
                 msg = paste("RequestId parameter was not verified by the StateManager.",
                             "See ?StateRestoreObject for further assistance."),
                 expect = TRUE)
        stop()
      }

      if (stateManager$verifyStateId(stateId) == FALSE) {
        v <- Validate0$new()
        v$notify(class = "StateRestoreObject", method = "restoreState",
                 fieldName = "stateId", value = stateId, level = "Error",
                 msg = paste("StateId parameter was not verified by the StateManager.",
                             "See ?StateRestoreObject for further assistance."),
                 expect = TRUE)
        stop()
      }

      return(TRUE)
    },

    restoreState = function(requestId, stateId) {

      self$validateObject(requestId, stateId)

      # Obtain path information
      p <- nlpStudio$getPaths()

      # Obtain file of archived states
      states <- base::readRDS(file = p$stateFile)

      # Return if state indicated by the stateId exists.
      if (exists(states[[stateId]])) {
        return(states[[stateId]])
      } else {
        v <- Validate0$new()
        v$notify(class = "StateRestoreObject", method = "restoreState",
                 fieldName = "stateId", value = stateId, level = "Error",
                 msg = paste("StateId parameter invalid. State not found.",
                             "See ?StateRestoreObject for further assistance."),
                 expect = TRUE)
        stop()
      }
    }
  )
)
