#==============================================================================#
#                                 StateRestore0                                #
#==============================================================================#
#' StateRestore0
#'
#' \code{StateRestore0} This abstract class defines the interface for deserializing
#'  and restoring objects in a specific state from disk.
#'
#' The StateRestore family of classes is an implementation of the strategy
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows for the
#' definition of a common interface to varying algorithms used to deserialize
#' and restore objects back into the NLPStudio package.
#'
#' \strong{Family of StateRestore0 Classes Overview:}
#' The StateRestore0 family of classes is responsible for deserializing and
#' restoring, objects from disk and is comprised of the following participants.
#'
#' \strong(StateRestore0 Class Family Participants:)
#' \itemize{
#'  \item StateRestore0: This abstract class is responsible for defining the common
#'  interface to be used by the participants of the StateRestore0 family of classes.
#'  \item StateRestore0bject: This class is responsible for deserializing and restoring
#'  objects from disk.
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
#' @param stateId Character string which uniquely identifies an object and its state at a specific point in time.
#' @return state The object in the state requested.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family State Classes
#' @export
StateRestore0 <- R6::R6Class(
  classname = "StateRestore0",
  lock_objects = FALSE,
  public = list(

    validateObject = function(requestId, stateId) stop("Method is not available from StateRestore0, an abstract class!"),
    restoreObject = function(requestId, stateId) stop("Method is not available from StateRestore0, an abstract class!")
  )
)
