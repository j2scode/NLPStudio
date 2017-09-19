#==============================================================================#
#                                 StateSave0                                   #
#==============================================================================#
#' StateSave0
#'
#' \code{StateSave0} This abstract class defines the interface for serializing
#'  and saving objects in their current state to disk.
#'
#' The StateSave family of classes is an implementation of the strategy
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows for the
#' definition of a common interface to varying algorithms used to serialize
#' and save objects in the NLPStudio package.
#'
#' \strong{Family of StateSave Classes Overview:}
#' The StateSave family of classes is responsible for serializing and saving
#' objects to disk and is comprised of the following participants.
#'
#' \strong(StateSave Class Family Participants:)
#' \itemize{
#'  \item StateSave0: This abstract class is responsible for defining the common
#'  interface to be used by the participants of the StateSave family of classes.
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
StateSave0 <- R6::R6Class(
  classname = "StateSave0",
  lock_objects = FALSE,
  public = list(

    validateObject = function(requestId, stateId, object) stop("Method is not available from StateSave0, an abstract class!"),
    saveState = function(requestId, stateId, object) stop("Method is not available from StateSave0, an abstract class!")
    )
)
