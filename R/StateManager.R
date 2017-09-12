#==============================================================================#
#                                 StateManager                                 #
#==============================================================================#
#' StateManager
#'
#' \code{StateManager} Class for managing persistence and states of objects
#' within the NLPStudio package.
#'
#' \strong{StateManager Class Overview:}
#' The StateManager class object provides overall management of object state
#' within the NLPStudio package. It accepts a save or restore request from
#' a client object, logs the request, dispatches the appropriate StateServer
#' (and, if requested, StateArchiver) object(s). Once the request is fulfilled
#' by the subclass(es), the object is returned to the StateManager.  The
#' StateManager adds the object with a designated stateId to its inventory
#' of states. A summary of the request is also stored for query and
#' restore purposes.
#'
#' \strong{State Class Family  Overview:}
#' The State family of classes is responsible for managing object persistence
#' and an archive / restore capability within the NLPStudio objects.
#'
#' \strong(State Class Family Participants:)
#' The participants of the family are as follows:
#' \itemize{
#'  \item StateManager: This class accepts save / restore requests from object
#'  methods, logs the request and dispatches requests to the StateServer, and,
#'  if the client requests that files be saved or restored, the StateArchiver
#'  class.
#'  \item StateServer: This class is responsible for saving and restoring
#'  objects from and to existing objects.
#'  \item StateArchiver: This class is responsible for archiving and
#'  restoring any files associated with an object being saved or restored.
#'  }
#'
#' @section Methods:
#' The methods for the StateServer class are as follows:
#' \describe{
#'  \item{\code{getState(stateId)}}{Returns the object at the state designated by the stateId parameter.}
#'  \item{\code{restoreState(stateId, files = FALSE)}}{Restores the object at the state designated by the stateId parameter to the global environment. If files is set TRUE, previously archived directories and files are restored. If the object is part of a composite, e.g. Document or DocumentCollection class object, and its parent does not exist, the files will be restored to an orphan object according to the class of the object, the files will be restored to the orphan object directory, and the parent will be changed accordingly. In this case the restored object may then be moved to another parent as required.}
#'  \item{\code{saveState(object, files = FALSE)}}{Saves the current state of an object and dispatches a StateServer object to save the state to disk. If the files parameter is set to true, a VisitorArchive object will be instantiated to compress and save the  files associated with the object to an archive directory.}
#' }
#'
#' @param object The object to be loaded or saved. Required parameter for the saveState method.
#' @param stateId The unique identifier for the object and state. Required parameter for the restoreState method.
#' @param files Logical indicating whether the files associated with an object should be archived.
#'
#' @return object The object to be returned from restoreState or the object passed to saveState.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family State Classes
#' @export
StateManager <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "StateManager",
        private = list(
          ..name = "stateManager",
          ..desc = "StateManager NLPStudio Object State Manager",
          ..log = list(),
          ..receipts = list(),
          ..created = character(0),
          ..modified = character(0),

          getSeqNum = function(name) {
            seqNum <- 1
            if (nrow(private$..log) > 0 ) {
              if (nrow(subset(private$..log,
                              request == "Save" &
                              as.Date(created) == as.Date(Sys.time()) &
                              objectName == name)) > 0) {
                seqNum <- private$..log %>%
                  filter(request == "Save" &
                           as.Date(created) == as.Date(Sys.time()) &
                           objectName == name) %>%
                  summarise(max(seqNum))
                seqNum <- seqNum + 1
              }
            }
            return(seqNum)
          }
        ),

        public = list(

          initialize = function() {
            private$..created <- Sys.time()
            private$..modified <- Sys.time()
            invisible(self)
          },

          getInstance = function() invisible(self),

          saveState = function(object) {

            # Validate
            if (missing(object)) {
              v <- Validate0$new()
              v$notify(cls = "StateManager", method = "saveState",
                       fieldName = "object", value = "", level = "Error",
                       msg = paste("Object parameter is missing with no default.",
                                   "See ?StateManager for further assistance."),
                       expect = TRUE)
              stop()
            }

            v <- ValidateClass$new()
            if (v$validate(cls = "StateManager", method = "saveState",
                           fieldName = "object", value = object, level = "Error",
                           msg = paste("Object is not a valid 'R6' object",
                                       "See ?StateServer for further assistance."),
                           expect = "R6") == FALSE) {
              stop()
            }

            # Obtain object information and format key variables
            o <- object$getObject()
            seqNum <- private$getSeqNum(o$name)
            stateId <- paste0(o$name,"-", as.Date(Sys.time()), "-", seqNum)

            # Archive the requested object and associated files
            stateServer <- StateServer$new()
            objectConfirmation <- stateServer$saveState(stateId, object)
            visitorArchive <- VisitorArchive$new()
            fileConfirmation <- object$accept(visitorArchive)

            # Store receipts
            private$..log[[stateId]] <- list(
              objectConfirmation = objectConfirmation,
              fileConfirmation = fileConfirmation
            )

            # Log State Request
            logEntry <- data.frame(request = "Save",
                                stateId = stateId,
                                class = class(object)[1],
                                name = o$name,
                                desc = o$desc,
                                parentName <- o$parentName,
                                path <- o$path,
                                fileName <- o$fileName,
                                seqNum = seqNum,
                                saved = Sys.time(),
                                restored = character(0))
            private$..log <- rbind(private$..log, logEntry)

            invisible(logEntry)

          },

          restoreState = function(stateId) {

            # Validate
            if (missing(stateId)) {
              v <- Validate0$new()
              v$notify(cls = "StateManager", method = "restoreState",
                       fieldName = "stateId", value = "", level = "Error",
                       msg = paste("StateId parameter is missing with no default.",
                                   "See ?StateManager for further assistance."),
                       expect = TRUE)
              stop()
            }

            # Obtain object and files from state archive
            stateServer <- StateServer$new()
            objectConfirmation <- stateServer$restoreState(stateId)
            visitorRestore <- VisitorRestore$new()
            fileConfirmation <- object$accept(visitorRestore)

            # Store receipts
            private$..log[[stateId]] <- list(
              objectConfirmation = objectConfirmation,
              fileConfirmation = fileConfirmation
            )

            # Log State Request
            logEntry <- data.frame(request = "Restore",
                                   stateId = stateId,
                                   class = class(object)[1],
                                   name = o$name,
                                   desc = o$desc,
                                   parentName <- o$parentName,
                                   path <- o$path,
                                   fileName <- o$fileName,
                                   seqNum = seqNum,
                                   saved = fileConfirmation$saved,
                                   restored = Sys.time())
            private$..log <- rbind(private$..log, logEntry)

            invisible(logEntry)
          }
        )
      )
      super$initialize(...)
    }
  ),lock_objects = FALSE
)#$new()
