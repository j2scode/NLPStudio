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
#' a client object, statess the request, dispatches the appropriate StateManager
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
#'  methods, statess the request and dispatches requests to the StateManager, and,
#'  if the client requests that files be saved or restored, the StateArchiver
#'  class.
#'  \item StateManager: This class is responsible for saving and restoring
#'  objects from and to existing objects.
#'  \item StateArchiver: This class is responsible for archiving and
#'  restoring any files associated with an object being saved or restored.
#'  }
#'
#' @section Methods:
#' The methods for the StateManager class are as follows:
#' \describe{
#'  \item{\code{getState(stateId)}}{Returns the object at the state designated by the stateId parameter.}
#'  \item{\code{restoreState(stateId, files = FALSE)}}{Restores the object at the state designated by the stateId parameter to the global environment. If files is set TRUE, previously archived directories and files are restored. If the object is part of a composite, e.g. Document or DocumentCollection class object, and its parent does not exist, the files will be restored to an orphan object according to the class of the object, the files will be restored to the orphan object directory, and the parent will be changed accordingly. In this case the restored object may then be moved to another parent as required.}
#'  \item{\code{saveState(object, files = FALSE)}}{Saves the current state of an object and dispatches a StateManager object to save the state to disk. If the files parameter is set to true, a VisitorArchive object will be instantiated to compress and save the  files associated with the object to an archive directory.}
#' }
#'
#' @param object The object to be loaded or saved. Required parameter for the saveState method.
#' @param stateId The unique identifier for the object and state. Required parameter for the restoreState method.
#' @param files statesical indicating whether the files associated with an object should be archived.
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
          ..requests = list(),
          ..states = list(),
          ..query = list(
            objectName = character(),
            dateFrom = character(),
            dateTo = character()
          ),
          ..stateClasses = character(0),
          ..statesFile = character(0),
          ..created = character(0),
          ..modified = character(0),

          getConstants = function() {
            constants <- Constants$new()
            private$..statesFile <- constants$getStatesFile()
            private$..stateClasses <- constants$getStateClasses()
          },

          validateObject = function(method, object) {

            v <- ValidateClass$new()
            if (v$validate(cls = "StateManager", method = method,
                           fieldName = "class(object)", value = object, level = "Error",
                           msg = paste("Object is not a serializable object",
                                       "See ?StateManager for further assistance."),
                           expect = private$..stateClasses) == FALSE) {
              return(FALSE)
            }
          },

          validateState = function(method, stateId) {

            if (!exists(private$..states[[stateId]])) {
              v <- Validate0$new()
              v$notify(cls = "StateManager", method = method,
                             fieldName = "stateId", value = stateId, level = "Error",
                             msg = paste("State does not exist.",
                                         "See ?StateManager for further assistance."),
                             expect = TRUE)
              return(FALSE)
            }
          },

          assignStateId = function(name) {
            seqNum <- 1
            states <- as.data.frame(private$..states)

            if (nrow(states) > 0 ) {
              if (nrow(subset(states,
                              as.Date(created) == as.Date(Sys.time()) &
                              objectName == name)) > 0) {
                seqNum <- states %>%
                  filter(as.Date(created) == as.Date(Sys.time()) &
                           objectName == name) %>%
                  summarise(max(seqNum))
                seqNum <- seqNum + 1
              }
            }

            stateId <- paste0(name,"-", as.Date(Sys.time()), "-", seqNum)
            return(stateId)
          },

          parseDate <- function(method, date) {
            v <- ValidateDate$new()
            date <- v$validate(cls = "StateManager", method = method,
                               fieldName = "date", value = date, level = "Error",
                               msg = paste("Date parameter not recognized. The",
                                           "recommended date format is 'YYYY-MM-DD'.",
                                           "See ?StateManager for further assistance."),
                               expect = TRUE)
            return(date)
          }
        ),

        public = list(

          initialize = function() {
            private$..created <- Sys.time()
            private$..modified <- Sys.time()
            invisible(self)
          },

          getInstance = function() invisible(self),

          #-------------------------------------------------------------------#
          #                        State Query Methods                        #
          #-------------------------------------------------------------------#
          querySetObjectName = function(name) {
            private$..query$object <- name
          },

          querySetDateFrom = function(date) {

            date <- private$parseDate(method = "querySetDateFrom", date)
            if(date == FALSE) stop()
            private$..query$dateFrom <- date

          },

          querySetDateTo = function(date) {

            date <- private$parseDate(method = "querySetDateTo", date)
            if (date == FALSE) stop()
            private$..query$dateTo <- date

          },

          queryStates = function() {

            states <- data.frame()

            if (nrow(private$..states) > 0) {

              # Filter by object name
              if (length(private$..query$objectName) > 0) {
                states <- subset(private$..states,
                                 objectName == private$..query$objectName)
              } else {
                states <- private$..states
              }

              # Filter by from date
              if (length(private$..query$dateFrom) > 0) {
                states <- subset(states,
                                 created >= private$..query$dateFrom)
              }

              # Filter by to date
              if (length(private$..query$dateTo) > 0) {
                states <- subset(states,
                                 created <= private$..query$dateTo)
              }
            }
            return(states)
          },

          #-------------------------------------------------------------------#
          #                        saveState Method                           #
          #-------------------------------------------------------------------#
          saveState = function(object = NULL) {

            # Get constants and validate request
            private$getConstants()
            if (private$validateObject(method = "saveState", object) == FALSE) stop()

            # Obtain object information and format stateId
            o <- object$getObject()
            stateId <- assignStateId(o$name)

            # Generate key
            t <- Tools$new()
            key <- t$makeRandomString()

            # Create state object and dispatch save request
            state <- State$new(key)
            path <- state$saveState(key = key, stateId = stateId, object = object)

            #  Create log entry for new saved state
            state <- list(
              stateId = stateId,
              objectName = o$name,
              objectDesc = o$desc,
              path = path,
              created = Sys.time())

            # Save state to memory list and to the serialized list on file
            private$..states[[stateId]] <- state
            statesFile <- readRDS(private$..statesFile)
            statesFile[[stateId]] <- state
            saveRDS(statesFile, file = private$..statesFile)

            invisible(private$..states[[stateId]])

          },

          restoreState = function(stateId) {

            # Get constants and validate request
            private$getConstants()
            if (private$validateState(method = "restoreState", stateId) == FALSE) stop()

            # Generate key
            t <- Tools$new()
            key <- t$makeRandomString()

            # Create state object and dispatch restore request
            state <- State$new(key)
            object <- state$restoreState(key = key, stateId = stateId)

            return(object)
          }
        )
      )
      super$initialize(...)
    }
  ),lock_objects = FALSE
)#$new()
