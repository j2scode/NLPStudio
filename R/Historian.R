#==============================================================================#
#                                 Historian                                    #
#==============================================================================#
#' Historian
#'
#' \code{Historian} Class that maintains the log history of objectNames and their
#' events.
#'
#' @section Historian Methods:
#' \describe{
#'  \item{\code{new()}}{Initializes a singleton objectName, noting the instantiation of the nlpStudio objectName.}
#'  \item{\code{addEvent(cls, objectName, method, event)}}{Posts the event to history.}
#'  \item{\code{searchEvents(dateFrom, dateTo, class, objectName, method)}}{Returns the history of events according to the parameters provided,  in a data frame format.}
#'  \item{\code{purgeEvents()}}{Sets events to null.}
#'  \item{\code{restoreEvents()}}{Restores events from the history file.}
#' }
#'
#' @param dateFrom Character string containing a date in any ISO 8601 format, from which the history log should be returned.
#' @param dateTo Character string containing a date in any ISO 8601 format, from which the history log should be returned.
#' @param class Character string indicating the class for which log entries should be returned.
#' @param objectName Character string indicating the name of the objectName for which log entries should be returned.
#' @param Method Character string indicating the method for which log entries should be returned.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Historian <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "Historian",
        private = list(
          ..name = character(0),
          ..events = data.frame(),
          ..historyFile = character(0),
          ..created = character(0),
          ..modified = character(0)
        ),

        public = list(

          initialize = function() {

            # Get file and directory information
            c <- Constants$new()
            file  <- c$getHistoryFile()
            paths <- c$getPaths()

            # Instantiate Object
            private$..name <- "historian"
            private$..historyFile <- file
            private$..created <- Sys.time()
            private$..modified <- Sys.time()

            # Load history from file if exists, otherwise, created directory
            if (file.exists(private$..historyFile)) {
              private$..events <- readRDS(file = private$..historyFile)
            } else {
              lapply(paths, function(p) {
                if (!dir.exists(p))  dir.create(p, recursive = TRUE)
              })
            }

            # Create instantiation event
            hello <- data.frame(class = "Historian",
                                method = "initialize",
                                objectName = private$..name,
                                event = paste("Instantiated object historian",
                                              "of the Historian class",
                                              "at", format(Sys.time())),
                                date = Sys.time(),
                                stringsAsFactors = FALSE)

            # Create/Update event log as appropriate
            private$..events <- rbind(private$..events, hello)

            # Save event to history file
            saveRDS(private$..events, file = private$..historyFile)

            # Assign its name in the global environment
            assign(private$..name, self, envir = .GlobalEnv)

            invisible(self)
          },

          getInstance = function() {
            invisible(self)
          },

          addEvent = function(cls, objectName, method, event) {

            # Validate
            if (!exists(cls)) stop("Invalid class.")
            if (!exists(objectName)) stop("Invalid objectName.")
            if (missing(method)) stop("Method parameter is missing without default.")
            if (missing(event)) stop("Event parameter is missing without default.")

            # Format and post event
            newEvent <- data.frame(class = cls,
                                   method = method,
                                   objectName = objectName,
                                   event = event,
                                   date = Sys.time(),
                                   stringsAsFactors = FALSE)
            private$..events <- rbind(private$..events, newEvent)
            private$..modified <- Sys.time()

            # Save event to history file
            saveRDS(private$..events, file = private$..historyFile)
          },

          purgeEvents = function() {
            private$..events <- NULL
            private$..modified <- Sys.time()
          },

          restoreEvents = function() {
            private$..events <- readRDS(file = private$..historyFile)
            private$..modified <- Sys.time()
          },

          searchEvents = function(dateFrom = NULL, dateTo = NULL, cls = NULL,
                                  objectName = NULL, method = NULL)  {
            tools <- Tools$new()
            events <- private$..events

            if (!is.null(dateFrom)) {
              dateFrom <- tools$parseDate(dateFrom, class = "Historian", method = "searchEvents")
              if(dateFrom == FALSE) stop()
              events <- subset(events, as.Date(date) >= as.Date(dateFrom))
            }
            if (!is.null(dateTo)) {
              dateTo <- tools$parseDate(dateTo, class = "Historian", method = "searchEvents")
              if(dateTo == FALSE) stop()
              events <- subset(events, as.Date(date) <= as.Date(dateTo))
            }
            if (!is.null(cls)) {
              events <- subset(events, class == cls)
            }
            if (!is.null(objectName)) {
              events <- subset(events, objectName == objectName)
            }
            if (!is.null(method)) {
              events <- subset(events, method == method)
            }

            return(events)
          }
        )
      )
      super$initialize(...)
    }
  ),lock_object = FALSE
)#$new()
