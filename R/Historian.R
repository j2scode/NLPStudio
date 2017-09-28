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
#'  \item{\code{addEvent(class, objectName, method, event)}}{Posts the event to history.}
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
          ..events = data.frame(),
          ..historyFile = character()
        ),

        public = list(

          initialize = function() {

            # Get history file name
            c <- Constants$new()
            private$..historyFile <- c$getHistoryFile()

            # Create directories if they don't exist
            paths <- c$getPaths()
            lapply(paths, function(p) {
              if (!dir.exists(p))  dir.create(p, recursive = TRUE)
            })

            # Format and post instantiating event
            private$..events <- data.frame(class = "Historian",
                                method = "initialize",
                                objectName = "historian",
                                event = paste("Instantiated object historian",
                                              "of the Historian class",
                                              "at", Sys.time()),
                                date = Sys.time(),
                                stringsAsFactors = FALSE)

            # Save event to history file
            saveRDS(private$..events, file = private$..historyFile)

            invisible(self)
          },

          addEvent = function(class, objectName, method, event) {

            # Validate
            if (!exists(class)) stop("Invalid class.")
            if (!exists(objectName)) stop("Invalid objectName.")
            if (missing(method)) stop("Method parameter is missing without default.")
            if (missing(event)) stop("Event parameter is missing without default.")

            # Format and post event
            newEvent <- data.frame(class = class,
                                   method = method,
                                   objectName = objectName,
                                   event = event,
                                   date = Sys.time(),
                                   stringsAsFactors = FALSE)
            private$..events <- rbind(private$..events, newEvent)

            # Save event to history file
            save(private$..events, file = private$..historyFile)
          },

          purgeEvents = function() {
            private$..events <- NULL
          },

          restoreEvents = function() {
            private$..events <- readRDS(file = private$..historyFile)
          },

          searchEvents = function(dateFrom = NULL, dateTo = NULL, class = NULL,
                                  objectName = NULL, method = NULL)  {
            tools <- Tools$new()

            if (!is.null(dateFrom)) {
              date <- tools$parseDate(dateFrom, class = "Historian", method = "searchEvents")
              if(date == FALSE) stop()
              events <- subset(private$..events, date >= as.date(date))
            }
            if (!is.null(dateTo)) {
              date <- tools$parseDate(dateTo, class = "Historian", method = "searchEvents")
              if(date == FALSE) stop()
              events <- subset(events, date <= as.date(date))
            }
            if (!is.null(class)) {
              events <- subset(events, class == class)
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
