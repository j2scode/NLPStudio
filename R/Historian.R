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
#'  \item{\code{getEvents(dateFrom, dateTo, class, objectName, method)}}{Returns the history of events according to the parameters provided,  in a data frame format.}
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
          ..events = list(),
          ..historyFile = character(),

          searchEvents = function(dateFrom = NULL, dateTo = NULL, class = NULL,
                               objectName = NULL, method = NULL)  {

            events <- do.call(rbind.data.frame, private$..events)
            rownames(events) <- NULL

            tools <- Tools$new()

            if (!is.null(dateFrom)) {
              date <- tools$parseDate(dateFrom, class = "Historian", method = "searchEvents")
              if(date == FALSE) stop()
              events <- subset(events, date >= as.date(date))
            }

            if (!is.null(dateTo)) {
              date <- tools$parseDate(dateTo, class = "Historian", method = "searchEvents")
              if(date == FALSE) stop()
              events <- subset(events, date <= as.date(date))
            }

            if (!is.null(class))  events <- subset(events, class == class)
            if (!is.null(objectName)) events <- subset(events, objectName == objectName)
            if (!is.null(method))  events <- subset(events, method == method)

            return(events)
          }
        ),

        public = list(

          initialize = function() {

            # Initialize constants
            c <- Constants$new()
            private$..historyFile <- c$getHistoryFile()

            # Format and post event
            date <- Sys.time()
            eventId <- paste0("nlpStudio ",date)
            event = list(date = date,
                         class = "NLPStudio",
                         objectName = "nlpStudio",
                         method = "initializes",
                         event = "NLPStudio objectName initialized.")
            private$..events[[eventId]] <- event

            # Save event to history file
            saveRDS(self, file = private$..historyFile)

            invisible(self)
          },

          addEvent = function(class, objectName, method, event) {

            # Validate
            if (!exists(class)) stop("Invalid class")
            if (!exists(objectName)) stop("Invalid objectName")

            # Format and post event
            date <- Sys.time()
            eventId <- paste0(objectName, " ", date)
            event = list(date = date,
                         class = class,
                         objectName = objectName,
                         method = method,
                         event = event)
            private$..events[[eventId]] <- event

            # Save event to history file
            if (file.exists(private$..historyFile)) {
              events <- readRDS(file = private$..historyFile)
            } else {
              events = list()
            }
            events[[eventId]] <- event
            saveRDS(events, file = private$..historyFile)
          },

          getEvents = function(...) {
            return(private$..searchEvents(...))
          }
        )
      )
      super$initialize(...)
    }
  ),lock_object = FALSE
)#$new()
