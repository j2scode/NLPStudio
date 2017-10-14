#==============================================================================#
#                                   Curator                                    #
#==============================================================================#
#' Curator
#'
#'
#' \code{Curator} Class responsible for building, validating and executing
#' queries into objects that manage history or states of other objects.
#' Concretely, this class manages queries into Historian and StateManager
#' objects.
#'
#' \strong{Curator Methods:}
#' The Curator methods are as follows:
#'  \itemize{
#'   \item{\code{initialize(target)}}{Initializes a Curator object for the target object.}
#'   \item{\code{buildQuery()}}{Builds the query for the target object.}
#'   \item{\code{submitQuery()}}{Submits the query to the target object.}
#'   \item{\code{getQuery()}}{Returns the query to the calling method.}
#' }
#'
#' \strong{Getter/Setter Methods:}
#' The query can be built, parameter at a time with the following getter/setter methods
#'  \itemize{
#'   \item{\code{className}}{Getter/setter method for the classNsme query parameter.}
#'   \item{\code{objectName}}{Getter/setter method for the objectName query parameter.}
#'   \item{\code{dateFrom}}{Getter/setter method for the dateFrom query parameter.}
#'   \item{\code{dateTo}}{Getter/setter method for the dateTo query parameter.}
#'   \item{\code{lastN}}{Getter/setter method for the lastN query parameter.}
#'   \item{\code{unit}}{Getter/setter method for the unit query parameter.}
#' }
#'
#' @param className Character string containing the class to which the query objects belong.
#' @param objectName Character string containing the name of the object being queried.
#' @param dateFrom ISO or 8601 formatted date indicating the date from which data should be returned.
#' @param dateTo ISO or 8601 formatted date indicating the date to which data should be returned.
#' @param lastN Numeric indicating the last N minutes, hours, days weeks or months for which data should be returned. Values will be rounded to nearest integer.
#' @param unit Character string indicating the unit of time associated with the lastN variable. Valid values include c("min", "hour", "day", "week", "month")
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Curator Classes
#' @export
Curator <- R6::R6Class(
  classname = "Curator",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..target = character(0),
    ..query = list(
      className = character(0),
      objectName = character(0),
      dateFrom = character(0),
      dateTo = character(0),
      lastN = numeric(),
      unit = character(0)
    ),
    ..response = data.frame()
  ),

  active = list(

    className = function(value) {
      if (missing(value)) { return(private$..query$className) }
      else { private$..query$className <- value }
    },

    objectName = function(value) {
      if (missing(value)) { return(private$..query$objectName) }
      else { private$..query$objectName <- value }
    },

    dateFrom = function(value) {
      if (missing(value)) { return(private$..query$dateFrom) }
      else { private$..query$dateFrom <- value }
    },

    dateTo = function(value) {
      if (missing(value)) { return(private$..query$dateTo) }
      else { private$..query$dateTo <- value }
    },

    lastN = function(value) {
      if (missing(value)) {  return(private$..query$lastN) }
      else { private$..query$lastN <- value }
    },

    unit = function(value) {
      if (missing(value)) { return(private$..query$unit) }
      else { private$..query$unit <- value }
    }
  ),

  public = list(

    initialize = function(target) {
      if (!missing(target)) {
        v <- Validator$new()
        if (v$setQueryTarget(self, target) == FALSE) stop()
      } else {
        v <- Validator$new()
        v$notify(class = class(object)[1], method = "initialize",
                  fieldName = "target", value = "",
                  level = "Error",
                  msg = paste0("Query target variable is missing ",
                               "with no default. See ?", class(object)[1],
                               " for further assistance."),
                  expect = classes)
        stop()
      }

      private$..target <- target

      invisible(self)
    },

    buildQuery = function(className = NULL, objectName = NULL, dateFrom = NULL,
                          dateTo = NULL, lastN = NULL, unit = NULL) {
      if (!is.null(className)) private$..query$className <- className
      if (!is.null(objectName)) private$..query$objectName <- objectName
      if (!is.null(dateFrom)) private$..query$dateFrom <- dateFrom
      if (!is.null(dateTo)) private$..query$dateTo <- dateTo
      if (!is.null(lastN)) private$..query$lastN <- lastN
      if (!is.null(unit)) private$..query$unit <- unit
      invisible(self)
    },

    submitQuery = function() {
      v <- Validator$new()
      if (v$submitQuery(self) == FALSE) stop()
      visitor <- VCurator$new(private$..query)
      private$..target$accept(visitor)
    },

    getQuery = function() {
      return(private$..query)
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$curator(self)
    }
  )
)
