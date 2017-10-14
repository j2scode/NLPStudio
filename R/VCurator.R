#==============================================================================#
#                                   VCurator                                   #
#==============================================================================#
#' VCurator
#'
#'
#' \code{VCurator} Visitor class responsible for processing queries on
#' StateManager and Historian class objects.
#'
#' \strong{VCurator Methods:}
#' The VCurator methods are as follows:
#'  \itemize{
#'   \item{\code{stateManager(object)}}{Method processing queries into StateManager objects.}
#'   \item{\code{historian(object)}}{Method processing queries into Historian objects.}
#' }
#'
#' @param className Character string containing the name of the class about which the query is rendered.
#' @param objectName Character string containing the name of the object about which the query is rendered.
#' @param dateFrom An ISO 8601 formatted date from which (inclusive) the data should be returned.
#' @param dateTo An ISO 8601 formatted date to which (inclusive) the data should be returned.
#' @param lastN A numeric indicating the numeric component of a date or time interval
#' @param unit Character string containing the name of the unit associated with teh lastN parameter.  Valid values are "min", "hour", "day", "week", "month"
#' @param child  The child object
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VCurator <- R6::R6Class(
  classname = "VCurator",
  lock_objects = FALSE,
  lock_class = FALSE,
  private = list(
    ..query = list(
      className = character(0),
      objectName = character(0),
      dateFrom = character(0),
      dateTo = character(0),
      lastN = numeric(),
      unit = character(0)
      )
  ),

  public = list(

    initialize = function(query) {
      private$..query <- query
    },

    stateManager = function(object) {

    },

    historian = function(object) {

    }
  )
)
