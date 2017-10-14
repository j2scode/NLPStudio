#==============================================================================#
#                                 Constants                                    #
#==============================================================================#
#' Constants
#'
#' \code{Constants} Class containing constants used throughout the package such
#' as directories structures.
#'
#' @section Constants Methods:
#' \describe{
#'  \item{\code{getPaths()}}{Returns the NLPStudio directory and file paths as a list.}
#' }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Constants <- R6::R6Class(
  classname = "Constants",
  lock_objects = TRUE,
  lock_class = TRUE,

  private = list(
    ..constants = list(
      stateClasses = c("File", "Document", "DocumentCollection", "Lab"),
      autoSave = TRUE
    ),
    ..paths = list(
      studio = "./NLPStudio",
      files = "./NLPStudio/Files",
      log = "./NLPStudio/Log",
      states = "./NLPStudio/States"
      ),
    ..files = list(
      policyFile = "./NLPStudio/Policy.Rdata",
      historyFile = "./NLPStudio/Log/History.Rdata"
    ),
    ..query = list(
      units = c("min", "hour", "day", "week", "month")
    )
  ),

  public = list(

    # Constants
    getStateClasses = function() private$..constants$stateClasses,
    getAutoSave = function() private$..constants$autoSave,

    # Paths
    getPaths = function() private$..paths,
    getStudioPath = function() private$..paths$studio,
    getFilesPath = function() private$..paths$files,
    getStatesPath = function() private$..paths$states,
    getLogPath = function() private$..paths$log,

    # Files
    getHistoryFile = function() private$..files$historyFile,
    getPolicyFile = function() private$..files$policyFile,

    # Query Constants
    getQueryVars = function() private$..query
  )
)
