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
      states = "./NLPStudio/States",
      labs = "./NLPStudio/Labs",
      log = "./NLPStudio/Log"
      ),
    ..files = list(
      statesFile = "./NLPStudio/States/.States.Rdata",
      historyFile = "./NLPStudio/States/.History.Rdata"
    )
  ),

  public = list(

    # Constants
    getStateClasses = function() private$..constants$stateClasses,
    getAutoSave = function() private$..constants$autoSave,

    # Paths
    getPaths = function() private$..paths,
    getStudioPath = function() private$..paths$studio,
    getStatesPath = function() private$..paths$states,
    getLabsPath = function() private$..paths$labs,
    getLogPath = function() private$..paths$log,

    # Files
    getStatesFile = function() private$..files$statesFile,
    getHistoryFile = function() private$..files$historyFile
  )
)
