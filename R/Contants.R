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
  classname = "Contants",
  lock_objects = TRUE,
  lock_class = TRUE,

  private = list(
    ..paths = list(
      studio = "./NLPStudio",
      states = "./NLPStudio/States",
      statesFile = "./NLPStudio/States/.States.Rdata",
      labs = "./NLPStudio/Labs",
      log = "./NLPStudio/Log"
      ),
    ..stateClasses = c("File", "Document", "DocumentCollection", "Lab")
  ),

  public = list(

    getStudioPath = function() private$..paths$studio,
    getStatesPath = function() private$..paths$states,
    getStatesFile = function() private$..paths$statesFile,
    getLabsPath = function() private$..paths$labs,
    getLogPath = function() private$..paths$log,

    getStateClasses = function() private$..stateClasses
  )
)
