#==============================================================================#
#                              CorpusBuilder0                                  #
#==============================================================================#
#' CorpusBuilder0
#'
#' \code{CorpusBuilder} Abstract builder class of the CorpusBuilder set of classes
#'
#' This abstract builder class defines the interface for the CorpusBuilder
#' subclasses.  The class interacts with director and the concrete builder
#' classes to build the document corpora required for the analysis, processing,
#' and modeling phases. The standard interface methods obtain and process the
#' corpus, create the composite document and return the corpus meta data and
#' content to the calling environment.
#'
#' @section Methods:
#' The following methods are defined in this class.
#' \describe{
#'  \item{\code{new(name, desc)}}{Abstract method for instantiating object of CorpusBuilder0 class.}
#'  \item{\code{buildComposite()}}{Abstract method for building the composite document.}
#'  \item{\code{buildComponent()}}{Abstract method for building the component document.}
#'  \item{\code{obtainCorpus()}}{Abstract method for obtaining the corpus. }
#'  \item{\code{processCorpus()}}{Abstract method for processing the corpus. }
#'  \item{\code{getCorpus()}}{Abstract method for returning the constructed corpus to the director class. }
#' }
#'
#' @param name String containing the name of the corpus to be built.
#' @param desc String containing the dscription of the corpus to be built.
#' @param path String indicating the directory path in which the corpus resides
#' @param created Datetime object indicating create date/time
#' @param modified Datetime object indicating modification date/time
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder Classes
#' @export
CorpusBuilder0 <- R6::R6Class(
  classname = "CorpusBuilder0",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..name = character(0),
    ..desc = character(0),
    ..path = character(0),
    ..created = character(0),
    ..modified = character(0)

  ),

  public = list(
    buildComposite = function() stop("Method is not available from CorpusBuilder0, an abstract class!"),
    buildComponent = function() stop("Method is not available from CorpusBuilder0, an abstract class!"),
    obtainCorpus = function() stop("Method is not available from CorpusBuilder0, an abstract class!"),
    processCorpus = function() stop("Method is not available from CorpusBuilder0, an abstract class!"),
    getCorpus = function() stop("Method is not available from CorpusBuilder0, an abstract class!")
  )
)
