#==============================================================================#
#                              CorpusDirector                                  #
#==============================================================================#
#' CorpusDirector
#'
#' \code{CorpusDirector} Director class of the CorpusBuilder set of classes
#'
#' This director class is part of the CorpusBuilder set of classes, an
#' implementation of the builder design pattern. The class interacts with
#' the builder interfaces to build a document corpus for use throughout
#' the data acquisition, cleaning, analysis, and modeling phases. The
#' standard interface methods obtain and process the corpus,
#' create the composite document and return the corpus meta data and
#' content to the calling environment.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder Classes
#' @export
CorpusDirector <- R6::R6Class(
  classname = "CorpusDirector",
  lock_objects = FALSE,
  lock_class = FALSE,

  public = list(
    buildCorpus = function(builder) {
      builder$obtainCorpus()
      builder$processCorpus()
      builder$createComposite()
      builder$getCorpus()
    }
  )
)
