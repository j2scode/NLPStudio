#==============================================================================#
#                              CorpusBuilderRawWeb                             #
#==============================================================================#
#' CorpusBuilderRawWeb
#'
#' \code{CorpusBuilder} Builds corpus sourced from the web.
#'
#' This concrete builder class obtains the corpus, creates the document
#' composite and returns the corpus content and meta data to the calling
#' environment.
#'
#' @section Methods:
#' The following methods are included in this class.
#' \describe{
#'  \item{\code{obtainCorpus()}}{Downloads the corpus from its internet web source.}
#'  \item{\code{processCorpus()}}{Unzips the corpus.}
#'  \item{\code{createComposite()}}{Creates the document composite and leaf objects for the corpus.}
#'  \item{\code{getCorpus()}}{Returns an object of the Document class, containing the corpus content and meta data.}
#' }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder Classes
#' @export
CorpusBuilderRawWeb <- R6::R6Class(
  classname = "CorpusBuilderRawWeb",
  inherit = CorpusBuilder0,
  lock_objects = FALSE,
  lock_class = FALSE,
  private = list(
    ..name = character(0),
    ..desc = character(0),
    ..url = character(0),
    ..fileNames = character(0),
    ..downloadFile = character(0),
    ..downloadDir = character(0),
    ..created = character(0),
    ..modified = character(0)
  ),

  active = list(
    name = function(value) {
      if (missing(value)) {
        private$..name
      } else {
        v <- ValidationManager$new()
        v$validateName(cls = "CorpusBuilderRawWeb",
                       method = "name", value, expect = FALSE)
        private$..name <- value
      }
      rm(v)
    },

    desc = function(value) {
      if (missing(value)) {
        private$..desc
      } else {
        private$..desc <- value
      }
    },

    url = function(value) {
      if (missing(value)) {
        private$..url
      } else {
        v <- ValidateUrl$new()
        v$validate(cls = "CorpusBuilderRawWeb",
                   method = "url", fieldName = "url",
                   value = value, level = "Error",
                   msg = "URL is invalid.", expect = TRUE)
        private$..url <- value
      }
      rm(v)
    },

    fileNames = function(value) {
      if (missing(value)) {
        private$..name
      } else {
        v <- ValidateString$new()
        v$validate(cls = "CorpusBuilderRawWeb",
                   method = "fileNames", fieldName = "fileNames",
                   value = value, level = "Error",
                   msg = "File names are invalid.", expect = TRUE)
        private$..name <- value
      }
    }
  ),

  public = list(

    obtainCorpus = function(name, desc, url, fileNames) {

      # Validate name
      v <- ValidationManager$new()
      v$validateName(value)
      rm(v)

      # Validate URL
      v <- ValidateUrl$new()
      v$validate(cls = "CorpusBuilderRawWeb",
                 method = "obtainCorpus", fieldName = "url",
                 value = url, level = "Error",
                 msg = "URL is invalid.", expect = TRUE)

      # Save parameters
      private$..name <- name
      if (missing(desc)) desc <- paste(name, "Corpus")
      private$..desc <- desc
      private$..url <- url
      private$..filesNames <- fileNames
      private$..downloadFile <- tempfile()
      private$..downloadDir <- tempdir()
      private$..created <- Sys.time()
      private$..modified <- Sys.time()

      # Download data
      download.file(url, destfile = private$..downloadFile, mode = 'wb')
    },

    processCorpus = function() {

      # Unzip data
      unzip(zipfile = private$..downloadFile, overwrite = FALSE,
            exdir = private$..downloadDir,
            junkpaths = TRUE, files = private$..fileNames)
      unlink(private$..downloadFile)

    },

    createComposite = function() {

      # Create collection object
      c <- DocumentCollection$new(name = private$..name, desc = private$..desc)

      for (i in 1:length(private$..fileNames)) {
        d <- Document$new(name = private$..name, desc = private$..desc, fileName = private$..fileNames[i])
        c$addDocument(d)
      }
    }
  )
)
