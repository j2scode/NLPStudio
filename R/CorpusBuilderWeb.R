#==============================================================================#
#                              CorpusBuilderWeb                                #
#==============================================================================#
#' CorpusBuilderWeb
#'
#' \code{CorpusBuilder} Builds a corpus obtained from the web.
#'
#' This concrete builder class obtains the corpus, creates the component
#' document and returns the object to the calling environment. This will
#' download a single compressed file from the web source.
#'
#' @section Methods:
#' The following methods are defined in this class.
#' \describe{
#'  \item{\code{new(name, desc, url)}}{Method for instantiating the builder}
#'  \item{\code{buildComposite()}}{Method for building the composite document. }
#'  \item{\code{buildComponent()}}{Method for building the component document.}
#'  \item{\code{obtainCorpus()}}{Method for obtaining the corpus. The corpus is downloaded from its web source }
#'  \item{\code{processCorpus()}}{Method for processing the corpus. This returns true as there is no processing.}
#'  \item{\code{getCorpus()}}{Method for returning the constructed composite document to the director class. }
#'  }
#'
#' @param fileNames String indicating the names of the files in the compressed document to obtain
#' @param url String containing the URL for the web source
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder Classes
#' @export
CorpusBuilderWeb <- R6::R6Class(
  classname = "CorpusBuilderWeb",
  inherit = CorpusBuilder0,
  lock_objects = FALSE,
  lock_class = FALSE,
  private = list(
    ..url = character(0),
    ..downloadFile = character(0)
  ),

  active = list(

    desc = function(value) {
      if (missing(value)) {
        private$..desc
      } else {
        private$..desc <- value
      }
    }
  ),

  public = list(

    initialize = function(name, url, fileNames) {

      # Validate name
      v <- ValidationManager$new()
      v$validateName(value)

      # Validate URL
      v <- ValidateUrl$new()
      v$validate(cls = "CorpusBuilderWeb",
                 method = "new", fieldName = "url",
                 value = url, level = "Error",
                 msg = "URL is invalid.", expect = TRUE)

      # Validate fileNames
      v <- ValidateString$new()
      v$validate(cls = "CorpusBuilderWeb",
                 method = "new", fieldName = "fileNames",
                 value = fileNames, level = "Error",
                 msg = "File names are invalid.", expect = TRUE)

      # Get current lab for path parameter
      lab <- nlpStudio$currentLab
      lab <- lab$getLab()

      # Save parameters
      private$..name <- name
      if (missing(desc)) desc <- paste(name, "Corpus")
      private$..desc <- desc
      private$..path <- file.path(lab$name, name)
      private$..url <- url
      private$..filesNames <- fileNames
      private$..downloadFile <- tempfile()
      private$..created <- Sys.time()
      private$..modified <- Sys.time()

      invisible(self)
    },

    buildComposite = function() {

      c <- DocumentCollection$new(name = private$..name,
                                  desc = private$..desc,
                                  path = private$..path)

      for (i in 1:length(fileNames)) {
        d <- Document$new(name = fileNames[d],
                          desc = paste(private$..desc, " ", fileNames[d]),
                          path = file.path(private$..path, fileNames[d]))
        c$addDocument(d)
      }
      return(c)
    },

    obtainCorpus = function() {

      # Download data
      download.file(private$..url, destfile = private$..downloadFile, mode = 'wb')

      # Unzip data
      unzip(zipfile = private$..downloadFile, overwrite = FALSE,
            exdir = private$..path,
            junkpaths = TRUE, files = private$..fileNames)
      unlink(private$..downloadFile)

      # Read Corpus
      corpus <- get(private$..name, envir = .GlobalEnv)
      corpus$setReader <- ReadBin$new()
      return(corpus$readData())

    },

    processCorpus = function() {
      return(TRUE)
    },

    getCorpus = function() {
      return(get(private$..name, envir = .GlobalEnv))
    }
  )
)
