#==============================================================================#
#                              CorpusBuilderRaw                                #
#==============================================================================#
#' CorpusBuilderRaw
#'
#' \code{CorpusBuilderRaw} Concrete builder class for obtaining and preparing raw corpora
#'
#' \strong{CorpusBuilder Class Overview:}
#'
#' The CorpusBuilder family of classes are an implementation of the builder
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows for different
#' representations of a corpus to be constructed, independent of the
#' components that comprise the corpus. Corpora are constructed to  support
#' various stages of analysis from raw, preprocessed, clean, cross-validation
#' and language modeling.
#'
#' @section CorpusBuilder Class Participants:
#' The participants of the CorpusBuilder
#' class are:
#' \enumerate{
#'  \item CorpusBuilder0: Specifies an abstract interface for creating parts
#'  of the composite Document0 Class object.
#'  \item CorpusDirector: Constructs the objects using the CorpusBuilder0
#'  interface.
#'  \item CorpusBuilderRaw: Constructs a raw corpus originating from a range
#'  of sources including web and directory sources.
#'  \item CorpusBuilderPreprocessor: Constructs a preprocessed corpus
#'  from the raw corpus. This will include basic encoding fixes and tokenization.
#'  \item CorpusBuilderClean: Constructs a clean corpus. Interacts with the
#'  CorpusDesigner0 and CorpusProcessor0 classes to design and render the
#'  clean corpus.
#'  \item CorpusBuilderPilot. Constructs a representative sample of the
#'  clean corpus for exploratory data analysis, modeling and evaluation. This
#'  method interfaces with the CorpusAnalyzer0, CorpusDesigner0, and
#'  CorpusProcessor0 classes to analyze the originating corpus, design
#'  the pilot corpus and process.
#'  \item CorpusBuilderValidationSets. Constructs training, validation
#'  and test sets.  Interacts with the CorpusAnalyzer0, CorpusDesigner0,
#'  and CorpusProcessor0 classes to analyze, design, and render the
#'  training, validation, and test set objects.
#'  }
#'
#' @section CorpusBuilder Generic Interface:
#' The abstract interface in the CorpusBuilder0 class outlines "generic"
#' algorithm common to all concrete builder classes. The core methods are
#' defined as follows:
#' \enumerate{
#'  \item initialize: Instantiates an object of a concrete CorpusBuilder
#'   classes.
#'  \item buildCollection: Constructs an object of the DocumentCollection
#'  class. This is a composite class containing other DocumentCollection
#'  objects or individual objects of the Document class.
#'  \item buildDocument: Constructs an object of the Document class. This
#'  method requires the designation of file names as a required parameter.
#'  \item sourceCorpus: Obtains the originator corpus from sources such
#'  as web sites or directories, and stores it to disk.
#'  \item analyzeCorpus: Analyzes the corpus via the CorpusAnalyzer0
#'  interface, an implementation of the strategy pattern.  Data quality,
#'  word frequency, and semantic analyses are a few of the types of analyses
#'  supported.
#'  \item designCorpus: Interacts with the CorpusDesigner0 interface,
#'  allowing the client to design a corpus based upon features to be
#'  included/excluded, sample sizes, etc...
#'  \item processCorpus: Constructs the corpus using the CorpusProcessor0
#'  interface.
#'  \item evaluateCorpus: Interfaces with the CorpusAnalyzer0 class to
#'  confirm that the corpus conforms to designated design features.
#' }
#'
#' @section CorpusBuilder Collaborations:
#' The sourcing, analysis, design, and processing steps are implemented using
#' various strategy patterns. The following behavioral methods have been
#' devised to select concrete implementations of the strategy classes and are
#' summarized as follows:
#' \itemize{
#'  \item getCorpusSource: Method for selecting the corpus source method.
#'  \item setCorpusSource:  Method for setting the corpus source methods.
#'  \item getAnalyzer: Method for selecting the analysis methods
#'  \item setAnalyzer: Method for setting the analysis methods.
#'  \item getDesigner: Method for selecting the corpus designer method.
#'  \item setDesigner: Method for setting the corpus designer methods.
#'  \item getProcessor: Method for selecting the corpus processor method.
#'  \item setProcessor: Method for setting the corpus processor methods.
#' }
#'
#' @section CorpusBuilderRaw Interface:
#' The concrete implementation of the abstract interface includes both core and behavioral interfaces.
#'
#' \strong{Core interfaces:}
#' \describe{
#'  \item{\code{new(corpusName, corpusDesc = NULL)}}{Instantiates object of class CorpusBuilderRaw. Parameters are:
#'   \item corpusName required
#'   \item corpusDesc optional}
#'  \item{\code{buildCollection()}}{Instantiates object of DocumentCollection class.}
#'  \item{\code{buildCDocument(documentNames, fileNames)}}{Instantiates object of Document class. Both parameters are required.}
#'  \item{\code{sourceCorpus(url, downloadFile)}}{Downloads the corpus from the designated URL and unzips the content into the downloadFile.  The files are stored in the directory designated for this object and data are loaded into the Document objects in memory.}
#'  \item{\code{analyzeCorpus()}}{Abstract method not implemented in this class.}
#'  \item{\code{designCorpus()}}{Abstract method not implemented in this class.}
#'  \item{\code{processCorpus()}}{Abstract method not implemented in this class.}
#'  \item{\code{evaluateCorpus()}}{Abstract method not implemented in this class.}
#'  \item{\code{getCorpus()}}{Returns the DocumentCollection object and a Document object for each file in the DocumentCollection.}
#' }
#'
#' \strong{Behavior Interfaces:}
#' \describe{
#'  \item{\code{corpusSource(value)}}{Method for getting and setting the corpus sources.}
#'  \item{\code{analyzer(value)}}{Not implemented in this class.}
#'  \item{\code{designer(value)}}{Not implemented in this class.}
#'  \item{\code{processor(value)}}{Not implemented in this class.}
#' }
#'
#' @param corpusName String containing the name of the corpus to be built.
#' @param corpusDesc String containing the dscription of the corpus to be built.
#' @param documentNames Character vector listing one or more names of the documents in the corpus.
#' @param documentFileNames Character vector listing one or more names for the files in the coprus source.
#' @param documentDescs Character vector listing one or more file descriptions for the documents in the corpus.
#' @param corpusSource Object of class CorpusSource, indicating the methods for sourcing the corpus.
#' @param analyzer Object of class Analyzer that conducts an analysis on the obtained corpus.
#' @param designer Object of class Designer that designates the features of the corpus.
#' @param processor Object of class Processor that renders the corpus.
#' @param url String containing the URL from which the corpus will be sourced
#' @param zipFile String containing the name of the zipfile to be downloaded
#'
#' @return An object of the DocumentCollection class and one instance of the Document class
#' for each file or register within the corpus.  The objects will contain the corpus
#' content and  meta data. The content will also be stored on disk.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family CorpusBuilder Classes
#' @export
CorpusBuilderRaw <- R6::R6Class(
  classname = "CorpusBuilderRaw",
  inherit = CorpusBuilder0,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..url = character(0),
    ..downloadDir = "download",
    ..rawDir = "raw"
  ),

  active = list(
    corpusSource = function(value) {
      if (missing(value)) {
        private$..corpusSource
      } else {
        private$..corpusSource <- value
      }
    },

    analyzer = function(value) {
      if (missing(value)) {
        private$..analyzer
      } else {
        private$..analyzer <- value
      }
    },

    designer = function(value) {
      if (missing(value)) {
        private$..designer
      } else {
        private$..designer <- value
      }
    },

    processor = function(value) {
      if (missing(value)) {
        private$..processor
      } else {
        private$..processor <- value
      }
    }
  ),

  public = list(
    initialize = function(corpusName, corpusDesc = NULL) {

      # Validate Name
      v <- ValidationManager$new()
      v$validateName(cls = "CorpusBuilderRaw", method = "initialize",
                     name = corpusName, expect = FALSE)

      # Format parameters
      private$..corpusName <- corpusName
      if (is.null(desc)) corpusDesc <- paste(corpusName, "raw corpus")
      private$..corpusDesc <- desc
      private$..corpusPath <- file.path(getLabPath(), corpusName)
      private$..created <- Sys.time()
      private$..modified <- Sys.time()

      assign(corpusName, self, .GlobalEnv)

      nlpStudioCache$setCache(corpusName, self)

      invisible(self)
    },
    buildCollection = function() {
      private$..documentCollection <-
        DocumentCollection$new(private$..corpusName,
                               private$..corpusDesc, private$..corpusPath)
    },

    buildDocuments = function(documentNames, documentFileNames,
                              documentDescs = NULL) {

      # Validate and format parameters
      v <- ValidationManager$new()
      for (d in 1:length(documentNames)) {
        v$validateName(cls = "CorpusBuilderRaw", method = "buildDocuments",
                       name = documentNames[d], expect = FALSE)
      }

      if (!identical(length(documentNames),
                     length(documentFileNames))) {
        v <- Validate0$new()
        v$notify(cls = "CorpusBuilderRaw", method = "buildDocument",
                 fieldName = "documentNames", value = documentNames,
                 level = "Error",
                 msg = "Document names and filename vectors are of different lengths",
                 expect = NULL)
      }

      if (is.null(documentDescs)) {
        documentDescs <- documentNames
      }

      for (d in 1:length(documentNames)) {
        if (is.null(documentDescs[d]) | length(documentDescs[d] == 0)) {
          documentDescs[d] < documentNames[d]
        }
      }

      # Format object variables
      private$..documentNames <- documentNames
      private$..documentFileNames <- documentFileNames
      private$..documentDescs <- documentDescs

      # Create Document objects
      for (d in 1:length(private$..documentNames)) {
        Document$new(name = private$..documentName[d],
                     fileName = private$..documentFileNames[d],
                     path = private$..corpusPath,
                     desc = private$..documentDescs[d])
      }
      # Add Documents to Corpus
      for (d in 1:length(private$..documentNames)) {
        document <- get(private$..documentNames[d],
                        envir = .GlobalEnv, inherits = TRUE)
        private$..documentCollection$addDocument(document)
      }
    },

    obtainCorpus = function(url, zipFile, compressed = TRUE) {

      # Validate Parameters
      v <- ValidateUrl$new()
      v$validate(cls = "CorpusBuilderRaw", method = "obtainCorpus",
                 fieldName = "url", value = url, level = "Error",
                 msg = paste("URL,", url, "is invalid."),
                 expect = TRUE)
      v <- ValidateLogical$new()
      v$validate(cls = "CorpusBuilderRaw", method = "obtainCorpus",
                 fieldName = "compressed", value = compressed, level = "Error",
                 msg = paste("Compressed must be TRUE or FALSE"),
                 expect = TRUE)

      # Format variables
      private$..url <- url
      private$..zipFile <- zipFile
      zipDir <- file.path(private$..corpusPath, private$..downloadDir)
      zipFile <- file.path(zipDir, zipFile)
      rawDir <- file.path(private$..corpusPath, private$..rawDir)

      # Download (and uncompress) corpus data
      if (compressed == TRUE) {

        download.file(url = url, destfile = zipDir, mode = "wb")

        unzip(zipfile = zipFile, overwrite = FALSE,
              exdir = rawDir, junkpaths = TRUE,
              files = private$..documentFileNames)
      } else {
        download.file(url = url, destfile = rawDir, mode = "wb")
      }

      # Upload the corpus into the Document objects.
      document <- private$..documentCollection$


    },
    analyzeCorpus = function() {},
    designCorpus = function() {},
    processCorpus = function() {},
    getCorpus = function() {}
  )
)
