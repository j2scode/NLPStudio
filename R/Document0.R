#==============================================================================#
#                                   Document0                                  #
#==============================================================================#
#' Document0
#'
#' \code{Document0} Abstract class for all documents in the NLPStudio
#'
#' This abstract class is the component class within the Document0 composition
#' pattern. To enable common handling of both leaf and composite objects, this
#' object includes the methods common to all objects, plus those methods
#' used to manipulate a composite of objects. The methods, therefore, fall
#' into three categories: (1), core methods implemented by both primative
#' (documents) and composite objects (collections), (2) methods to manage child
#' objects (documents), and (3) behavior strategy setting methods used as part of
#' the strategy pattern to select specific behaviors for the core methods
#' at run time.
#'
#'
#' @section Core Methods:
#' The core methods are shared by both primative and composite objects and are
#' as follows:
#' \describe{
#'  \item{\code{new()}}{Base method for initializing an object of class Document0 or its descendants. Specific behaviors implemented in the Collection subclasses }
#'  \item{\code{getDocument()}}{Base method for retrieving the meta data for a document. Specific behaviors implemented in the Document and Collection subclasses }
#'  \item{\code{readDocument()}}{Base method for reading documents. Specific behaviors implemented in the Document and Collection subclasses }
#'  \item{\code{writeDocument()}}{Base method for writing documents. Specific behaviors implemented in the Document and Collection subclasses }
#'  #'  \item{\code{downloadDocument()}}{Method for downloading the document collection from the web. Specific behaviors implemented in the Document and Collection subclasses}
#'  \item{\code{unzipDocument()}}{Method for unzipping the document collection. Specific behaviors implemented in the Document and Collection subclasses}
#' }
#'
#' @section Composite Methods:
#' The composite methods are used to manage the collection of documents within the composite objects or corpora within
#' other corpora. For instance,the training corpus may be a collection of training corpora of varying sizes. These methods include:
#' \describe{
#'  \item{\code{addDocument(document)}}{Base method for adding documents to a collection. Specific behaviors implemented in the Collection composite class}
#'  \item{\code{removeDocument(document)}}{Base method for removing documents from a collection. Specific behaviors implemented in the Collection composite class}
#'  \item{\code{searchDocuments(name)}}{Base method for searching for documents.  Specific behaviors implemented in the Collection composite class}
#'  \item{\code{listDocuments()}}{Base method for getting a list of documents in a collection. Specific behaviors implemented in the Collection composite class}
#' }
#'
#'
#' @section Behavior Strategy Methods:
#' These methods enable clients to define behaviors for Register and Korpus class object at runtime.
#' \describe{
#'  \item{getReader}{Method for getting the current Read strategy for the object.}
#'  \item{setReader}{Method for setting the current Read strategy for the object.}
#'  \item{getWriter}{Method for getting the current Write strategy for the object.}
#'  \item{setWriter}{Method for setting the current Write strategy for the object.}
#' }
#'
#'
#' @param name Character string indicating the name of the document (required)
#' @param path Character string containing the relative directory path to the document
#' @param documents A list of Document objects contained in the object of the DocumentCollection class
#' @param created A date time variable indicating the date / time the object was created
#' @param modified A date time variable indicating the date / time the object was modified
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
Document0 <- R6::R6Class(
  classname = "Document0",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..name = character(0),
    ..desc = character(0),
    ..path = character(0),
    ..documents = list(),
    ..created = character(0),
    ..modified = character(0),

    ..reader = ReadText$new(),
    ..writer = WriteText$new()
  ),

  active = list(
    reader = function(value) {
      if (missing(value)) {
        private$..reader
      } else {
        private$..reader <- value
      }
    },

    writer = function(value) {
      if (missing(value)) {
        private$..writer
      } else {
        private$..writer <- value
      }
    }
  ),

  public = list(

    # Core Methods
    initialize = function() stop("Method is not available from Document0, an abstract class!"),
    getDocument = function() stop("Method is not available from Document0, an abstract class!"),
    readDocument = function() stop("Method is not available from Document0, an abstract class!"),
    writeDocument = function() stop("Method is not available from Document0, an abstract class!"),
    downloadDocument = function() stop("Method is not available from Document0, an abstract class!"),
    unzipDocument = function() stop("Method is not available from Document0, an abstract class!"),

    # Composite Methods
    addDocument = function(document) stop("Method is not available from Document0, an abstract class!"),
    removeDocument = function(document) stop("Method is not available from Document0, an abstract class!"),
    searchDocuments = function(document) stop("Method is not available from Document0, an abstract class!"),
    listDocuments = function() stop("Method is not available from Document0, an abstract class!"),

    # Behavior Strategy Methods
    getReader = function() stop("Method is not available from Document0, an abstract class!"),
    setReader = function(value) stop("Method is not available from Document0, an abstract class!"),
    getWriter = function() stop("Method is not available from Document0, an abstract class!"),
    setWriter = function(value) stop("Method is not available from Document0, an abstract class!")
  )
)
