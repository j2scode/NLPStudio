#==============================================================================#
#                                   Document0                                  #
#==============================================================================#
#' Document0
#'
#' \code{Document0} Abstract class for composite and individual documents in the NLPStudio
#'
#' \strong{Document0 Class Overview:}
#'
#' The Document0 family of classes is an implementation of the composite
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows composite
#' and individual objects to be treated uniformly.
#'
#' The following sections include:
#' \itemize{
#'  \item Class Participants: Classes the comprise this composite pattern.
#'  \item Class Collaborators: Classes which interact with the Document0 class.
#'  \item Core Methods: Methods implemented by both individual and document
#'  collections.
#'  \item Composite Methods: Methods for managing child parent relationships.
#'  \item Input / Output Methods: Methods for reading and writing documents.
#' }
#'
#' \strong{Document0 Class Participants:}
#' The participants of the Document0 class are:
#' \itemize{
#'  \item Document0: This component class specifies an abstract interface
#'  for all individual and composite document classes.
#'  \item Document: This "leaf" class specifies the  interface forindividual
#'  document objects.
#'  \item DocumentCollection: The composite class specifies the interface
#'  for the composite document objects.
#'  }
#'
#' \strong{Document0 Class Collaborators:}
#' The collaborators of the Document0 class are:
#' \itemize{
#'  \item Lab: This class instantiates labs within NLPStudio which have the
#'  responsibility of managing document objects.
#'  \item CorpusBuilder0: A family of classes of the builder pattern,
#'  responsibile for building varying representations of document collections.
#'  }
#'
#' \strong{Document0 Core Methods:}
#' The core methods are shared by both primative and composite objects and are
#' as follows:
#' \itemize{
#'  \item{\code{new(name, fileName, desc)}}{Base method for instantiating
#'  an object of the Document and  DocumentCollection classes. File name is
#'  only only required of objects of the Document class.
#'  Specific behaviors implemented in the Collection subclasses. }
#'  \item{\code{getDocument()}}{Base method for retrieving an object of the
#'  "Document" or "DocumentCollection" class. Specific behaviors
#'  implemented in the Document and Collection subclasses.}
#'  \item{\code{accept(visitor)}}{Base method for accepting a visitor object. Specific behaviors
#'  implemented in the Document and Collection subclasses, }
#'  \item{\code{addContent(content)}}{Base method for adding content to an object of the Document class. Specific behaviors
#'  implemented in the Document and Collection subclasses, }
#'  \item{\code{removeContent()}}{Base method for removing content from an object of the Document class. Specific behaviors
#'  implemented in the Document and Collection subclasses, }#'
#' }
#'
#' \strong{Document0 Composite Methods:}
#' The composite methods are used to manage parent / child relationshipos among
#' the documents, and are:
#' \itemize{
#'  \item{\code{addDocument(document)}}{Base method for adding documents to a
#'  collection. Specific behaviors implemented in the Collection composite class}
#'  \item{\code{getDocuments()}}{Base method for retrieving the meta data for
#'  a collection of child objects of the Document class. Specific behaviors
#'  implemented in the Document and Collection subclasses }
#'  \item{\code{removeDocument(document)}}{Base method for removing documents from
#'  a collection. Specific behaviors implemented in the Collection composite class}
#'  \item{\code{getAncestor()}}{Base method for retrieving the parent object for a document.
#'  Specific behaviors implemented in the Collection composite class}
#'  \item{\code{setAncestor(parent)}}{Base method for adding the parent object to a document.
#'  Specific behaviors implemented in the Collection composite class}
#' }
#'
#'
#' @param content A character vector or list containing text content for objects. Required for write methods.
#' @param desc Character string containing the description of the document.
#' @param document An object of the Document class.  Maintained as members of objects of the DocumentClass composite class.
#' @param fileName Character string cointaining the file name for a document.  Required for objects of Document class.
#' @param name Character string indicating the name of the document. Required for all objects
#' @param parent An object of the Lab or DocumentCollection class that represents the parent object. Required for input / output methods.
#' @param purge Logical indicating whether the removeDocument method should purge the document from the current environment. The default is FALSE
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
    ..parent = character(0),
    ..parentName = character(0),
    ..path = character(0),
    ..fileName = character(0),
    ..documents = list(),
    ..content = character(0),
    ..created = character(0),
    ..modified = character(0)
  ),

  public = list(

    # Core Methods
    initialize = function() stop("Method is not available from Document0, an abstract class!"),
    getDocument = function() stop("Method is not available from Document0, an abstract class!"),
    accept = function(visitor) stop("Method is not available from Document0, an abstract class!"),

    # Composite Methods
    getDocuments = function() stop("Method is not available from Document0, an abstract class!"),
    addDocument = function(document) stop("Method is not available from Document0, an abstract class!"),
    removeDocument = function(document, purge = FALSE) stop("Method is not available from Document0, an abstract class!"),
    getAncestor = function() stop("Method is not available from Document0, an abstract class!"),
    setAncestor = function(parent) stop("Method is not available from Document0, an abstract class!"),

    # IO Methods
    readDocument = function(format) stop("Method is not available from Document0, an abstract class!"),
    writeDocument = function(format, content) stop("Method is not available from Document0, an abstract class!")
  )
)
