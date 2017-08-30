#==============================================================================#
#                                   Document0                                  #
#==============================================================================#
#' Document0
#'
#' \code{Document0} Abstract class for all documents in the NLPStudio
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
#'
#' @section Document0 Class Participants:
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
#' @section Document0 Class Collaborators:
#' The collaborators of the Document0 class are:
#' \itemize{
#'  \item Lab: This class instantiates labs within NLPStudio which have the
#'  responsibility of managing document objects.
#'  \item CorpusBuilder0: A family of classes of the builder pattern,
#'  responsibile for building varying representations of document collections.
#'  }
#'
#' @section Document0 Core Methods:
#' The core methods are shared by both primative and composite objects and are
#' as follows:
#' \itemize{
#'  \item{\code{new(name, path, fileName, desc)}}{Base method for instantiating
#'  an object of the Document and  DocumentCollection classes. File name is
#'  only only required of objects of the Document class.
#'  Specific behaviors implemented in the Collection subclasses }
#'  \item{\code{getDocument()}}{Base method for retrieving the meta data for
#'  a document. Objects of the DocumentCollection class also return a
#'  list of contained objects of the Document class. Specific behaviors
#'  implemented in the Document and Collection subclasses }
#'  \item{\code{printDocument()}}{Base method for printing the meta data for a
#'  document to console. Objects of the DocumentCollection class also
#'  print a list of contained objects of the Document class. Specific behaviors
#'  implemented in the Document and Collection subclasses }
#' }
#'
#' @section Document0 Composite Methods:
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
#'  \item{\code{addParent(path)}}{Base method for adding the parent object to a document.
#'  Specific behaviors implemented in the Collection composite class}
#'  \item{\code{getParent(path)}}{Base method for retrieving the parent object for a document.
#'  Specific behaviors implemented in the Collection composite class}
#' }
#'
#'
#' @section Document0 Input/Output Methods:
#' Methods for reading and writing individual and composite document objects.
#' \itemize{
#'  \item{readDocument(how = "txt")}{Method for reading a document.}
#'  \item{writeDocument(what, how = "txt" )}{Method for writing a document.}
#' }
#'
#'
#' @param content A character vector or list containing text content for objects. Required for write methods.
#' @param desc Character string containing the description of the document.
#' @param document An object of the Document class.  Maintained as members of objects of the DocumentClass composite class.
#' @param fileName Character string cointaining the file name for a document.  Required for objects of Document class.
#' @param format  A character string indicating the document format to be read or written.  Valid values are c("bin", "text", "csv", "rdata"). Defaults to "text"
#' @param name Character string indicating the name of the document. Required for all objects
#' @param parent An object of the Lab or DocumentCollection class that represents the parent object. Required for input / output methods.
#' @param path Character string containing the relative directory path to the document.
#' @param purge Logical indicating whether the removeDocument method should purge the document from the current environment. The default is FALSE
#' @param type = Character string indicating the type of object to be returned from get methods.  Valid values are c("object", "list", "df"). Defaults to "list".
#'
#' @field class Character string indicating the class of the object. This field used to restore objects from archive.
#' @field created A date time variable indicating the date / time the object was created.
#' @field documents A list of contained objects of the Document or DocumentCollection classes, maintained within the DocumentCollection class.
#' @field modified A date time variable indicating the date / time the object was modified.
#' @field parentName Character string indicating the name of the parent object.
#' @field parentTypes A list of valid types for parent objects.  These are initialized as "Lab", and "DocumentCollection". They may be overwritten by subclasses.
#' @field path Character string containing the relative directory path to the document. This is a concatenation of the parent's path and the object name.
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
    ..class = "Document0",
    ..content = character(0),
    ..desc = character(0),
    ..fileName = character(0),
    ..format = c("bin", "text", "txt", "csv", "rdata", "RData", "Rdata"),
    ..name = character(0),
    ..parent = character(0),
    ..parentName = character(0),
    ..path = character(0),
    ..type = c("df", "list", "object"),
    ..parentTypes = c("Lab", "DocumentCollection"),
    ..documents = list(),
    ..created = character(0),
    ..modified = character(0)
  ),

  public = list(

    # Core Methods
    initialize = function() stop("Method is not available from Document0, an abstract class!"),
    getDocument = function() stop("Method is not available from Document0, an abstract class!"),
    printDocument = function() stop("Method is not available from Document0, an abstract class!"),

    # Composite Methods
    addDocument = function(document) stop("Method is not available from Document0, an abstract class!"),
    getDocuments = function() stop("Method is not available from Document0, an abstract class!"),
    removeDocument = function(document, purge = FALSE) stop("Method is not available from Document0, an abstract class!"),
    addParent = function(parent) stop("Method is not available from Document0, an abstract class!"),
    getParent = function() stop("Method is not available from Document0, an abstract class!"),

    # Input / Output Methods
    readDocument = function() stop("Method is not available from Document0, an abstract class!"),
    writeDocument = function() stop("Method is not available from Document0, an abstract class!")
  )
)
