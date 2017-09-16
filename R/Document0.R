#==============================================================================#
#                                   Document0                                  #
#==============================================================================#
#' Document0
#'
#' \code{Document0} Component class for Document composite in the NLPStudio.
#'
#' \strong{Document Family of Classes Overview:}
#'
#' The Document family of classes is an implementation of the composite
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows composite
#' and individual objects to be treated uniformly.
#'
#' The following sections include:
#' \itemize{
#'  \item Class Participants: Classes the comprise this composite pattern.
#'  \item Class Collaborators: Classes which interact with the Document0 class.
#'  \item Class Methods: Methods included in the interface.
#'  }
#'
#' \strong{Document Family of Classes Participants:}
#' The participants of the Document0 class are:
#' \itemize{
#'  \item Document0: This component class specifies an abstract interface
#'  for all leaf and composite document classes.
#'  \item Document: This "leaf" class specifies the concrete class for
#'  individual.
#'  \item DocumentCollection: The composite class that maintains the
#'  hierarchical structure of document collections (composites) and individual
#'  documents (leafs).
#'  }
#'
#' \strong{Document Family of Classes Collaborators:}
#' The collaborators of the Document family  are:
#'  \itemize{
#'   \item Lab: This class maintains a one-to-many "has a" association
#'   relationship with the Document Class Family.
#'   \item File: The File family of classes have a one-to-one association
#'   with the Document class. Whereas a document is an abstract entity, a
#'   File object is a manifestation of the document in a physical
#'   file on disk.  Each document's file manifestation is
#'   managed by the File family of classes.
#'  }
#'
#' \strong{Document0 Methods:}
#' There are six types of methods within the Document0 class and they are:
#' \itemize{
#'  \item{Core Methods: Core methods shared by both Document and
#'  DocumentCollection objects.}
#'  \item{Composite Methods: Methods implemented by the DocumentCollection
#'  class to maintain the document heirarchy.}
#'  \item{Read/Write Methods: Methods responsible for reading and writing
#'  objects of the Document and DocumentCollection classes.}
#'  \item{File Methods: Methods implemented by the Document class to add
#'  and remove document file information.}
#'  \item{Visitor Methods: Methods for implementation of and messaging
#'  with objects of the visitor classes.}
#'  \item{State Methods: Methods responsible for save and restoring objects
#'  at designated states.}
#' }
#'
#' \strong{Document0 Core Methods:}
#'  \itemize{
#'   \item{\code{new(name, desc)}}{Base method for instantiating
#'   an object of the Document or DocumentCollection classes.
#'   Specific behaviors implemented in the subclasses. }
#'   \item{\code{getObject()}}{Base method for retrieving an object of the
#'   "Document" or "DocumentCollection" class. Specific behaviors
#'   implemented in the subclasses.}
#'   \item{\code{getPath()}}{Method tha returns the relative path for the object's files.}
#'   \item{\code{desc()}}{Method used to get / set the description variable.
#'   Implemented as an active binding and so the field may be updated
#'   by assignment. This method is concrete and inherited by sub-classes.}#'
#' }
#'
#' \strong{Document0 Composite Methods:}
#'  \itemize{
#'   \item{\code{addChild(document)}}{Base method for adding documents to a
#'   collection. Specific behaviors implemented in the DocumentCollection composite
#'   sub-class}
#'   \item{\code{getChildren()}}{Base method for retrieving child objects. Specific behaviors
#'   implemented in the DocumentCollection subclass }
#'   \item{\code{removeChild(document)}}{Base method for removing documents from
#'   a collection. Specific behaviors implemented in the DocumentCollection composite
#'   sub-class}
#'   \item{\code{getAncestor()}}{Base method for retrieving the parent object for a document.
#'   Specific behaviors implemented in the Document and DocumentCollection composite
#'   sub-classes}
#'   \item{\code{setAncestor(parent)}}{Base method for adding the parent
#'   object to a document. Specific behaviors implemented in the Document and
#'   DocumentCollection sub-classes.}
#' }
#'
#' \strong{Document0 Read/Write Methods:}
#'  \itemize{
#'   \item{\code{readDocument()}}{Base method for reading objects of the Document
#'   or DocumentCollection class. Specific behaviors implemented in the Document
#'   and DocumentCollection composite sub-classes.}
#'  }
#'  \item{\code{writeDocument(content)}}{Base method for writing objects of the Document
#'   or DocumentCollection class. Specific behaviors implemented in the Document
#'   and DocumentCollection composite sub-classes.}
#'  }
#'
#' \strong{Document0 File Methods:}
#'  \itemize{
#'   \item{\code{addFile()}}{Base method for adding files to objects of the Document
#'   class.}
#'   \item{\code{removeFile()}}{Base method for removing files from objects of the Document
#'   class.}
#'  }
#'
#' \strong{Document0 Visitor Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Base method for accepting visitor methods. Specific
#'   behaviors implemented in the Document and DocumentCollection composite
#'   sub-classes.}
#'  }
#'
#' \strong{Document0 State Methods:}
#'  \itemize{
#'  \item{\code{saveState()}}{Base method for saving the current state of the object.
#'   visitor methods. Specific behaviors implemented in the Document and
#'   DocumentCollection composite sub-classes.}
#'  \item{\code{restoreState(stateId)}}{Base method for restoring an object to
#'  a previous state, as indicated by the stateId. Specific behaviors implemented
#'  in the Document and DocumentCollection composite sub-classes.}
#' }
#'
#' @param name Character string indicating the name of the document or file. Required for all objects.
#' @param desc Character string containing the description of the document.
#' @param content List of content to be written to file.
#' @param fileName Character string indicating File object's file name.
#' @param parent An object of the Lab or DocumentCollection class that represents
#' the parent object.
#' @param visitor An object from one of the visitor classes.
#' @param stateId Character string that uniquely identifies an object and its
#' state at a specific point in time.
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
    ..children = list(),
    ..file = character(),
    ..created = character(0),
    ..modified = character(0)
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

    # Core Methods
    initialize = function(name, desc) stop("Method is not available from Document0, an abstract class!"),
    getObject = function() stop("Method is not available from Document0, an abstract class!"),
    getPath = function()  return(file.path(private$..parent$getPath(), private$..name)),

    # Composite Methods
    getChildren = function() stop("Method is not available from Document0, an abstract class!"),
    addChild = function(document) stop("Method is not available from Document0, an abstract class!"),
    removeChild = function(document) stop("Method is not available from Document0, an abstract class!"),
    getAncestor = function() stop("Method is not available from Document0, an abstract class!"),
    setAncestor = function(parent) stop("Method is not available from Document0, an abstract class!"),

    # Read/Write Methods
    readDocument = function() stop("Method is not available from Document0, an abstract class!"),
    writeDocument = function(content) stop("Method is not available from Document0, an abstract class!"),

    # File Methods
    addFile = function(name, fileName, desc) stop("Method is not available from Document0, an abstract class!"),
    removeFile = function(name) stop("Method is not available from Document0, an abstract class!"),

    # Visitor Methods
    accept = function(visitor) stop("Method is not available from Document0, an abstract class!"),

    # State Methods
    setState = function() stop("Method is not available from Document0, an abstract class!"),
    restoreState = function(stateId) stop("Method is not available from Document0, an abstract class!")
  )
)
