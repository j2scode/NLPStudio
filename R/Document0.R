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
#'  \item DocumentCollection: The composite class that maintains the
#'  hierarchical structure of document collections (composites) and individual
#'  documents (leafs).
#'  \item Document: An "abstract leaf" class defines the interface for
#'  DocumentText, DocumentRdata, DocumentCsv, and DocumentXlsx sub-classes.
#'  \item DocumentText: This "concrete leaf" class for text documents.
#'  \item DocumentCsv: This "concrete leaf" class for csv documents.
#'  \item DocumentRdata: This "concrete leaf" class for RData documents.
#'  \item DocumentXlsx: This "concrete leaf" class for excel documents.
#'  }
#'
#' \strong{Document Family of Classes Collaborators:}
#' The collaborators of the Document family  are:
#'  \itemize{
#'   \item Lab: Class responsible for document collections.
#'   \item State: Class responsible for saving current and restoring prior states of objects.
#'   \item Historian: Class responsible for maintaining the history of events on objects.
#'   \item Curator: Class responsible for maintaining the object hierarchy.
#'   \item Reader: Class responsible for initiating the document read operation.
#'   \item Writer: Class responsible for initiating the document write operation.
#'   \item VReader: Visitor class responsible for performing read operations through the Document hierarchy.
#'   \item VWriter: Visitor class responsible for performing write operations through the Document hierarchy.
#'   \item VCurator: Visitor class that fulfills commands from the Curator class.
#'  }
#'
#' \strong{Document0 Methods:}
#' There are six types of methods within the Document0 class and they are:
#' \itemize{
#'  \item{Core Methods: Core methods shared by both Document and
#'  DocumentCollection objects.}
#'  \item{Getter/Setter Methods: Active binding methods for getting and setting
#'  selected private members.}
#'  \item{Composite Methods: Methods implemented by the DocumentCollection
#'  class to maintain the document heirarchy.}
#'  \item{State Methods: Methods for saving current and restoring prior states of objects .}
#'  \item{Visitor Methods: Methods for implementation of and messaging
#'  with objects of the visitor classes.}
#' }
#'
#' \strong{Document0 Core Methods:}
#'  \itemize{
#'   \item{\code{new(name, desc)}}{Base method for instantiating
#'   an object of the Document or DocumentCollection classes.
#'   Specific behaviors implemented in the subclasses. }
#'   \item{\code{getName()}}{Returns the name of the current object.}
#'   \item{\code{exposeObject(requester)}}{Base method for returning the elements of the current object if invoked by an authorized method.}
#'   \item{\code{restore(requester, prior)}}{Base method for restoring an object
#'   to a prior state, as per the object parameter.}
#' }
#'
#' \strong{Document0 Field Getter/Setter Active Binding Methods:}
#'  \itemize{
#'   \item{\code{desc()}}{Method used to get / set the description variable.
#'   Implemented as an active binding and so the field may be updated
#'   by assignment. This method is concrete and inherited by sub-classes.}
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
#'   \item{\code{parent(value)}}{Getter/setter method for the parent field, implemented as an active binding on the private member.}
#' }
#'
#' \strong{Document0 State Methods:}
#'  \itemize{
#'   \item{\code{saveState()}}{Method for saving the current state of an object to file.}
#'   \item{\code{restoreState(prior)}}{Method for restoring an object to a prior state.}
#'  }
#'
#' \strong{Document0 Visitor Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Method for accepting the visitor objects. Subclasses override these methods.}
#' }
#'
#' @param name Character string indicating the name of the document or file. Required for all objects.
#' @param desc Character string containing the description of the document.
#' @param parent An object of the Lab or DocumentCollection class that represents the parent object.
#' @param visitor An object from one of the visitor classes.
#' @param prior A Document Family Class object, deserialized from a prior state.
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
    ..stateId = character(0),
    ..stateDesc = character(0),
    ..created = character(0),
    ..modified = character(0)

  ),

  active = list(

    desc = function(value) {
      if (missing(value)) {
        private$..desc
      } else {
        private$..desc <- value
        private$..modified <- Sys.time()
        private$..stateDesc <- paste(private$..name, "description changed at",
                                     Sys.time())
        # self$saveState()
      }
    },

    parent = function(value) {
      if (missing(value)) {
        private$..parent
      } else {
        v <- Validator$new()
        if (v$setParent(self, value) == FALSE) {
          stop()
        }
        # Before
        private$..stateDesc <- paste("Memento of ",private$..name, "prior to",
                                     "changing its parent at", Sys.time())
        # self$saveState()

        private$..parent <- value
        private$..modified <- Sys.time()

        # After
        private$..stateDesc <- paste(private$..name, "parent changed to",
                                     value, "at", Sys.time())
        # self$saveState()
      }
    }
  ),

  public = list(

    # Core Methods
    initialize = function(name, desc) stop("Method is not available from Document0, an abstract class!"),
    getName = function() private$..name,
    exposeObject = function(requester) stop("Method is not available from Document0, an abstract class!"),
    restore = function(requester, prior) stop("Method is not available from Document0, an abstract class!"),

    # Composite Methods
    getChildren = function() stop("Method is not available from Document0, an abstract class!"),
    addChild = function(document) stop("Method is not available from Document0, an abstract class!"),
    removeChild = function(document) stop("Method is not available from Document0, an abstract class!"),
    getAncestor = function() stop("Method is not available from Document0, an abstract class!"),
    setAncestor = function(parent) stop("Method is not available from Document0, an abstract class!"),

    # Visitor Methods
    saveState = function() stop("Method is not available from Document0, an abstract class!"),
    restoreState = function(prior)  stop("Method is not available from Document0, an abstract class!"),

    # Visitor Methods
    accept = function(visitor) stop("Method is not available from Document0, an abstract class!")
  )
)
