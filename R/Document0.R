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
#'  }
#'
#' \strong{Document0 Methods:}
#' There are six types of methods within the Document0 class and they are:
#' \itemize{
#'  \item{Core Methods: Core methods shared by both Document and
#'  DocumentCollection objects.}
#'  \item{Composite Methods: Methods implemented by the DocumentCollection
#'  class to maintain the document heirarchy.}
#'  \item{State Methods: Methods for saving and restoring state of the object.}
#'  \item{Visitor Methods: Methods for implementation of and messaging
#'  with objects of the visitor classes.}
#' }
#'
#' \strong{Document0 Core Methods:}
#'  \itemize{
#'   \item{\code{new(name, desc)}}{Base method for instantiating
#'   an object of the Document or DocumentCollection classes.
#'   Specific behaviors implemented in the subclasses. }
#'   \item{\code{desc()}}{Method used to get / set the description variable.
#'   Implemented as an active binding and so the field may be updated
#'   by assignment. This method is concrete and inherited by sub-classes.}
#'   \item{\code{getObject()}}{Base method for retrieving an object of the
#'   "Document" or "DocumentCollection" class. Specific behaviors
#'   implemented in the subclasses.}
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
#' \strong{Document0 State Methods:}
#'  \itemize{
#'   \item{\code{saveState()}}{Method that initiates the process of saving the current state of the object.}
#'   \item{\code{restoreState(stateId)}}{Method that initiates the process of restoring an object to a prior state.}
#'  }
#'
#' \strong{Document0 Visitor Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Method for accepting the visitor objects. Subclasses override these methods.}
#'  }
#'
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
    ..documents = list(),
    ..fileName = character(0),
    ..state = character(0),
    ..stateId = character(0),
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

    # Composite Methods
    getChildren = function() stop("Method is not available from Document0, an abstract class!"),
    addChild = function(document) stop("Method is not available from Document0, an abstract class!"),
    removeChild = function(document) stop("Method is not available from Document0, an abstract class!"),
    getAncestor = function() stop("Method is not available from Document0, an abstract class!"),
    setAncestor = function(parent) stop("Method is not available from Document0, an abstract class!"),

    # State Methods
    saveState = function() {
      v <- ValidateClass$new()
      if (v$validate(class = "Document0", level = "Error", method = "saveState",
                     fieldName = "class(self)", value = class(self)[1],
                     msg = paste("The saveState method is not implemented for the Document0 class."),
                     expect = "Document0") == TRUE) {
        stop()
      }
      state <- State$new()
      state$save(self)
    },

    restoreState = function(stateId) {
      v <- ValidateClass$new()
      if (v$validate(class = "Document0", level = "Error", method = "restoreState",
                     fieldName = "class(self)", value = class(self)[1],
                     msg = paste("The restoreState method is not implemented for the Document0 class."),
                     expect = "Document0") == TRUE) {
        stop()
      }
      private$..stateId <- stateId
      state <- State$new()
      state$restore(self)
    },

    # Visitor Methods
    accept = function(visitor) stop("Method is not available from Document0, an abstract class!")
  )
)
