#==============================================================================#
#                           DocumentCollection                                 #
#==============================================================================#
#' DocumentCollection
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
#' \strong{DocumentCollection Collaborators:}
#' The collaborators of the Document family  are:
#'  \itemize{
#'   \item Lab: This class maintains a one-to-many "has a" association
#'   relationship with the Document Class Family.
#'   \item State: Class responsible for saving current and restoring prior states of objects.
#'   \item Historian: Class responsible for maintaining the history of events on objects.
#'   \item Reader: Class responsible for initiating the document read operation.
#'   \item Writer: Class responsible for initiating the document write operation.
#'   \item Validator: Class responsible for validating method parameters.
#'  }
#'
#' \strong{DocumentCollection Methods:}
#' There are six types of methods within the DocumentCollection class and they are:
#' \itemize{
#'  \item{Core Methods: Core methods shared by both Document and
#'  DocumentCollection objects.}
#'  \item{Getter/Setter Methods: Active binding methods for getting and setting
#'  selected private members.}
#'  \item{Composite Methods: Methods implemented by the DocumentCollection
#'  class to maintain the document heirarchy.}
#'  \item{State Methods: Methods for saving current and restoring prior object states.}
#'  \item{Visitor Methods: Methods for implementation of and messaging
#'  with objects of the visitor classes.}
#' }
#'
#' \strong{DocumentCollection Core Methods:}
#'  \itemize{
#'   \item{\code{new(name, desc)}}{Method for instantiating a document collection.}
#'   \item{\code{getName()}}{Method returns the name of the DocumentCollection object.}
#'   \item{\code{getObject(requester)}}{Method that returns the elements of the current DocumentCollection object, if invoked by an authorized method.}
#'   \item{\code{restore(requester, prior)}}{Method for restoring a document collection to a prior state, as per the object parameter.}
#' }
#'
#' \strong{DocumentCollection Field Getter/Setter Active Binding Methods:}
#'  \itemize{
#'   \item{\code{desc()}}{Method used to get / set the description variable.
#'   Implemented as an active binding and so the field may be updated
#'   by assignment. This method is inherited from the Document0 class.}
#' }
#'
#' \strong{DocumentCollection Composite Methods:}
#'  \itemize{
#'   \item{\code{addChild(document)}}{Method adds a Document or DocumentCollection object
#'   as a child object.}
#'   \item{\code{getChildren()}}{Method retrieves the child documents of the object.}
#'   \item{\code{removeChild(document)}}{Method removes a Document or DocumentCollection
#'   object from its list of child documents.}
#'   \item{\code{parent(value)}}{Getter/setter method for the parent field, implemented as an active binding on the private member.}
#' }
#'
#' #' \strong{DocumentCollection State Methods:}
#'  \itemize{
#'   \item{\code{saveState()}}{Method for saving current state of an object.}
#'   \item{\code{restoreState()}}{Methods for restoring an object to a prior state.}
#'  }
#'
#'
#' \strong{DocumentCollection Visitor Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Method for accepting the objects of a visitor class.}
#'  }
#'
#'
#' @param name Character string indicating the name of the document or file. Required for all objects.
#' @param desc Character string containing the description of the document.
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
DocumentCollection <- R6::R6Class(
  classname = "DocumentCollection",
  inherit = Document0,
  private = list(
    ..documents = list()
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                            Core Methods                                 #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Instantiate variables
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "Document Collection"), desc)
      private$..stateDesc <- paste("DocumentCollection", name, "instantiated at", Sys.time())
      private$..created <-Sys.time()
      private$..modified <- Sys.time()

      # Validate Document Collection
      v <- Validator$new()
      if (v$init(self) == FALSE) stop()

      # Assign the name to the object in the global environment and update state
      assign(name, self, envir = .GlobalEnv)

      # Log Event
      #historian$addEvent(cls = "DocumentCollection", objectName = name,
      #                   method = "initialize",
      #                   event = private$..stateDesc)

      invisible(self)
    },

    getObject = function(requester) {

      v <- Validator()
      if (v$getObject(object = self,
                      requester = requester) == FALSE) stop()

      document <- list(
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        documents = private$..documents,
        stateId = private$..stateId,
        stateDesc = private$..stateDesc,
        created = private$..created,
        modified = private$..modified
      )
      return(document)
    },

    restore = function(requester, prior) {

      v <- Validator()
      if (v$restore(object = self,
                      requester = requester) == FALSE) stop()

      private$..desc <- prior$desc
      private$..parent <- prior$parent
      private$..documents <- prior$documents
      private$..stateDesc <- paste("DocumentCollection object", private$..name,
                                   "restored to prior state designated by",
                                   "state identifier:",
                                   r$stateId,"at", Sys.time())
      private$..stateId <- prior$stateId
      private$..created <- prior$created
      private$..modified <- Sys.time()

      # Log event
      # historian$addEvent(cls = class(self)[1], objectName = name,
      #                    method = "restore",
      #                    event = private$..stateDesc)

      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#
    getChildren = function() { private$..documents },

    addChild = function(document) {

      v <- Validator()
      if (v$addChild(object = self, child = document) == FALSE) stop()

      # Add document to list of documents for collection
      d <- document$getObject()
      private$..documents[[d$name]] <- document

      # Update the parent of the document object
      document$setAncestor(self)

      # Update State
      private$..stateDesc <- paste("Added", class(document)[1], "object",
                               "to the", private$..name, "DocumentCollection",
                               "at", Sys.time())
      private$..modified <- Sys.time()
      self$saveState()

      # Log Event
      # historian$addEvent(cls = "DocumentCollection", objectName = private$..name,
      #                    method = "addChild",
      #                    event = private$..stateDesc)
    },

    removeChild = function(document) {

      v <- Validator()
      if (v$removeChild(object = self, child = document) == FALSE) stop()

      # Save Memento
      private$..stateDesc <- paste("Memento of DocumentCollection object", private$..name,
                                   "prior to removing child document, ", document$getName(),
                                   "at", Sys.time())
      private$..modified <- Sys.time()
      #self$saveState()

      # Remove document from collection
      private$..documents[[d$name]] <- NULL

      # Update State
      private$..stateDesc <- paste("Removed", class(document)[1], "object",
                               "from the", private$..name, "DocumentCollection",
                               "at", Sys.time())
      private$..modified <- Sys.time()
      #self$saveState()

      # Log Event
      # historian$addEvent(cls = "DocumentCollection", objectName = private$..name,
      #                    method = "removeChild",
      #                    event = private$..stateDesc)
    },

    #-------------------------------------------------------------------#
    #                           State Method                            #
    #-------------------------------------------------------------------#
    saveState = function() {
      state <- State$new()
      private$..stateId <- state$save(self)
    },

    restoreState = function(stateId) {
      private$..stateId <- stateId
      state <- State$new()
      state$restore(self)
      invisible(self)
    },

    #-------------------------------------------------------------------------#
    #                             Visitor Methods                             #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$documentCollection(self)
    }
  )
)
