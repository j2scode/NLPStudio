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
#' \strong{DocumentCollection Methods:}
#' There are six types of methods within the DocumentCollection class and they are:
#' \itemize{
#'  \item{Core Methods: Core methods shared by both Document and
#'  DocumentCollection objects.}
#'  \item{Getter/Setter Methods: Active binding methods for getting and setting
#'  selected private members.}
#'  \item{Composite Methods: Methods implemented by the DocumentCollection
#'  class to maintain the document heirarchy.}
#'  \item{Visitor Methods: Methods for implementation of and messaging
#'  with objects of the visitor classes.}
#' }
#'
#' \strong{DocumentCollection Core Methods:}
#'  \itemize{
#'   \item{\code{new(name, desc)}}{Method for instantiating a document collection.}
#'   \item{\code{getObject()}}{Method for obtaining the document collection data in a list format.}
#'   \item{\code{setObject(object)}}{Method for restoring a document collection to a prior state, as per the object parameter.}
#' }
#'
#' \strong{DocumentCollection Field Getter/Setter Active Binding Methods:}
#'  \itemize{
#'   \item{\code{desc()}}{Method used to get / set the description variable.
#'   Implemented as an active binding and so the field may be updated
#'   by assignment. This method is inherited from the Document0 class.}
#'   \item{\code{fileName()}}{This method is not implemented for the DocumentCollection class.}
#' }
#'
#' \strong{DocumentCollection Composite Methods:}
#'  \itemize{
#'   \item{\code{addChild(document)}}{Method adds a Document or DocumentCollection object
#'   as a child object.}
#'   \item{\code{getChildren()}}{Method retrieves the child documents of the object.}
#'   \item{\code{removeChild(document)}}{Method removes a Document or DocumentCollection
#'   object from its list of child documents.}
#'   \item{\code{getAncestor()}}{Method returns the parent object.}
#'   \item{\code{setAncestor(parent)}}{Method sets the parent object and moves any
#'   associated files to the new parent.}
#' }
#'
#'
#' \strong{DocumentCollection Visitor Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Method for accepting the objects of a visitor class.}
#'   \item{\code{acceptUpdate(visitor, object)}}{Accepts an object of the VUpdate class.}
#'   \item{\code{acceptAdd(visitor, object)}}{Accepts an object of the VAddChild class.}
#'   \item{\code{acceptRemove(visitor, object)}}{Accepts an object of the VRemoveChild class.}
#'   \item{\code{acceptAssociate(visitor, object)}}{Accepts an object of the VAssociate class.}
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

  public = list(
    #-------------------------------------------------------------------------#
    #                            Core Methods                                 #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Validate Name
      v <- ValidateName$new()
      if (v$validate(class = "DocumentCollection", method = "initialize",
                 value = name, expect = FALSE) == FALSE) {
        stop()
      }

      # Instantiate variables
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "Document Collection"), desc)
      private$..state <- paste("DocumentCollection", name, "instantiated at", Sys.time())
      private$..created <-Sys.time()
      private$..modified <- Sys.time()

      # Assign the name to the object in the global environment and update state
      assign(name, self, envir = .GlobalEnv)

      # Log Event
      historian$addEvent(class = "DocumentCollection", objectName = name,
                         method = "initialize",
                         event = private$..state)

      invisible(self)
    },

    getObject = function() {

      document <- list(
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        documents = private$..documents,
        state = private$..state,
        stateId = private$..stateId,
        created = private$..created,
        modified = private$..modified
      )
      return(document)
    },

    setObject = function(object) {
      o <- object$getObject()
      private$..desc <- o$desc
      private$..state <- o$state
      private$..stateId <- o$stateId
      private$..created <- o$created
      private$..modified <- o$modified
      invisible(self)
    },


    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#
    getChildren = function() { private$..documents },

    addChild = function(document) {

      # Validate document
      v <- ValidateClass$new()
      if (v$validate(class = "DocumentCollection", method = "addDocument",
                 fieldName = "document", value = document, level = "Error",
                 msg = "Argument is not a Document class object.",
                 expect = c("Document", "DocumentCollection")) == FALSE) {
        stop()
      }

      # Add document to list of documents for collection
      d <- document$getObject()
      private$..documents[[d$name]] <- document

      # Update the parent of the document object
      document$setAncestor(self)

      # Update State
      private$..state <- paste("Added", class(document)[1], "object",
                               "to the", private$..name, "DocumentCollection",
                               "at", Sys.time())
      private$..modified <- Sys.time()
      self$saveState()

      # Log Event
      historian$addEvent(class = "DocumentCollection", objectName = private$..name,
                         method = "addChild",
                         event = private$..state)
    },

    removeChild = function(document) {

      # Validate parameters
      if (missing(document)) {
        v <- Validate0$new()
        v$notify(class = "DocumentCollection", method = "removeChild",
                 fieldName = "document", value = "", level = "Error",
                 msg = paste("Document is missing with no default.",
                             "See ?DocumentCollection for further assistance."),
                 expect = TRUE)
        stop()
      }

      # Obtain document information
      d <- document$getObject()

      # Confirm document exists
      if (!exists(private$..documents[[d$name]])) {
        v <- Validate0$new()
        v$notify(class = "DocumentCollection", method = "removeChild",
                 fieldName = "document", value = "", level = "Error",
                 msg = paste("Document is not in the.", private$..name,
                             "collection.",
                             "See ?DocumentCollection for further assistance."),
                 expect = TRUE)
        stop()

      }

      # Remove document from collection
      private$..documents[[d$name]] <- NULL

      # Update State
      private$..state <- paste("Removed", class(document)[1], "object",
                               "from the", private$..name, "DocumentCollection",
                               "at", Sys.time())
      private$..modified <- Sys.time()
      self$saveState()

      # Log Event
      historian$addEvent(class = "DocumentCollection", objectName = private$..name,
                         method = "removeChild",
                         event = private$..state)
    },

    getAncestor = function() {

      p <- private$..parent

      return(p)
    },

    setAncestor = function(parent) {

      if (missing(parent)) {
        v <- Validate0$new()
        v$notify(class = "DocumentCollection", method = "setAncestor",
                 fieldName = "parent", value = "", level = "Error",
                 msg = paste("Parent is missing with no default.",
                             "See ?DocumentCollection for further assistance."),
                 expect = TRUE)
        stop()
      }

      v <- ValidateClass$new()
      if (v$validate(class = "Document", method = "setAncestor", fieldName = "class(parent)",
                     level = "Error", value = class(parent)[1],
                     msg = paste("Unable to set parent.  Parent must be a",
                                 "DocumentCollection or Lab object.",
                                 "See ?DocumentCollection for assistance."),
                     expect = c("DocumentCollection", "Lab")) == FALSE) {
        stop()
      }

      # Set parent and obtain parent information.
      private$..parent <- parent
      p <- parent$getObject()

      # Update State
      private$..state <- paste("Set parent of DocumentCollection object,",
                               private$..name, "to", p$name, "at",
                               Sys.time())
      private$..modified <- Sys.time()
      self$saveState()

      # Log Event
      historian$addEvent(class = "DocumentCollection", objectName = private$..name,
                         method = "removeChild",
                         event = private$..state)

    },


    #-------------------------------------------------------------------------#
    #                             Visitor Methods                             #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$documentCollection(self)
    },
    acceptVUpdate = function(visitor, priorObject)  {
      visitor$documentCollection(self, priorObject)
    },
    acceptVAddChild = function(visitor, child)  {
      visitor$documentCollection(self, child)
    },
    acceptVRemoveChild = function(visitor, child)  {
      visitor$documentCollection(self, child)
    }
  )
)
