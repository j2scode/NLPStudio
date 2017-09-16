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
#'   \item File: The File family of classes have a one-to-one association
#'   with the Document class. Whereas a document is an abstract entity, a
#'   File object is a manifestation of the document in a physical
#'   file on disk.  Each document's file manifestation is
#'   managed by the File family of classes.
#'  }
#'
#' \strong{DocumentCollection Methods:}
#' There are six types of methods within the DocumentCollection class and they are:
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
#' \strong{DocumentCollection Core Methods:}
#'  \itemize{
#'   \item{\code{new(name, desc)}}{Method for instantiating objects of the
#'   DocumentCollection class.}
#'   \item{\code{getObject()}}{Method for returning object member data.}
#'   \item{\code{getPath()}}{Method for returning object path.}
#'   \item{\code{desc()}}{This active binding method, inherited from
#'   the Document0 class gets and sets the DocumentCollection object description.}
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
#' \strong{DocumentCollection Read/Write Methods:}
#'  \itemize{
#'   \item{\code{readDocument()}}{Method for reading a DocumentCollection object.}
#'   \item{\code{writeDocument(content)}}{Method for writing a DocumentCollection object.}
#'  }
#'
#'  \strong{DocumentCollection File Methods:}
#'  \itemize{
#'   \item{\code{addFile(file)}}{This method is not implemented for this class.}
#'   \item{\code{removeFile(file)}}{This method is not implemented for this class.}
#'  }
#'
#' \strong{DocumentCollection Visitor Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Method which accepts operations from
#'   visitor classes.}
#'  }
#'
#' \strong{DocumentCollection State Methods:}
#'  \itemize{
#'  \item{\code{saveState()}}{Method for saving the current state of an document.}
#'  \item{\code{restoreState(stateId)}}{Method for restoring a document to a prior state.}
#' }
#'
#' @param name Character string indicating the name of the document or file. Required for all objects.
#' @param desc Character string containing the description of the document.
#' @param content Nested list of content to be written to files.
#' @param parent An object of the Lab or DocumentCollection class that represents
#' the parent object.
#' @param file This parametes is not used in this method.
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
      private$..desc <- desc
      private$..created <-Sys.time()
      private$..modified <- Sys.time()

      # Assign the name to the object in the global environment and update state
      assign(name, self, envir = .GlobalEnv)

      # Log Event
      historian$addEvent(class = "DocumentCollection", objectName = name,
                         method = "initialize",
                         event = paste("Instantiated DocumentCollection",
                                       "object", name))

      invisible(self)
    },

    getObject = function() {

      document <- list(
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        documents = private$..documents,
        created = private$..created,
        modified = private$..modified
      )
      return(document)
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
      stateManager$saveState(object = self,
                             desc = paste("Added",
                                          d$name, "to",
                                          private$..name,
                                          "collection."))

      # Log Event
      historian$addEvent(class = "DocumentCollection", objectName = private$..name,
                         method = "addChild",
                         event = paste("Added", class(document)[1],
                                       "object", d$name, "to", private$..name,
                                       "DocumentCollection,"))
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
      private$..modified <- Sys.time()

      # Update State
      stateManager$saveState(object = self,
                             desc = paste("Removed",
                                         d$name, "from",
                                         private$..name,
                                         "collection."))

      # Log Event
      historian$addEvent(class = "DocumentCollection", objectName = private$..name,
                         method = "removeChild",
                         event = paste("Removed", class(document)[1],
                                       "object", d$name, "from", private$..name,
                                       "DocumentCollection,"))
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

      private$..parent <- parent

      # Log Event
      p <- parent$getObject()
      historian$addEvent(class = "DocumentCollection", objectName = private$..name,
                         method = "setAncestor",
                         event = paste("Set parent for DocumentCollection object",
                                       private$..name, "to", class(parent)[1],
                                       "class object,", p$name))

    },

    #-------------------------------------------------------------------------#
    #                         Read/Write Methods                              #
    #-------------------------------------------------------------------------#
    readDocument = function(format = "text") {

      r <- private$..reader

      documents <- lapply(private$..documents, function(d) {
        d$readDocument(format)
      })

      # Log Event
      historian$addEvent(class = "DocumentCollection", objectName = private$..name,
                         method = "readDocument",
                         event = paste("Read", class(self)[1],
                                       "object,", private$..name))
      return(documents)
    },

    writeDocument = function(content) {

      lapply(private$..documents, function(d) {
        w$writeDocument(format, content)
      })

      # Log Event
      historian$addEvent(class = "DocumentCollection", objectName = private$..name,
                         method = "writeDocument",
                         event = paste("Wrote", class(self)[1],
                                       "object,", private$..name))
    },

    #-------------------------------------------------------------------------#
    #                              File Methods                               #
    #-------------------------------------------------------------------------#
    addFile = function(file) stop("The addFile method is not implemented for the DocumentCollection class."),
    removeFile = function(file) stop("The addFile method is not implemented for the DocumentCollection class."),


    #-------------------------------------------------------------------------#
    #                             Visitor Methods                             #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {

      v <- visitor$getObject()
      # Log Event
      historian$addEvent(class = "DocumentCollection", objectName = private$..name,
                         method = "accept",
                         event = paste("Accepted", class(visitor)[1],
                                       "object,", v$name))
      visitor$visitDocumentCollection(self)

    },

    #-------------------------------------------------------------------------#
    #                            State Methods                                #
    #-------------------------------------------------------------------------#
    saveState = function() {
      stateId <- stateManager$saveState(self)

      # Log Event
      historian$addEvent(class = "DocumentCollection", objectName = private$..name,
                         method = "saveState",
                         event = paste("Saved state of Document class object",
                                       private$..name))
    },

    restoreState = function(stateId) {
      object <- stateManager$restoreState(stateId)
      o <- object$getObject()
      private$..name <- o$name
      private$..desc <- o$desc
      private$..parent <- o$parent
      private$..file <- o$file

      # Log Event
      historian$addEvent(class = "DocumentCollection", objectName = private$..name,
                         method = "restoreState",
                         event = paste("Restored state of DocumentCollection",
                                       "class object", private$..name,
                                       "to state", stateId))

      invisible(self)
    }
  )
)
