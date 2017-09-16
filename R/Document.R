#==============================================================================#
#                                   Document                                   #
#==============================================================================#
#' Document
#'
#' \code{Document} Class for instantiating and ,returning and printing individual
#' Document objects within objects of the DocumentCollection class.
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
#' \strong{Document Methods:}
#' There are six types of methods within the Document class and they are:
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
#' \strong{Document Core Methods:}
#'  \itemize{
#'   \item{\code{new(name, desc)}}{Method for instantiating a document}
#'   \item{\code{getObject()}}{Method for obtaining the document
#'   meta data.}
#'   \item{\code{getPath()}}{Method returns the absolute path for an object's files.}
#'   \item{\code{desc()}}{Method used to get / set the description variable.
#'   This method is inherited from the Document0 class.}
#'  }
#'
#'  #' \strong{File Methods:}
#'  \itemize{
#'   \item{\code{new(name, desc)}}{Method for instantiating a document}
#'   \item{\code{getObject()}}{Method for obtaining the document
#'   meta data.}
#'   \item{\code{getPath()}}{Method returns the absolute path for an object's files.}
#'   \item{\code{desc()}}{Method used to get / set the description variable.
#'   This method is inherited from the Document0 class.}
#'  }
#'
#'
#'   \item{\code{addFile()}}{Method used to get / set the description variable.
#'   This method is inherited from the Document0 class.}
#' }
#'
#' \strong{Document Visitor Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Method for accepting objects of
#'   one of the visitor classes.}
#'  }
#'
#' \strong{Document State Methods:}
#'  \itemize{
#'  \item{\code{saveState()}}{Method for saving the current state of a document
#'  object.}
#'  \item{\code{restoreState(stateId)}}{Method for restoring the current state
#'  of a document..}
#' }
#'
#' @param name Character string indicating the name of the document or file. Required for all objects.
#' @param desc Character string containing the description of the document.
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
Document <- R6::R6Class(
  classname = "Document",
  inherit = Document0,
  lock_objects = FALSE,
  lock_class = FALSE,

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Confirm required parameters are not missing.
      if (missing(name)) {
        v <- Validate0$new()
        v$notify(class = "Document", method = "initialize", fieldName = "name",
                 value = "", level = "Error",
                 msg = paste("Name parameter is missing with no default.",
                             "See ?Document for further assistance."),
                 expect = NULL)
        stop()
      }

      # Validate name
      v <- ValidateName$new()
      if (v$validate(class = "Document", method = "initialize",
                     value = name, expect = FALSE) == FALSE) {
        stop()
      }

      # Instantiate variables
      private$..name <- name
      private$..desc <- desc
      private$..created <- Sys.time()
      private$..modified <- Sys.time()

      # Assign to object to name  in global environment
      assign(name, self, envir = .GlobalEnv)

      # Log event
      historian$addEvent(class = "Document", objectName = name,
                         method = "initialize",
                         event = paste("Instantiated", name, "Document"))

      invisible(self)
    },

    getOjbect = function() {

      document <- list (
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        file = private$..file,
        created = private$..created,
        modified = private$..modified
      )

      return(document)
    },

    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#
    addChild = function(document) { stop("This method not implemented for this class")},
    getChildren = function() { stop("This method not implemented for this class")},
    removeChild = function(name, purge = FALSE) { stop("This method not implemented for this class")},

    getAncestor = function() private$..parent,

    setAncestor = function(parent) {

      v <- ValidateClass$new()
      if (v$validate(class = "Document", method = "setAncestor", fieldName = "class(parent)",
                     level = "Error", value = class(parent)[1],
                     msg = paste("Unable to set parent.  Parent must be a",
                                 "DocumentCollection object.",
                                 "See ?Document for assistance."),
                     expect = "DocumentCollection") == FALSE) {
        stop()
      }

      # Get parent information
      p <- parent$getObject()

      # Set parent variable
      private$..parent <- parent

      # TODO: Implement move functionality
      private$..file$moveFile(parent)

      historian$addEvent(class = "Document", objectName = private$..name,
                         method = "setAncestor",
                         event = paste("Set ancestor of,",
                                       private$..name, "to",
                                       p$name))
    },

    #-------------------------------------------------------------------------#
    #                           Read/Write Methods                            #
    #-------------------------------------------------------------------------#

    #-------------------------------------------------------------------------#
    #                              File Methods                               #
    #-------------------------------------------------------------------------#

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {

      # Log Event
      v <- visitor$getObject()
      historian$addEvent(class = "Document", objectName = private$..name,
                         method = "accept",
                         event = paste("Accepted", class(visitor)[1],
                                       "class object,", v$name))

      visitor$visitDocument(self)
    },

    #-------------------------------------------------------------------------#
    #                            State Methods                                #
    #-------------------------------------------------------------------------#
    saveState = function() {
      stateId <- stateManager$saveState(self)

      # Log Event
      historian$addEvent(class = "Document", objectName = private$..name,
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
      historian$addEvent(class = "Document", objectName = private$..name,
                         method = "restoreState",
                         event = paste("Restored state of Document class object",
                                       private$..name, "to state", stateId))

      invisible(self)
    }
  )
)
