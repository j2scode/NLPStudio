#==============================================================================#
#                           DocumentCollection                                 #
#==============================================================================#
#' DocumentCollection
#'
#' \code{DocumentCollection} Class for managing composite document collections
#' in the NLPStudio
#'
#' \strong{DocumentCollection Class Overview:}
#'
#' The DocumentCollection class is the composite part of the Document0 composite
#' class. This is an implementation of the composite pattern documented in the
#' book "Design Patterns: Elements of Reusable Object-Oriented Software" by
#' Erich Gamma, Richard Helm, Ralph Johnson and John Vlissides (hence Gang of Four).
#' This pattern allows composite and individual objects to be treated uniformly.
#'
#' \strong{DocumentCollection Core Methods:}
#' The core methods are as follows:
#' \itemize{
#'  \item{\code{new(name, desc)}}{Method for instantiating an object of the
#'  DocumentCollection classes.}
#'  \item{\code{getObject()}}{Method for retrieving the object and its
#'  meta data.}
#'  \item{\code{accept(visitor)}}{Method for accepting a visitor.  It
#'  dispatches the appropriate visitor, passing 'self' as a parameter.}
#'  \item{\code{addContent(content)}}{Not implemented for the DocumentCollection
#'  class.}
#'  \item{\code{removeContent()}}{Not implemented for the DocumentCollection
#'  class.}
#' }
#'
#' @section DocumentCollection Composite Methods:
#' The composite methods are used to manage parent / child relationshipos among
#' the documents, and are:
#' \itemize{
#'  \item{\code{addDocument(document)}}{Base method for adding documents to a
#'  collection. Specific behaviors implemented in the Collection composite class}
#'  \item{\code{removeDocument(document)}}{Base method for removing documents from
#'  a collection. Specific behaviors implemented in the Collection composite class}
#'  \item{\code{addPath(path)}}{Base method for adding a path to a document.
#'  Specific behaviors implemented in the Collection composite class}
#' }
#'
#'
#' @section DocumentCollection Input/Output Methods:
#' Methods for reading and writing individual and composite document objects.
#' \itemize{
#'  \item{readDocument(what, how)}{Method for reading a document.}
#'  \item{writeDocument(what, how)}{Method for writing a document.}
#' }
#'
#'
#' @param desc Character string containing the description of the document.
#' @param document An object of the Document class.  Used in DocumentClass composite methods
#' @param name Character string indicating the name of the document (required)
#' @param parent The parent object for an object of the DocumentCollection class. Parents may be objects of the DocumentCollection or Lab classes.
#' @param purge Logical indicating whether the removeDocument method should purge the document from the current environment. The default is FALSE
#'
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
      if (v$validate(cls = "DocumentCollection", method = "initialize",
                 value = name, expect = FALSE) == FALSE) {
        stop()
      }

      # Get orphan lab information
      o <- OrphanCollections$getObject()

      # Instantiate variables
      private$..name <- name
      private$..desc <- desc
      private$..parent <- OrphanCollections
      private$..parentName <- o$name
      private$..path <- file.path(o$path, name)
      private$..created <-Sys.time()
      private$..modified <- Sys.time()

      # Assign the name to the object in the global environment and update state
      assign(name, self, envir = .GlobalEnv)

      invisible(self)
    },

    getObject = function() {

      document <- list(
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        parentName = private$..parentName,
        path = private$..path,
        documents = private$..documents,
        created = private$..created,
        modified = private$..modified
      )
      return(document)
    },

    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#

    getChildren = function() {

      documents <- lapply(private$..documents, function(d) {
        d$getObject()
      })

      return(documents)
    },

    addChild = function(document) {

      # Validate document
      v <- ValidateClass$new()
      if (v$validate(cls = "DocumentCollection", method = "addDocument",
                 fieldName = "document", value = document, level = "Error",
                 msg = "Argument is not a Document class object.",
                 expect = "Document") == FALSE) {
        stop()
      }

      # Add document to list of documents for collection
      d <- document$getObject()
      private$..documents[[d$name]] <- document

      # Update the parent of the document object
      document$setAncestor(self)

      # Update state
      stateManager$saveState(d$name, document)
    },

    removeChild = function(document, purge = FALSE) {

      # Validate parameters
      if (missing(document)) {
        v <- Validate0$new()
        v$notify(cls = "DocumentCollection", method = "removeDocument",
                 fieldName = "document", value = "", level = "Error",
                 msg = paste("Document is missing with no default.",
                             "See ?DocumentCollection for further assistance."),
                 expect = TRUE)
        stop()
      }

      # Validate document class
      v <- ValidateClass$new()
      if (v$validate(cls = "DocumentCollection", method = "removeDocument",
               fieldName = "document", value = "", level = "Error",
               msg = paste("Parameter is not a valid 'Document'",
                           "or 'DocumentCollection'object.",
                           "See ?DocumentCollection for further assistance."),
               expect = c("Document", "DocumentCollection")) == FALSE) {
        stop()
      }

      # Obtain document information
      d <- document$getObject()

      # Confirm document is not self
      if (documentInfo$name == private$..name) {
        v <- Validate0$new()
        v$notify(cls = "DocumentCollection", method = "removeDocument",
                 fieldName = "name", value = name, level = "Error",
                 msg = paste("Object", d$name,
                             "cannot remove itself. Remove operations must",
                             "be performed by the parent object.",
                             "See ?DocumentCollection and ?Lab for further assistance"),
                 expect = NULL)
        stop()
      }

      # Remove document from collection
      private$..documents[[d$name]] <- NULL
      private$..modified <- Sys.time()

      # Remove from  memory and disc if purge == TRUE
      if (purge == TRUE) {

        # Remove from disc
        base::unlink(documentInfo$path)

        # Remove from global environment
        rm(list = ls(envir = .GlobalEnv)[grep(documentInfo$name,
                                              ls(envir = .GlobalEnv))],
           envir = .GlobalEnv)

        # Update state

      }
      # Update State
      stateManager$saveState(self)
    },

    getAncestor = function() {

      p <- private$..parent

      return(p)
    },

    setAncestor = function(parent) {

      oldParent <- self$getAncestor()
      oldParent <- oldparent$getObject()
      oldPath <- oldParent$path

      if (class(parent)[1] %in% c("DocumentCollection", "Lab")) {
        # Add parent
        private$..parent <- parent

        # Add parent name
        p <- parent$getObject()
        private$..parentName <- p$name

        # Update path
        private$..path <- file.path(p$path, private$..name)

        # Move files to new collection
        files <- list.files(path = oldPath, all.files = TRUE, full.names = TRUE,
                            recursive = TRUE, include.dirs = TRUE)
        if (length(files) != 0) {
          file.copy(oldPath, private$..path)
          base::unlink(oldPath)
        }

        # Update state
        # Update State
        stateManager$saveState(self)


      } else {
        v <- ValidateClass$new()
        v$notify(cls = "DocumentCollection", method = "setAncestor",
                 fieldName = "parent", level = "Error", value = parent,
                 msg = paste("Unable to add parent object. Objects of the",
                             "DocumentCollection class may only",
                             "have DocumentCollection or Lab",
                             "objects as parents",
                             "See ?DocumentCollection for further assistance."),
                 expect = "DocumentCollection")
        stop()
      }
    },

    #-------------------------------------------------------------------------#
    #                                  I/O                                    #
    #-------------------------------------------------------------------------#
    readDocument = function(format = "text") {

      r <- private$..reader

      documents <- lapply(private$..documents, function(d) {
        d$readDocument(format)
      })
      return(documents)
    },

    writeDocument = function(content) {

      lapply(private$..documents, function(d) {
        w$writeDocument(format, content)
      })
    },

    #-------------------------------------------------------------------------#
    #                             Visitor Methods                             #
    #-------------------------------------------------------------------------#
    accept = function(visitor) {
      visitor$visitDocumentCollection(self)
    },

    acceptArchive = function(visitor, stateId) {
      visitor$visitDocumentCollection(stateId, self)
    },

    acceptRestore = function(visitor, stateId) {
      visitor$visitDocumentCollection(stateId, self)
    }
  )
)
