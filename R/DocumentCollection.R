#==============================================================================#
#                           DocumentCollection                                 #
#==============================================================================#
#' DocumentCollection
#'
#' \code{DocumentCollection} Class for managing composite document collections
#' in the NLPStudio
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
#' @section DocumentCollection Core Methods:
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
#' @param content A character vector or list containing text content for objects. Used in the I/O methods.
#' @param desc Character string containing the description of the document.
#' @param document An object of the Document class.  Used in DocumentClass composite methods
#' @param fileName Character string cointaining the file name for a document.  Only required for objects of Document class.
#' @param format  A character string indicating the document format to be read or written.  Valid values are c("bin", "text", "csv", "rdata"). Defaults to "text"
#' @param name Character string indicating the name of the document (required)
#' @param parent The parent object for an object of the DocumentCollection class. Parents may be objects of the DocumentCollection or Lab classes.
#' @param path Character string containing the relative directory path to the document.
#' @param purge Logical indicating whether the removeDocument method should purge the document from the current environment. The default is FALSE
#' @param type = Character string indicating the type of object to be returned from get methods.  Valid values are c("object", "list", "df"). Defaults to "list"
#'
#' @field created A date time variable indicating the date / time the object was created.
#' @field documents A list of objects of the Document class, maintained within the DocumentCollection class.
#' @field modified A date time variable indicating the date / time the object was modified.
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
    initialize = function(name, parent, desc = NULL) {

      # Validate Name
      v <- ValidateName$new()
      if (v$validate(cls = "DocumentCollection", method = "initialize",
                 value = name, expect = FALSE) == FALSE) {
        stop()
      }
      # Validate parent
      v <- ValidateClass$new()
      if (v$validate(cls = "DocumentCollection", method = "initialize",
                 fieldName = "parent",
                 value = parent, level = "Error",
                 msg = "Path is required.",
                 expect = private$..parentTypes) == FALSE) {
        stop()
      }

      # Instantiate variables
      private$..name <- name
      private$..path <- path
      private$..desc <- desc
      private$..created <-Sys.time()
      private$..modified <- Sys.time()

      # Create Directory
      dir.create(path)

      # Assign the name to the object in the global environment and update cache
      assign(name, self, envir = .GlobalEnv)
      nlpStudioCache$setCache(key = name, value = self)
      invisible(self)
    },

    getDocument = function(type = "list") {

      if (type == "object") {
        document <- self
      } else if (type == "list") {
        document = list(
          metaData = list(
            name = private$..name,
            parent = private$..parentName,
            path = private$..path,
            desc = private$..desc,
            modified = private$..modified,
            created = private$..created
          ),
          documents = self$getDocuments(type = "list")
        )
      } else if (type == "df") {
        document = list(
          metaData = data.frame(name = private$..name,
                                  parent = private$..parentName,
                                  path = private$..path,
                                  fileName = private$..fileName,
                                  desc = private$..desc,
                                  modified = private$..modified,
                                  created = private$..created,
                                  stringsAsFactors = FALSE),
          documents = self$getDocuments(type = "df")
        )
      } else {
        v <- Validate0$new()
        v$notify(cls = "DocumentCollection", method = "getDocument",
                 fieldName = "type", value = type, level = "Error",
                 msg = paste("Invalid type requested.",
                             "Must be 'object', 'list', or 'df'.",
                             "See ?DocumentCollection"),
                 expect = NULL)
        stop()
      }
      return(document)
    },


    printDocument = function() {

      d <- self$getDocument(type = "df")

      cat("\n\n#===============================================================================#")
      cat("\n                            DOCUMENT COLLECTION                                 \n")
      print.data.frame(d$documentDf)
      cat("\n#-------------------------------------------------------------------------------#")
      cat("\n                              DOCUMENTS                                        \n")
      print.data.frame(d$documentsDf)
      cat("\n#===============================================================================#\n\n")
    },

    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#
    addDocument = function(document) {

      # Validate document
      v <- ValidateClass$new()
      if (v$validate(cls = "DocumentCollection", method = "addDocument",
                 fieldName = "document", value = document, level = "Error",
                 msg = "Argument is not a Document class object.",
                 expect = "Document") == FALSE) {
        stop()
      }

      # Add document to list of documents for collection
      d <- document$getDocument(type = "list")
      private$..documents[[d$name]] <- document

      # Update the parent of the document object
      document$addParent(self)

      # Update cache
      nlpStudioCache$setCache(key = private$..name, value = self)
    },

    getDocuments = function(type = "list") {

      if (type == "object") {
        documents = lapply(private$..documents, function(d) d)
      } else if (type == "list") {
        documents = lapply(private$..documents, function(d) {
          document <- d$getDocument(type = "list")
          document$metaData
        })
      } else if (type == "df") {
        documents = rbindlist(lapply(private$..documents, function(d) {
          document <- d$getDocument(type = "list")
          document$metaData
        }))
      } else {
        v <- Validate0$new()
        v$notify(cls = "DocumentCollection", method = "getdocuments",
                 fieldName = "type", value = type, level = "Error",
                 msg = paste("Invalid type requested.",
                             "Must be 'object', 'list', or 'df'.",
                             "See ?DocumentCollection"),
                 expect = NULL)
        stop()
      }
      return(documents)
    },

    removeDocument = function(name, purge = FALSE) {

      # Confirm name parameter is not missing
      if (missing(name)) {
        v <- Validate0$new()
        v$notify(cls = "DocumentCollection", method = "removeDocument",
                 fieldName = "name", value = "", level = "Error",
                 msg = paste("Name of document is missing with no default.",
                             "See ?DocumentCollection for further assistance."),
                 expect = TRUE)
        stop()
      }

      # Confirm document exists
      if (!exists(name, envir = .GlobalEnv)) {
        v <- Validate0$new()
        v$notify(cls = "DocumentCollection", method = "removeDocument",
                 fieldName = "name", value = name, level = "Error",
                 msg = paste("Document does not exist.",
                             "See ?DocumentCollection for further assistance."),
                 expect = TRUE)
        stop()
      }

      # Get document
      d <- get(name, envir = .GlobalEnv)

      # Confirm the document is a document or collection
      classes <- c("Document", "DocumentCollection")
      v <- ValidateClass$new()
      if (v$validate(cls = "DocumentCollection", method = "removeDocument",
                 fieldName = "name", value = name, level = "Error",
                 msg = paste("The object named", name,
                             "is not a valid Document or DocumentCollection",
                             "object.",
                             "See ?DocumentCollection"),
                 expect = classes) == FALSE) {
        stop()
      }

      # Confirm document is not self
      if (name == private$..name) {
        v <- Validate0$new()
        v$notify(cls = "DocumentCollection", method = "removeDocument",
                 fieldName = "name", value = name, level = "Error",
                 msg = paste("The object named", name,
                             "cannot remove itself. Remove operations must",
                             "be performed by the parent object.",
                             "See ?DocumentCollection"),
                 expect = NULL)
        stop()
      }

      # Archive
      nlpArchives$archive(self)
      nlpArchives$archive(d)

      # Remove document from collection
      private$..documents[[name]] <- NULL
      private$..modified <- Sys.time()
      nlpStudioCache$setCache(private$..name, self)

      # Remove from  memory and disc if purge == TRUE
      if (purge == TRUE) {

        # Get document information
        d <- d$getDocument(format = "list")

        # Remove from disc
        file.remove(d$path)

        # Remove from global environment
        rm(list = ls(envir = .GlobalEnv)[grep(name,ls(envir = .GlobalEnv))], envir = .GlobalEnv)

        # Remove from cache
        cache <- nlpStudioCache$loadCache()
        cache[[name]] <- NULL
        nlpStudioCache$replaceCache(cache)
        nlpStudioCache$saveCache()

      }
    },

    addParent = function(parent) {

      if (class(parent) %in% c("DocumentCollection", "Lab")) {
        # Add parent
        private$..parent <- parent

        # Add parent name
        p <- getParent(format = "list")
        private$..parentName <- p$metaData$name

        # Update path
        private$..path <- file.path(p$metaData$path, private$..name)

        # Update cache
        nlpStudioCache$setCache(private$..name, self)

      } else {
        v <- ValidateClass$new()
        v$notify(cls = "DocumentCollection", method = "addParent",
                 fieldName = "parent", level = "Error", value = parent,
                 msg = paste("Unable to add parent object. Objects of the",
                             "DocumentCollection class may only",
                             "have DocumentCollection or Lab",
                             "objects as parents"),
                 expect = "DocumentCollection")
        stop()
      }
    },

    getParent = function() {

      if ("Lab" %in% class(private$..parent)) p <- private$..parent$getLab(format = "list")
      else p <- private$..parent$getDocument(format = "list")

      return(p)
    },

    #-------------------------------------------------------------------------#
    #                                  I/O                                    #
    #-------------------------------------------------------------------------#
    readDocument = function() {

      r <- private$..reader

      document <- lapply(private$..documents, function(d) {
        document = list(
          metaData = list(
            name = d$private$..name,
            desc = d$private$..desc,
            path = d$private$..path,
            fileName = d$private$..fileName),
          content = r$readData(d))
        document
      })
      return(document)
    },

    writeDocument = function(content) {

      w <- private$..writer

      lapply(private$..documents, function(d) {
        w$writeData(content[[d]])
      })
    }
  )
)
