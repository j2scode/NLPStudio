## ---- DocumentCollection
#==============================================================================#
#                            DocumentCollection                                #
#==============================================================================#
#' DocumentCollection
#'
#' \code{DocumentCollection} Composite class for creating and interacting with documents
#'
#' This compsite class provides data and methods for creating, interacting with,
#' and transforming documents and document collections. The methods fall
#' into three categories: (1), core methods for instantiating, printing,
#' reading and writing document collections. (2) methods to manage child
#' objects (documents) and (3) methods to set sourcing, reading and
#' writing behaviors. Note that document collections may include other
#' collections.
#'
#' @section Core Methods:
#' \describe{
#'  \item{\code{initialize()}}{Method for instantiating a document collection.}
#'  \item{\code{getDocument()}}{Method for retrieving and printing collection meta data.}
#'  \item{\code{readDocument()}}{Method for reading the document collection, using the read behavior strategy set below.}
#'  \item{\code{writeDocument()}}{Method for writing the document collection, using the write behavior strategy set below.}
#' }
#'
#' @section Composite Methods:
#' The composite methods control the document tree structure and include:
#' \describe{
#'  \item{\code{addDocument(document)}}{Method for adding a document to a collection.}
#'  \item{\code{searchDocuments(name)}}{Method for searching documents or collections within collections.}
#'  \item{\code{removeDocument(name)}}{Method for removing a document or collection from a collection.}
#'  \item{\code{listDocuments()}}{Method for obtaining and printing a list of documents in a collection.}
#' }
#'
#' @section Behavior Strategy Methods:
#' These methods enable clients to define behaviors for Register and Korpus class object at runtime.
#' \describe{
#'  \item{getReader}{Method for getting the current Read strategy for the object.}
#'  \item{setReader}{Method for setting the current Read strategy for the object.}
#'  \item{getWriter}{Method for getting the current Write strategy for the object.}
#'  \item{setWriter}{Method for setting the current Write strategy for the object.}
#' }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
DocumentCollection <- R6::R6Class(
  classname = "DocumentCollection",
  inherit = Document0,
  private = list(
    ..url = character(0)
  ),

  public = list(
    #-------------------------------------------------------------------------#
    #                            Core Methods                                 #
    #-------------------------------------------------------------------------#
    initialize = function(name, path, desc = NULL) {

      # Validate Name
      v <- ValidationManager$new()
      v$validateName(cls = "DocumentCollection", method = "initialize",
                     name, expect = FALSE)
      # Validate path
      v <- ValidatePath$new()
      v$validate(cls = "DocumentCollection", method = "initialize",
                 fieldName = "path",
                 value = path, level = "Error", msg = "Path is required.",
                 expect = FALSE)

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

    getDocument = function(format = "object") {

      if (format == "object") {
        document <- self
      } else if (format == "list") {
        document = list(
          name = private$..name,
          desc = private$..desc,
          path = private$..path,
          documents = self$getDocuments(format = "list"),
          modified = private$..modified,
          created = private$..created
        )
      } else if (format == "df") {
        document = list(
          documentDf = data.frame(name = private$..name,
                                    path = private$..path,
                                    desc = private$..desc,
                                    modified = private$..modified,
                                    created = private$..created,
                                    stringsAsFactors = FALSE),
          documentsDf = self$getDocuments(format = "df")
        )
      } else {
        v <- Validate0$new()
        v$notify(cls = "DocumentCollection", method = "getDocument",
                 fieldName = "format", value = format, level = "Error",
                 msg = paste("Invalid format requested.",
                             "Must be 'object', 'list', or 'df'.",
                             "See ?DocumentCollection"),
                 expect = NULL)
      }
      return(collection)
    },

    getDocuments = function(format = "object") {

      if (format == "object") {
        documents = lapply(private$..documents, function(d) d)
      } else if (format == "list") {
        documents = lapply(private$..documents, function(d) {
          d$getDocument(format = "list")
        })
      } else if (format == "df") {
        documents = rbindlist(lapply(private$..documents, function(d) {
          d$getDocument(format = "list")
        }))
      } else {
        v <- Validate0$new()
        v$notify(cls = "DocumentCollection", method = "getdocuments",
                 fieldName = "format", value = format, level = "Error",
                 msg = paste("Invalid format requested.",
                             "Must be 'object', 'list', or 'df'.",
                             "See ?DocumentCollection"),
                 expect = NULL)
      }
      return(documents)
    },

    printDocument = function() {

      document <- self$getDocument(format = "df")

        cat("\n\n#===============================================================================#")
        cat("\n                            DOCUMENT COLLECTION                                 \n")
        print.data.frame(document$collectionDf)
        cat("\n#-------------------------------------------------------------------------------#")
        cat("\n                              DOCUMENTS                                        \n")
        print.data.frame(document$documentsDf)
        cat("\n#===============================================================================#\n\n")
    },

    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#
    addDocument = function(document) {

      # Validate document
      v <- ValidateClass$new()
      v$validate(cls = "DocumentCollection", method = "addDocument",
                 fieldName = "document", value = document, level = "Error",
                 msg = "Argument is not a Document class object.",
                 expect = "Document")

      # Add document to list of documents for collection
      doc <- document$getDocument()
      private$..documents[[doc$name]] <- document

      # Update cache
      assign(private$..name, self, envir = .GlobalEnv)
      nlpStudioCache$setCache(key = private$..name, value = self)
    },

    removeDocument = function(name) {

      # TODO: Implement archive for environment
      private$..documents[[name]] <- NULL
      rm(name, envir = .GlobalEnv)
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
