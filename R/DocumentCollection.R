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

      v <- ValidationManager$new()
      v$validateName(cls = "DocumentCollection", method = "initialize",
                     name, expect = FALSE)

      private$..name <- name
      private$..path <- path
      if (is.null(desc)) {
        desc <- paste(name, "collection")
      }
      private$..desc = desc
      private$..created <-Sys.time()
      private$..modified <- Sys.time()

      assign(name, self, envir = .GlobalEnv)
      nlpStudioCache$setCache(key = name, value = self)
      invisible(self)
    },

    getDocument = function(verbose = TRUE) {
      collection <- data.frame(name = private$..name,
                      desc = private$..desc,
                      created = private$..created,
                      modified = private$..modified,
                      stringsAsFactors = FALSE)

      documents <- rbindlist(lapply(private$..documents, function(x) {
        doc <- list(
          name = x$private$..name,
          desc = x$private$..desc,
          fileName = x$private$..fileName,
          created = x$private$..created,
          modified = x$private$..modified
        )
        doc
      }))

      if (verbose == TRUE) {
        cat("\n\n#===============================================================================#")
        cat("\n                            DOCUMENT COLLECTION                                 \n")
        print.data.frame(collection)
        cat("\n#-------------------------------------------------------------------------------#")
        cat("\n                              DOCUMENTS                                        \n")
        print.data.frame(documents)
        cat("\n#===============================================================================#\n\n")
      }

      document = list(collection = collection, documents = documents)
      return(document)
    },

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
    },


    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#
    addDocument = function(document) {

      # Add document to list of documents for collection
      if (length(private$..documents) == 0) {
        private$..documents <- list(document)
      } else {
        private$..documents <- list(private$..documents, list(document))
      }

      # Update cache
      assign(name, self, envir = .GlobalEnv)
      nlpStudioCache$setCache(key = name, value = self)
    },

    searchDocuments = function(name) {
      if (length(private$..documents) > 0) {
        for (i in 1:length(private$..documents)) {
          if (private$..documents[[i]]$name == name) { return(i) }
        }
        return(FALSE)
      } else {
        return(FALSE)
      }
    },

    removeDocument = function(name) {

      # TODO: Implement archive for environment
      docIdx <- searchDocuments(name)
      private$..documents[[docIdx]] <- NULL
      rm(name, envir = .GlobalEnv)
    },

    listDocuments = function(verbose = TRUE) {

      documents = lapply(private$..documents, function(d) {
        docs <- list(
          name = d[[1]]$private$..name,
          desc = d[[1]]$private$..desc,
          path = d[[1]]$private$..path,
          fileName = d[[1]]$private$..fileName,
          created = d[[1]]$private$..created,
          modified = d[[1]]$private$..modified
        )
        docs
      })

      if (verbose == TRUE) {
        print.data.frame(as.data.frame(documents))
      }
      return(documents)
    },

    #-------------------------------------------------------------------------#
    #                          Behavior Methods                               #
    #-------------------------------------------------------------------------#
    getReader = function() private$..reader <- ReadText$new(),
    setReader = function(value) private$..reader <- value,
    getWriter = function() private$..reader <- WriteText$new(),
    setWriter = function(value) private$..writer <- value

  )
)
