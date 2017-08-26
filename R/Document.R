#==============================================================================#
#                                   Document                                   #
#==============================================================================#
#'
#' Document
#'
#' \code{Document} Leaf class for instantiating, printing, reading and writing non-composite, leaf documents
#'
#' This leaf class contains the data and methods used to instantiate objects of
#' the Document class, print the document meta data and to read  and write
#' the document.
#'
#' @section  Core Methods:
#' The methods are as follows:
#' \describe{
#'  \item{\code{new(name, desc, fileName)}}{Method for instantiating an object of the Document class}
#'  \item{\code{getDocument()}}{Method for returning and printing the document object meta data.}
#'  \item{\code{readDocument(reader, labName, collectionName)}}{Method for reading the document using the designated reader.}
#'  \item{\code{writeDocument(writer, labName, collectionName)}}{Method for writing the document using the designated writer.}
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
#' @examples
#' \dontrun{
#' Document$new(name = "news", desc = "News register of Brown Corpus" fileName = "news.txt")
#' news$getDocument() # Returns the document meta data in data frame format and prints it to console.
#' news$getDocument(verbose = FALSE) Returns the document metea data in data frame format, but does not print to console.
#' news$readDocument(reader = readText, labName = "alphaLab", collectionName =  "brown")
#' }
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

  private = list(
    ..fileName = "",
    ..content = ""
  ),

  public = list(

    #=========================================================================#
    #                           Core Methods                                  #
    #=========================================================================#
    initialize = function(name, fileName, path, desc = NULL) {

      # Validate
      v <- ValidationManager$new()
      v$validateName(cls = "Document", method = "initialize", name, expect = FALSE)

      if (missing(fileName)) {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "initialize", fieldName = "fileName",
                 value = fileName, level = "Error", msg = "File name is required.",
                 expect = NULL)
      }

      if (missing(path)) {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "initialize", fieldName = "path",
                 value = path, level = "Error", msg = "Path is required.",
                 expect = NULL)
      }
      rm(v)

      # Instantiate variables
      private$..name <- name
      private$..desc <- desc
      private$..fileName <- fileName
      private$..path<- path
      private$..created <- Sys.time()
      private$..modified <- Sys.time()

      assign(name, self, envir = .GlobalEnv)
      nlpStudioCache$setCache(key = name, value = self)
      invisible(self)

    },

    getDocument = function(format = "object") {
      if (format == "object") {
        document <- self
      } else if (format == "list") {
        document <- list(
          name = private$..name,
          desc = private$..desc,
          path = private$..path,
          fileName = private$..fileName,
          modified = private$..modified,
          created = private$..created
        )
      } else if (format == "df") {
        document <- data.frame(name = private$..name,
                             desc = private$..desc,
                             path = private$.path,
                             fileName = private$..fileName,
                             modified = private$..modified,
                             created = private$..created,
                             stringsAsFactors = FALSE)
      } else {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "getDocument",
                 fieldName = "format", value = format, level = "Error",
                 msg = paste("Invalid format requested.",
                             "Must be 'object', 'list', or 'df'.",
                             "See ?NLPStudio"),
                 expect = NULL)
      }
      return(document)
    },

    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#
    addDocuments = function(document) {
      invisible(self)
    },

    searchDocuments = function(name) {
      invisible(self)
    },

    removeDocument = function(name) {
      invisible(self)
    },

    listDocuments = function(verbose = TRUE) {

      d <- data.table(name = private$..name,
                      desc = private$..desc,
                      fileName = private$..fileName,
                      path = private$..path,
                      created = private$..created,
                      modified = private$..modified)

      if (verbose == TRUE) {
        print.data.frame(d)
      }
      return(d)
    },

    #-------------------------------------------------------------------------#
    #                                  I/O                                    #
    #-------------------------------------------------------------------------#
    readDocument = function(reader) {

      v <- ValidateNotEmpty$new()
      v$validate(cls = "Document", method = "readDocument", fieldName = "reader",
                 level = "Error", value = reader,
                 msg = paste("Unable to conduct read. Reader has not been set.",
                             "See ?Document for assistance."))

      v$validate(cls = "Document", method = "readDocument", fieldName = "path",
                 level = "Error", value = private$..path,
                 msg = paste("Unable to conduct read. Document path is required.",
                             "See ?Document for assistance."))

      v$validate(cls = "Document", method = "readDocument", fieldName = "fileName",
                 level = "Error", value = private$..fileName,
                 msg = paste("Unable to conduct read. File name is required.",
                             "See ?Document for assistance."))
      rm(v)

      r <- private$..reader
      private$..content <- r$readData(self)
      return(private$..content)
    },

    writeDocument = function(content) {
      v <- ValidateNotEmpty$new()
      v$validate(cls = "Document", method = "writeDocument", fieldName = "writer",
                 level = "Error", value = private$..writer,
                 msg = paste("Unable to conduct write. Writer has not been set.",
                             "See ?Document0 for assistance."))

      v$validate(cls = "Document", method = "writeDocument", fieldName = "path",
                 level = "Error", value = private$..path,
                 msg = paste("Unable to conduct write. Path variable is empty.",
                             "See ?Document for assistance."))

      v$validate(cls = "Document", method = "writeDocument", fieldName = "fileName",
                 level = "Error", value = self$fileName,
                 msg = paste("Unable to conduct write. File name is empty.",
                             "See ?Document for assistance."))

      v$validate(cls = "Document", method = "writeDocument", fieldName = "content",
                 level = "Error", value = content,
                 msg = paste("Unable to conduct write. Content is empty.",
                             "See ?Document for assistance."))
      rm(v)
      w <- private$..writer
      w$writeData(self, content)
    }
  )
)
