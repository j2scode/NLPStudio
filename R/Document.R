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

    ..fileName = character(0),
    ..path = character(0)
  ),

  active = list(

    path = function(value) {
      if (missing(value)) private$..path
      else private$...path <- value
    }
  ),

  public = list(

    #=========================================================================#
    #                           Core Methods                                  #
    #=========================================================================#
    initialize = function(name, desc, fileName) {

      v <- ValidationManager$new()
      v$validateName(cls = "Document", method = "initialize", name, expect = FALSE)

      if (missing(fileName)) {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "initialize", fieldName = "fileName",
                 value = fileName, level = "Error", msg = "File name is required.",
                 expect = NULL)
      }
      rm(v)

      private$..name <- name
      if (is.null(desc)) desc <- paste(name, "document")
      private$..desc <- desc
      private$..fileName <- fileName
      private$..created <- Sys.time()
      private$..modified <- Sys.time()

      assign(name, self, envir = .GlobalEnv)
      studioCacheManager$setCache(key = name, value = self)
      invisible(self)

    },

    getDocument = function(verbose = TRUE) {

      v <- ValidateLogical$new()
      v$validate(cls = "Document", method = "getDocument", fieldName = "verbose",
                 level = "Warn", value = verbose,
                 msg = "Verbose must be a logical.")
      rm(v)

      d <- data.frame(name = private$..name,
                      desc = private$..desc,
                      fileName = private$..fileName,
                      created = private$..created,
                      modified = private$..modified,
                      stringsAsFactors = FALSE
      )
      if (verbose == TRUE) {
        print.data.frame(d)
      }
      return(d)
    },

    readDocument = function(reader, labName, collectionName) {

      v <- ValidateNotEmpty$new()
      v$validate(cls = "Document", method = "readDocument", fieldName = "reader",
                 level = "Error", value = reader,
                 msg = paste("Unable to conduct read. Reader has not been set.",
                             "See ?Document for assistance."))

      v$validate(cls = "Document", method = "readDocument", fieldName = "labName",
                 level = "Error", value = labName,
                 msg = paste("Unable to conduct read. Lab name variable is empty.",
                             "See ?Document for assistance."))

      v$validate(cls = "Document", method = "readDocument", fieldName = "collectionName",
                 level = "Error", value = collectionName,
                 msg = paste("Unable to conduct read. Collection variable is empty.",
                             "See ?Document for assistance."))

      v$validate(cls = "Document", method = "readDocument", fieldName = "fileName",
                 level = "Error", value = private$..fileName,
                 msg = paste("Unable to conduct read. File name variable is empty.",
                             "See ?Document for assistance."))
      rm(v)

      r <- private$..reader
      return(r$readData(self))
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
    },

    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#
    addDocument = function(document) {
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
    #                          Behavior Methods                               #
    #-------------------------------------------------------------------------#
    getReader = function() private$..reader <- ReadText$new(),
    setReader = function(value) private$..reader <- value,
    getWriter = function() private$..reader <- WriteText$new(),
    setWriter = function(value) private$..writer <- value
  )
)
