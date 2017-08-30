#==============================================================================#
#                                   Document                                   #
#==============================================================================#
#' Document
#'
#' \code{Document} Class for instantiating,returning and printing individual
#' Document objects within objects of the DocumentCollection class.
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
#' @section Document Core Methods:
#' The core methods for the Document class are as follows:
#' \itemize{
#'  \item{\code{new(name, path, fileName, desc)}}{Method for instantiating
#'  an object of the Document class.}
#'  \item{\code{getDocument()}}{Method for retrieving the meta data for
#'  a document. }
#'  \item{\code{printDocument()}}{Method for printing the meta data for a
#'  document to console.}
#' }
#'
#' @section Composite Methods:
#' The composite methods implemented for this class:
#' \itemize{
#'  \item{\code{addDocument(document)}}{Returns the current object.}
#'  \item{\code{getDocuments()}}{Returns the current object.}
#'  \item{\code{removeDocument(document)}}{Not implemented for this class.}
#'  \item{\code{addPath(path)}}{Adds a value to the path variable for an object of the Document class.}
#' }
#'
#'
#' @section Input/Output Methods:
#' Methods for reading and writing individual document objects.
#' \itemize{
#'  \item{readDocument(what, how)}{Method for reading a document.}
#'  \item{writeDocument(what, how)}{Method for writing a document.}
#' }
#'
#' @param content A character vector or list containing text content for objects. Required for write methods.
#' @param desc Character string containing the description of the document.
#' @param document An object of the Document class.  Maintained as members of objects of the DocumentClass composite class.
#' @param fileName Character string cointaining the file name for a document.  Required for objects of Document class.
#' @param format  A character string indicating the document format to be read or written.  Valid values are c("bin", "text", "csv", "rdata"). Defaults to "text"
#' @param name Character string indicating the name of the document. Required for all objects
#' @param parent An object of the Lab or DocumentCollection class that represents the parent object. Required for input / output methods.
#' @param purge Logical indicating whether the removeDocument method should purge the document from the current environment. The default is FALSE
#' @param type = Character string indicating the type of object to be returned from get methods.  Valid values are c("object", "list", "df"). Defaults to "list".
#'
#' @field created A date time variable indicating the date / time the object was created.
#' @field documents A list of contained objects of the Document or DocumentCollection classes, maintained within the DocumentCollection class.
#' @field modified A date time variable indicating the date / time the object was modified.
#' @field parentTypes A list of valid types for parent objects.  These are initialized as "Lab", and "DocumentCollection". They may be overwritten by subclasses.
#' @field path Character string containing the relative directory path to the document. This is a concatenation of the parent's name (for Lab or DocumentCollection objects), or a file name for Document objects.
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
    initialize = function(name, parent, fileName, desc = NULL) {

      # Confirm required parameters are not missing.
      if (missing(name)) {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "initialize", fieldName = "name",
                 value = "", level = "Error", msg = "Name is required.",
                 expect = NULL)
        stop()
      }
      if (missing(fileName)) {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "initialize", fieldName = "fileName",
                 value = fileName, level = "Error", msg = "File name is required.",
                 expect = NULL)
        stop()
      }

      # Validate non-missing parameters
      v <- ValidateName$new()
      if (v$validate(cls = "Document", method = "initialize",
                     value = name, expect = FALSE) == FALSE) {
        stop()
      }

      if (!missing(parent)) {
        v <- ValidatePath$new()
       if (v$validate(cls = "Document", method = "initialize", fieldName = "path",
                   value = path, level = "Error", msg = "",
                   expect = private$..parentTypes) == FALSE) {
         stop()
       }
      }

      # Instantiate variables
      private$..desc <- desc
      private$..fileName <- fileName
      private$..name <- name
      if (!missing(parent)) {
        p <- getParent(parent)
        private$..parent <- parent
        private$..parentName <- p$name
        private$..path <- file.path(p$path, name)
      }
      private$..created <- Sys.time()
      private$..modified <- Sys.time()

      assign(name, self, envir = .GlobalEnv)
      nlpStudioCache$setCache(key = name, value = self)
      invisible(self)

    },

    getDocument = function(type = "list") {

      getObject <- function() {
        return(self)
      }

      getList <- function() {
        document <- list(
          metaData = list(
            name = private$..name,
            parent = private$..parentName,
            path = private$..path,
            fileName = private$..fileName,
            desc = private$..desc,
            modified = private$..modified,
            created = private$..created
          ),
          documents = list()
        )
        return(document)
      }

      getDf <- function() {
        document = list(
          metaData <- data.frame(name = private$..name,
                                 parent = private$..parentName,
                                 path = private$..path,
                                 fileName = private$..fileName,
                                 desc = private$..desc,
                                 modified = private$..modified,
                                 created = private$..created,
                                 stringsAsFactors = FALSE),
          documents = data.frame(0)
        )
        return(document)
      }

      if (format == "object") {document <- getObject()}
      else if (format == "list") {document <- getList()}
      else if (format == "df") {document <- getDf()}
      else {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "getDocument",
                 fieldName = "type", value = type, level = "Warn",
                 msg = paste("Invalid type requested.",
                             "Must be 'object', 'list', or 'df'.",
                             "Document returned in 'list' format.",
                             "See ?Document"),
                 expect = NULL)
        document <- getList()
      }
      return(document)
    },

    printDocument = function() {

      d <- self$getDocument(type = "df")

      cat("\n\n================================================================================",
          "\nDocument:")
      print.data.frame(d$documentDf)
      cat("\n================================================================================\n")

    },

    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#
    addDocument = function(document) {
      v <- Validate0$new()
      v$notify(cls = "Document", method = "addDocument",
                fieldName = "addDocument", value = "", level = "Warn",
                msg = paste("The addDocument method for the Document class",
                            "is not implemented for individual document objects.",
                            "See ?Document"),
                expect = NULL)
    },

    getDocuments = function(type = "list") {
      v <- Validate0$new()
      v$notify(cls = "Document", method = "getDocuments", fieldName = "",
               level = "Warn", value = "",
               msg = paste("The getDocuments method is not implemented",
                           "for the Document class.",
                           "See ?Document, and ?DocumentCollection",
                           "for further assistance."),
               expect = NULL)
    },

    removeDocument = function(name, purge = FALSE) {
      v$notify(cls = "Document", method = "removeDocument",
               fieldName = "removeDocument", value = "", level = "Error",
               msg = paste("The removeDocument method is not implemented for objects",
                           "of the Document class. There are two approaches",
                           "for removing individual documents. First, if the",
                           "document is part of an object of the DocumentCollection",
                           "class, the document may be removed using the removeDocument",
                           "method for the DocumentCollection object. To permanently",
                           "remove the object from current environment, set the 'purge'",
                           "parameter to TRUE when invoking the removeDocument method",
                           "on the DocumentCollection object. Second, if the object",
                           "does not belong to DocumentCollection object, you may",
                           "remove the object from the current environment using",
                           "the 'rm()' function. See ?Document or ?rm for further",
                           "assistance."),
               expect = NULL)
      stop()
    },

    addParent = function(parent) {

      if (class(parent) %in% c("DocumentCollection")) {
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
        v$notify(cls = "Document", method = "addParent",
                 fieldName = "parent", level = "Error", value = parent,
                 msg = paste("Unable to add parent object. Objects of the",
                             "Document class may only",
                             "have DocumentCollection",
                             "objects as parents"),
                 expect = "DocumentCollection")
        stop()
      }
    },

    getParent = function() {

      p <- private$..parent$getDocument(format = "list")

      return(p)
    },
    #-------------------------------------------------------------------------#
    #                                  I/O                                    #
    #-------------------------------------------------------------------------#
    readDocument = function(format = "text") {

      if (format %in% private$..format) {
        if (format == "bin") r <- ReadBin$new()
        if (format == "text") r <- ReadText$new()
        if (format == "txt") r <- ReadText$new()
        if (format == "csv") r <- ReadCsv$new()
        if (format == "RData") r <- ReadRdata$new()
        if (format == "rdata") r <- ReadRdata$new()
        if (format == "Rdata") r <- ReadRdata$new()
        private$..content <- r$readData(private$..path)
        return(private$..content)
      } else {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "readDocument", fieldName = "format",
                   level = "Error", value = format,
                   msg = paste("Format,", format, ",is not a valid document format.",
                               "See ?Document for assistance."))
        stop()
      }
    },

    writeDocument = function(format = "text", content) {

      if (missing(content)) {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "writeDocument", fieldName = "content",
                   level = "Error", value = content,
                   msg = paste("Unable to write content.",
                               "Content variable is missing with no default.",
                               "See ?Document for assistance."),
                 expect = NULL)
        stop()
      } else {
        v <- ValidateNotEmpty$new()
        if (v$validate(cls = "Document", method = "writeDocument", fieldName = "content",
                   level = "Error", value = content,
                   msg = paste("Unable to write content.  Content variable is empty.",
                               "See ?Document for assistance."),
                   expect = NULL) == FALSE) {
          stop()
        }
      }
      if (format %in% private$..format) {
        if (format == "bin") w <- WriteBin$new()
        if (format == "text") w <- WriteText$new()
        if (format == "txt") w <- WriteText$new()
        if (format == "csv") w <- WriteCsv$new()
        if (format == "RData") w <- WriteRdata$new()
        if (format == "rdata") w <- WriteRdata$new()
        if (format == "Rdata") w <- WriteRdata$new()
        private$..content <- w$writeData(private$..name, private$..path, content)
        return(private$..content)
      } else {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "writeDocument", fieldName = "format",
                 level = "Error", value = format,
                 msg = paste("Unable to write content, format,", format,
                             ",is not a valid document format.",
                             "See ?Document for assistance."))
        stop()
      }
    }
  )
)
