#==============================================================================#
#                                   Document                                   #
#==============================================================================#
#' Document
#'
#' \code{Document} Class for instantiating,returning and printing individual
#' Document objects within objects of the DocumentCollection class.
#'
#' \strong{Document Class Overview:}
#'
#' The Document class is component part of the Document0 composite class. This
#' pattern is documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows composite
#' and individual objects to be treated uniformly.
#'
#' \strong{Document Core Methods:}
#' The core methods are as follows:
#' \itemize{
#'  \item{\code{new(name, fileName, desc)}}{Instantiates an object of the Document class. }
#'  \item{\code{getDocument()}}{Returns the object and its meta data.}
#'  \item{\code{accept(visitor)}}{Accepts a visitor and dispatches the visitor, passing 'self' as a parameter}
#'  \item{\code{addContent(content)}}{Method for adding content to an object of the Document class.}
#'  \item{\code{removeContent()}}{Method for removing content from an object of the Document class.}
#' }
#' \strong{Document Composite Methods:}
#' The composite methods are not implemented for the Document class.
#' \itemize{
#'  \item{\code{addDocument(document)}}{Not implemented for the Document class.}
#'  \item{\code{getDocuments()}}{Not implemented for the Document class.}
#'  \item{\code{removeDocument(document)}}{Not implemented for the Document class.}
#'  \item{\code{getAncestor()}}{Returns the parent object.}
#'  \item{\code{setAncestor(parent)}}{Sets the parent object.}
#' }
#'
#'#' \strong{Document I/O Methods:}
#' The following input/output methods are implemented for the Document class.
#' \itemize{
#'  \item{\code{readDocument(format)}}{Reads the document in the format designated in the format parameter.}
#'  \item{\code{writeDocument(format, content)}}{Writes the content to file in the format designated in the format parameter. }
#' }
#' @param content A character vector or list containing text content for objects. Required for write methods.
#' @param desc Character string containing the description of the document.
#' @param fileName Character string cointaining the file name for a document.  Required for objects of Document class.
#' @param name Character string indicating the name of the document. Required for all objects
#' @param parent An object of the Lab or DocumentCollection class that represents the parent object. Required for input / output methods.
#' @param purge Logical indicating whether the removeDocument method should purge the document from the current environment. The default is FALSE
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
    initialize = function(name, fileName, desc = NULL) {

      # Confirm required parameters are not missing.
      if (missing(name)) {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "initialize", fieldName = "name",
                 value = "", level = "Error",
                 msg = paste("Name parameter is missing with no default.",
                             "See ?Document for further assistance."),
                 expect = NULL)
        stop()
      }

      if (missing(fileName)) {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "initialize", fieldName = "fileName",
                 value = fileName, level = "Error",
                 msg = paste("File name parameter is missing with no default.",
                             "See ?Document for further assistance."),
                 expect = NULL)
        stop()
      }

      # Validate name
      v <- ValidateName$new()
      if (v$validate(cls = "Document", method = "initialize",
                     value = name, expect = FALSE) == FALSE) {
        stop()
      }

      # Get orphan collection information
      o <- orphanCollection$getObject()

      # Instantiate variables
      private$..name <- name
      private$..class <- "Document"
      private$..desc <- desc
      private$..parent <- orphanCollection
      private$..parentName <- o$name
      private$..path <- file.path(o$path, fileName)
      private$..fileName <- fileName
      private$..created <- Sys.time()
      private$..modified <- Sys.time()

      # Assign to object to name  in global environment
      assign(name, self, envir = .GlobalEnv)

      invisible(self)

    },

    getOjbect = function() {

      document <- list (
        name = private$..name,
        class = private$..class,
        desc = private$..desc,
        parent = private$..parent,
        parentName = private$..parentName,
        path = private$..path,
        fileName = private$..fileName,
        content = private$..content,
        created = private$..created,
        modified = private$..modified
      )

      # Update State
      nlpStudioState$saveState(private$..name, self)


      return(document)
    },

    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#
    addChild = function(document) {
      v <- Validate0$new()
      v$notify(cls = "Document", method = "addDocument",
                fieldName = "addDocument", value = "", level = "Warn",
                msg = paste("The addDocument method for the Document class",
                            "is not implemented for individual document objects.",
                            "See ?Document"),
                expect = NULL)
    },

    getChildren = function(type = "list") {
      v <- Validate0$new()
      v$notify(cls = "Document", method = "getDocuments", fieldName = "",
               level = "Warn", value = "",
               msg = paste("The getDocuments method is not implemented",
                           "for the Document class.",
                           "See ?Document, and ?DocumentCollection",
                           "for further assistance."),
               expect = NULL)
    },

    removeChild = function(name, purge = FALSE) {
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

    getAncestor = function() {

      p <- private$..parent

      return(p)
    },

    setAncestor = function(parent) {

      # Obtain old parent and path information
      oldParent <- self$getAncestor()
      oldParent <- oldparent$getObject()
      oldPath <- oldParent$path

      if (class(parent) %in% c("DocumentCollection")) {
        # Add parent
        private$..parent <- parent

        # Add parent name
        p <- parent$getObject()
        private$..parentName <- p$name

        # Update path
        private$..path <- file.path(p$path, private$..fileName)

        # Move file to new path
        file.copy(oldPath, private$..path)
        base::unlink(oldPath)

        # Update State
        nlpStudioState$saveState(private$..name, self)


      } else {
        v <- ValidateClass$new()
        v$notify(cls = "Document", method = "setAncestor",
                 fieldName = "parent", level = "Error", value = parent,
                 msg = paste("Unable to add parent object. Objects of the",
                             "Document class may only",
                             "have DocumentCollection",
                             "objects as parents.",
                             "See ?Document for further assistance."),
                 expect = "DocumentCollection")
        stop()
      }
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


      } else {
        v <- Validate0$new()
        v$notify(cls = "Document", method = "readDocument", fieldName = "format",
                 level = "Error", value = format,
                 msg = paste("Format,", format, ",is not a valid document format.",
                             "See ?Document for assistance."))
        stop()
      }
      # Update state
      # Update State
      nlpStudioState$saveState(private$..name, self)

      return(private$..content)
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
    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor) {
      visitor$visitDocument(self)
    },

    acceptArchive = function(visitor, stateId) {
      visitor$visitDocument(stateId, self)
    },

    acceptRestore = function(visitor, stateId) {
      visitor$visitDocument(stateId, self)
    }
  )
)
