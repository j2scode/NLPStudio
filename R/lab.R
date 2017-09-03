#==============================================================================#
#                                 Lab                                          #
#==============================================================================#
#' Lab
#'
#' \code{Lab} Class that contains document collections and the environment in which NLP happens.
#'
#' The environment in which NLP happens. There are two groups of methods. The
#' first group allows clients to instantiate, retrieve, print, enter, leave,
#' and archive a Lab object.  The second set of methods allow clients to retrieve
#' the contained documents, add a document, and remove a document.
#'
#' @section Lab Methods:
#' \describe{
#'  \item{\code{new(name, desc = NULL)}}{Creates an object of Lab Class}
#'  \item{\code{desc}}{A getter/setter method allowing clients to retrieve and set the Lab description variable.}
#'  \item{\code{getParent(type = object)}}{Method that returns the parent object in object. list, or data frame format. The default is object format.}
#'  \item{\code{getLab(type = "list")}}{Retrieves the meta data for the Lab, as well as a list of its document collections. The results can be returned in a variety of types (formats). Valid types are c("object", "list", "df"). The default is "list".}
#'  \item{\code{printLab()}}{Prints the meta data for the Lab object as well as a list of document collections in data frame format.}
#'  \item{\code{enterLab()}}{Sets a Lab object current in the NLPStudio.}
#'  \item{\code{leaveLab()}}{Sets the current Lab object in the NLPStudio to "none" .}
#'  \item{\code{archiveLab()}}{Archives a Lab object.}
#' }
#'
#' @section Document Methods:
#' \describe{
#'  \item{\code{getDocuments(type = "list")}}{Returns a list of documents in a range of formats.  Valid types (formats) are c("object", "list", "df").  The default is "list".}
#'  \item{\code{addDocument(document)}}{Adds a document to the Lab object's list of document collections.}
#'  \item{\code{removeDocument(document, purge = FALSE)}}{Removes a document from the Lab object's list of document collections.  If the purge variable is set to TRUE, the Lab object is archived, the document is removed from the global environment, its directory is deleted, and it is removed from state.}
#' }
#'
#' @param desc A chararacter string containing the description of the Lab
#' @param document An object of the DocumentCollection class to be added to the Lab object's list of document collections.
#' @param name A character string containing the name of the Lab object. This variable is used in the instantiation and remove methods.
#' @param purge A boolean variable.  Used in the removeDocument method. Indicates whether to purge a document from memory, disk, and state.
#' @param type A character string indicating the return format for the "get" methods.  Valid values are c("object", "list", "df")
#'
#' @field parent An object of class NLPStudio that contains the Lab object. This is always equal to the singleton object "nlpStudio".
#' @field parentName Character string indicating the name of the parent, which is always "nlpStudio".
#' @field path Character string indicating the directory path of the Lab object.
#' @field documents List of documents of class DocumentCollection that are members of the Lab object.
#' @field directories List of directories included in each lab object.
#' @field modified Datetime variable indicating the date/time the object was created.
#' @field created Datetime variable indicating the date/time the object was last modified.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Lab <- R6::R6Class(
  classname = "Lab",
  private = list(
    ..class = "Lab",
    ..name = character(0),
    ..desc = character(0),
    ..parent = character(0),
    ..parentName = "nlpStudio",
    ..path = character(0),
    ..documents = list(),
    ..directories = list(),
    ..modified = "None",
    ..created = "None"
  ),

  active = list(

    desc = function(value) {
      if (missing(value)) {
        private$..desc
      } else {
        private$..desc <- value
      }
      nlpStudioState$setState(private$..name, self)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                              Lab Methods                                #
    #-------------------------------------------------------------------------#

    initialize = function(name, desc = NULL) {

      # Load directories
      private$..directories <- nlpStudio$getDirectories()

      # Validate Name
      v <- ValidateName$new()
      if (v$validate(cls = "NLPStudio", method = "initialize", value = name,
                     expect = FALSE) == FALSE) {
        stop()
      }

      # Confirm lab does not already exist
      v <- ValidateExists$new()
      if (v$validate(cls = "Lab", method = "initialize",
                 fieldName = "name", value = name, level = "Error",
                 msg = paste("Cannot create lab because", name,
                             "already exists.",
                             "See ?Lab"),
                 expect = FALSE) == FALSE) {
        stop()
      }

      # Confirm directory does not exist
      v <- ValidatePath$new()
      if (v$validate(cls = "Lab", method = "initialize",
                 fieldName = "name",
                 value = file.path(private$..directories$labs, name),
                 level = "Error",
                 msg = paste("Cannot create lab because", name,
                             "directory already exists.",
                             "See ?Lab"),
                 expect = FALSE) == FALSE) {
        stop()
      }

      # Instantiate variables
      private$..name <- name
      if (is.null(desc)) { desc <- paste(name, "Lab") }
      private$..desc <- desc
      private$..parent <- nlpStudio
      private$..parentName <- "nlpStudio"
      private$..path <- file.path(private$..directories$labs, name)
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Create lab directory
      dir.create(file.path(private$..directories$labs, name))

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      # Add lab to NLPStudio
      nlpStudio$addLab(self)

      # Update State
      nlpStudioState$setState(name, self)

      invisible(self)
    },

    getParent = function(type = "object") {

      if (type == "object") {
        private$..parent
      } else if (type == "list") {
        private$..parent(getStudio(type = "list"))
      } else if (type == "df") {
        private$..parent(getStudio(type = "df"))
      } else {
        v <- Validate0$new()
        v$notify(cls = "Lab", method = "getLab",
                 fieldName = "type", value = type, level = "Warn",
                 msg = paste("Invalid format requested.",
                             "Must be 'object', 'list', or 'df'.",
                             "Parent is returned in 'list' format",
                             "See ?Lab"),
                 expect = NULL)
        private$..parent(getStudio(type = "list"))
      }
    },

    getLab = function(type = "list") {

      getObject <- function() {
        return(self)
      }

      getList <- function() {
        lab = list(
          metaData = list(
            name = private$..name,
            class = private$..class,
            desc = private$..desc,
            parentName = private$..parentName,
            path = private$..path,
            modified = private$..modified,
            created = private$..created
          ),
          documents = self$getDocuments(type = "list")
        )
        return(lab)
      }

      getDf <- function() {
        lab = list(
          metaData = data.frame(name = private$..name,
                                class = private$..class,
                                desc = private$..desc,
                                parentName = private$..parentName,
                                path = private$..path,
                                modified = private$..modified,
                                created = private$..created,
                                stringsAsFactors = FALSE),
          documents = self$getDocuments(type = "df")
        )
        return(lab)
      }

      if (type == "object") {lab <- getObject()}
      else if (type == "list") {lab <- getList()}
      else if (type == "df") {lab <- getDf()}
      else {
        v <- Validate0$new()
        v$notify(cls = "Lab", method = "getLab",
                 fieldName = "type", value = type, level = "Warn",
                 msg = paste("Invalid format requested.",
                             "Must be 'object', 'list', or 'df'.",
                             "Lab is returned in 'list' format",
                             "See ?Lab"),
                 expect = NULL)
        lab <- getList()
      }

      return(lab)
    },

    printLab = function() {

      lab <- self$getLab(type = "df")

      cat("\n\n================================================================================",
          "\n--------------------------------------Lab----------------------------------------")
      cat("\n                              Name:", lab$metaData$name)
      cat("\n                       Description:", lab$metaData$desc)
      cat("\n                            Parent:", lab$metaData$parentName)
      cat("\n                              Path:", lab$metaData$path)
      cat("\n                     Date Modified:", format(lab$metaData$modified))
      cat("\n                      Date Created:", format(lab$metaData$created))
      cat("\n================================================================================\n")

      cat("\n\n================================================================================")
      cat("\n-------------------------Document Collections(s)--------------------------------\n")
      print.data.frame(lab$documents)
      cat("\n================================================================================\n")
    },

    enterLab = function() {
      nlpStudio$enterLab(self)
    },

    leaveLab = function() {
      nlpStudio$leaveLab(self)
    },

    archiveLab = function() {

      a <- Archive$new()
      a$archive(self)

    },

    #-------------------------------------------------------------------------#
    #                         Document Methods                                #
    #-------------------------------------------------------------------------#

    getDocuments = function(type = "list") {

      getObject <- function() {
        documents = lapply(private$..documents, function(d) d)
        return(documents)
      }

      getList <- function() {
        documents = lapply(private$..documents, function(d) {
          document <- d$getDocument(type = "list")
          document$metaData
        })
        return(documents)
      }

      getDf <- function() {
        documents = rbindlist(lapply(private$..documents, function(d) {
          document <- d$getDocument(type = "list")
          document$metaData
        }))
        return(documents)
      }

      if (type == "object") {documents <- getObject() }
      else if (type == "list") {documents <- getList() }
      else if (type == "df") {documents <- getDf()}
      else {
        v <- Validate0$new()
        v$notify(cls = "Lab", method = "getDocuments",
                 fieldName = "format", value = format, level = "Warn",
                 msg = paste("Invalid format requested.",
                             "Must be 'object', 'list', or 'df'.",
                             "Documents returned in 'list' format.",
                             "See ?Lab"),
                 expect = NULL)
        documents <- getList()
      }
      return(documents)
    },

    addDocument = function(document) {

      # Validater
      v <- ValidateClass$new()
      if (v$validate(cls = "Lab", level = "Error", method = "addDocument",
                   fieldName = "document", value = document,
                   msg = paste("Invalid class. DocumentCollection class expected",
                               class(document), "encountered."),
                   expect = "DocumentCollection") == FALSE) {
        stop()
      }

      # Add collection to list of collections
      d <- document$getDocument(type = "list")
      private$..documents[[d$metaData$name]] <- document
      private$..modified <- Sys.time()

      # Add parent to document
      document$addParent(self)

      # Update State
      nlpStudioState$setState(private$..name, self)

      invisible(self)

    },

    removeDocument = function(document, purge = FALSE) {

      # Confirm name parameter is not missing
      if (missing(document)) {
        v <- Validate0$new()
        v$notify(cls = "Lab", method = "removeDocument",
                 fieldName = "document", value = "", level = "Error",
                 msg = paste("Document is missing with no default.",
                             "See ?Lab for further assistance."),
                 expect = TRUE)
        stop()
      }

      # Confirm document class
      v <- ValidateClass$new()
      if (v$validate(cls = "Lab", method = "removeDocument",
                     fieldName = "document", value = document, level = "Error",
                     msg = paste("Lab method is unable to remove document",
                                 "of the", class(document), "class.",
                                 "Lab methods can only remove DocumentCollection",
                                 "objects.", "See ?Lab for further assistance."),
                     expect = "DocumentCollection") == FALSE) {
        stop()
      }

      # Obtain document meta data
      documentInfo <- document$getDocument(type = "list")

      # Confirm object is not self
      if (private$..name == documentInfo$metaData$name) {
        v <- Validate0$new()
        v$notify(cls = "Lab", method = "removeDocument",
                 fieldName = "document", value = documentInfo$metaData$name,
                 level = "Error",
                 msg = paste("Object unable to remove itself. Removal must",
                             "be invoked from the parent object.",
                             "See ?Lab for further assistance"),
                 expect = NULL)
          stop()
      }

      # Archive before removing document
      nlpArchives$archive(document)

      # Remove collection from lab and update state
      private$..documents[[documentInfo$metaData$name]] <- NULL
      private$..modified <- Sys.time()
      nlpStudioState$setState(private$..name, self)

      if (purge == TRUE) {

        # Remove from disc
        base::unlink(documentInfo$metaData$path, recursive = TRUE)

        # Remove from global environment
        rm(list = ls(envir = .GlobalEnv)[grep(documentInfo$metaData$name,
                                              ls(envir = .GlobalEnv))],
           envir = .GlobalEnv)

      }

      # Update State
      nlpStudioState$setState(private$..name, self)

      invisible(self)

    }
  ), lock_objects = FALSE
)
