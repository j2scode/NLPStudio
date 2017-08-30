## ---- Archive
#==============================================================================#
#                                 Archive                                      #
#==============================================================================#
#' Archive
#'
#' \code{Archive} Abstract class for archiving and restoring objects in the NLPStudio
#'
#' This singleton class has one instance, the 'nlpArchive', which is created at package
#' load time. Tbis class enables clients to archive and restore objectswithin the NLPStudio.
#'
#' @section Archive and Restore Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of class Archive}
#'  \item{\code{getInstance()}}{Returns the object 'nlpArchive' as an Archive class object.}
#'  \item{\code{getArchive()}}{Returns the Archive meta data in a variety of formats.  Supported formats include c("object", "list", "df",). The default format is 'list'.}
#'  \item{\code{getArchives()}}{Returns a list of Archived objects in a variety of formats.  Supported formats include c("object", "list", "df",). The default format is 'list'.}
#'  \item{\code{printArchives()}}{Prints the meta data and list of Archived objects to the console in data frame format.}
#'  \item{\code{archive(object)}}{Archives the object in the NLPStudio archive subdirectory }
#'  \item{\code{restore(objectName, parent = NULL)}}{Restores the object given by the name parameter. This recovers the directories and files, intantiates the object and, if the parent object is passed as a parameter, adds the object to the parent object.}
#' }
#'
#' @section Parameters:
#' @param object
#' \describe{
#'  \item{object$name}{Character string with name of the object}
#'  \item{object$desc}{Character string with description of the object e.g. "development-environment-archive"}
#'  \item{object$path}{Character string containing the directory containing the object}
#' }
#'
#' @docType class
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
Archive <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "Archive",

        private = list(
          ..name = "nlpArchive",
          ..desc = "Archive for NLPStudio Objects",
          ..path = character(0),
          ..modified = character(0),
          ..created = character(),
          ..archives = list(
            name = character(0),
            file = character(0),
            objName = character(0),
            objClass = character(0),
            objDesc = character(0),
            objParentName = character(0),
            objPath = character(0)

          )
        ),

        public = list(

          initialize = function() {
            dirs <- nlpStudio$getDirectories()
            private$..path <- dirs$archives
            private$..modified <- Sys.time()
            private$..created <- Sys.time()
          },

          getInstance = function() {
            invisible(self)
          },

          getArchive = function(type = "list") {

            getObject <- function() {
              return(self)
            }

            getList <- function() {
              archive = list(
                metaData = list(
                  name = private$..name,
                  desc = private$..desc,
                  path = private$..path,
                  modified = private$..modified,
                  created = private$..created
                ),
                archives = self$getArchives(type = "list")
              )
              return(archive)
            }

            getDf <- function() {
              archive = list(
                metaData = data.frame(name = private$..name,
                                      desc = private$..desc,
                                      path = private$..path,
                                      modified = private$..modified,
                                      created = private$..created,
                                      stringsAsFactors = FALSE),
                archives = self$getArchives(type = "df")
              )
              return(archive)
            }

            if (type == "object") {archive <- getObject()}
            else if (type == "list") {archive <- getList()}
            else if (type == "df") {archive <- getDf()}
            else {
              v <- Validate0$new()
              v$notify(cls = "Archive", method = "getArchive",
                       fieldName = "type", value = type, level = "Warn",
                       msg = paste("Invalid type requested.",
                                   "Must be 'object', 'list', or 'df'.",
                                   "Returning Archive in 'list' format.",
                                   "See ?Archive"),
                       expect = NULL)
              archive <- getList()
            }
            return(archive)
          },

          getArchives = function(type = "list") {

            getObject <- function() {
              archives = lapply(private$..archives, function(a) a)
              return(archives)
            }

            getList <- function() {
              archives = lapply(private$..archives, function(a) a)
              return(archives)
            }

            getDf <- function() {
              archives = rbindlist(lapply(private$..archives, function(a) a))
              return(archives)
            }

            if (type == "object") {archives <- getObject()}
            else if (type == "list") {archives = getList()}
            else if (type == "df") {archives = getDf()}
            else {
              v <- Validate0$new()
              v$notify(cls = "Archive", method = "getArchives",
                       fieldName = "type", value = type, level = "Warn",
                       msg = paste("Invalid type requested.",
                                   "Must be 'object', 'list', or 'df'.",
                                   "Returning Archives in 'list' format.",
                                   "See ?Archive"),
                       expect = NULL)
              archives <- getList()
            }
            return(archives)
          },

          printArchives = function() {

            archives <- self$getArchive(type = "df")

            cat("\n\n================================================================================",
                "\nArchive:")
            print.data.frame(archives$archiveDf)
            cat("\n--------------------------------------------------------------------------------",
                "\nArchives:")
            print.data.frame(archives$archivesDf)
            cat("\n================================================================================\n")
          },

          archive = function(object) {

            c <- class(object)[1]

            if (c == "Lab") {
              o <- object$getLab(type = "list")
            } else {
              o <- object$getDocument(type = "list")
            }

            dirs <- nlpStudio$getDirectories()

            archiveFile <- file.path(dirs$archives,
                                     paste0(sub('\\..*', '',c), "-object",
                                            "-archive-", o$metaData$name,
                                            format(Sys.time(),'-%Y%m%d-%H%M%S')))

            files <- list.files(path = o$metaData$path, all.files = TRUE, full.names = TRUE,
                                recursive = TRUE, include.dirs = TRUE)
            zip(archiveFile, files)

            # Add to list of archives
            o$metaData$archivePath <- paste0(archiveFile, ".zip")
            private$..archives[[o$metaData$name]] <- o
            private$..modified <- Sys.time()
          },

          restore = function(objectName) {

            # Confirm object name is not missing
            if (missing(objectName)) {
              v <- Validate0$new()
              v$notify(cls = "Archive", method = "restore",
                       fieldName = "objectName", value = "", level = "Error",
                       msg = paste("The object name is a required field",
                                   "See ?Archive for further assistance."),
                       expect = NULL)
              stop()
            }

            # Confirm object name is a character string
            v <- ValidateClass$new()
            if (v$validate(cls = "Archive", method = "restore",
                     fieldName = "objectName", value = "", level = "Error",
                     msg = paste("The object name muset be a character string.",
                                 "See ?Archive for further assistance."),
                     expect = "character") == FALSE) {
              stop()
            }

            # Confirm object has been archived.
            if (!exists(private$archives[[objectName]])) {
              v <- Validate0$new()
              v$notify(cls = "Archive", method = "restore",
                       fieldName = "objectName", value = objectName, level = "Error",
                       msg = paste("The object", objectName, "has not been archived.",
                                   "See ?Archive"),
                       expect = NULL)
              stop()
            }

            # Confirm parent exists
            objectData <- private$..archives[[objectName]]

            v <- ValidateExists$new()
            if (v$validate(cls = "Archive", method = "restore",
                           fieldName = "parent", value = objectData$metaData$parentName,
                           level = "Error",
                           msg = paste("Unable to restore object.
                                       Object parent does not exist.",
                                       "Restore", objectData$metaData$parentName,
                                       ", then restore", objectName,
                                       "See ?Archive for further assistance."),
                           expect = NULL) == FALSE) {
              stop()
            }

            # Restore files
            unzip(zipfile = objectData$metaData$archivePath, overwrite = FALSE,
                  exdir = objectData$metaData$path, junkpaths = TRUE,
                  files = NULL)

            # Create object


          }
        )
      )
      super$initialize(...)
    }
  )
)
