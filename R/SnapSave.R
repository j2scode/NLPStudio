#==============================================================================#
#                                   SnapSave                                   #
#==============================================================================#
#' SnapSave
#'
#' \code{SnapSave} Class for saving snapshots of objects
#'
#' \strong{SnapSave Class Overview:}
#'
#' The SnapSave class manages the process of archiving and restoring objects
#' in the NLPStudio.
#'
#' \strong{Snap Family of Classes Participants:}
#' \itemize{
#'  \item Snap0: This class which manages and reports the saving and restoration of snapshots.
#'  \item SnapSave: Class which saves the snapshot to disk.
#'  \item SnapRestore: Class which restores a snapshot from disk.
#'  }
#'
#'
#' \strong{SnapSave Methods:}
#' \itemize{
#'  \item{\code{save(object)}}{Method that saves the snapshot of an object. A directory is created containing two compressed files: (1), the serialized object, and (2), the files in the object's directory.}
#' }
#'
#' @param snapName Character string indicating the unique name for the snapshot.
#' @param object Object of the "Corpus", "Document", "DocumentCollection", or "Lab" class to be archived.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Snapshot classes
#' @export
SnapSave <- R6::R6Class(
  classname = "SnapSave",
  public = list(
    save = function(name, object) {

      # Get archive directory
      dirs <- nlpStudio$getDirectories()

      # Validate parameter
      if (missing(name)) {
        v <- Validate0$new()
        v$notify(cls = "SnapSave", method = "save",
                 fieldName = "name", value = "", level = "Error",
                 msg = paste("Snapshot name is missing with no default.",
                             "See ?SnapSave for further assistance."),
                 expect = NULL)
        stop()
      }


      if (missing(object)) {
        v <- Validate0$new()
        v$notify(cls = "SnapSave", method = "save",
                 fieldName = "object", value = "", level = "Error",
                 msg = paste("Object is missing with no default.",
                             "See ?SnapSave for further assistance."),
                 expect = NULL)
        stop()
      }

      # Create Directory for Snapshot files
      snapDir <- file.path(dirs$snapshots, name)
      dir.create(snapDir)

      # Compress and SnapSave Files
      snapFile <- file.path(snapDir,
                            paste0(sub('\\..*', '',cls), "-class-object",
                                   "-snapshot-", name,
                                   format(Sys.time(),'-%Y%m%d-%H%M%S')))

      snapFiles <- list.files(path = o$metaData$path, all.files = TRUE, full.names = TRUE,
                          recursive = TRUE, include.dirs = TRUE)

      if (length(files) == 0) {
        archiveFile <- ""
      } else {
        zip(archiveFile, files)
      }

      # Add to list of archives
      archiveName <- paste0(objectName,"-",today,"-", seqNum)
      archiveFile <- paste0(archiveFile, ".zip")
      private$..archives[[archiveName]]$objectName <- objectName
      private$..archives[[archiveName]]$archiveName <- archiveName
      private$..archives[[archiveName]]$archiveFile <- archiveFile
      private$..archives[[archiveName]]$numFiles <- length(files)
      private$..archives[[archiveName]]$seqNum <- seqNum
      private$..archives[[archiveName]]$object <- as.environment(as.list(object, all.names = TRUE))
      private$..archives[[archiveName]]$created <- Sys.time()

      # Note date modified and store in state.
      private$..modified <- Sys.time()
      nlpStudioState$setState(key = private$..name, value = self)

      invisible(self)
    },

          getSnapSaves = function(objectName = "all", type = "list") {

            # Actually not an object but a list containing the archived object.
            getObject <- function(archives) {
              return(archives)
            }

            getList <- function(archives) {
              if (length(archives) == 0) {
                archives <- list()
              } else {
                archives <- lapply(archives, function(a) {
                  archives = list(
                    objectName = a$objectName,
                    archiveName = a$archiveName,
                    archiveFile = a$archiveFile,
                    numFiles = a$numFiles,
                    seqNum = a$seqNum,
                    created = a$created
                  )
                  archives
                })
              }
              return(archives)
            }

            getDf <- function(archives) {

              if (length(archives) == 0) {
                archives <- data.frame()
              } else {
                archives <- rbindlist(lapply(archives, function(a) {
                  archives = list(
                    objectName = a$objectName,
                    archiveName = a$archiveName,
                    archiveFile = a$archiveFile,
                    numFiles = a$numFiles,
                    seqNum = a$seqNum,
                    created = a$created
                  )
                  archives
                }))
              }

              return(archives)
            }

            # Search for object in archive
            archives <- self$searchSnapSaves(objectName)

            # Format archives
            if (type == "object") {archives <- getObject(archives)}
            else if (type == "list") {archives <- getList(archives)}
            else if (type == "df") {archives <- getDf(archives)}
            else {
              v <- Validate0$new()
              v$notify(cls = "SnapSave", method = "getSnapSaves",
                       fieldName = "type", value = type, level = "Warn",
                       msg = paste("Invalid type requested.",
                                   "Must be 'object', 'list', or 'df'.",
                                   "Returning SnapSave in 'list' format.",
                                   "See ?SnapSave for further assistance."),
                       expect = NULL)
              archives <- getList(archives)
            }
            return(archives)
          },

          printSnapSaves = function(objectName = "all") {

            archives <- self$getSnapSaves(objectName, type = "df")

            if (nrow(archives) == 0) {
              cat("\n\n================================================================================",
                  "\n-------------------------------SnapSave(s)----------------------------------------")
              cat("\n                              No SnapSaves.")
              cat("\n================================================================================\n")
            } else {

              cat("\n\n================================================================================",
                  "\n-------------------------------SnapSave(s)----------------------------------------\n")
              print.data.frame(archives)
              cat("\n================================================================================\n")
            }
          },

          searchSnapSaves = function(objectName) {

            if (length(private$..archives) == 0) {
              archives <- list()
            } else if (objectName == "all") {
              archives <- private$..archives
            } else {
              for (a in 1:length(private$..archives)) {
                if (private$..archives[[a]]$objectName == objectName) {
                  archives[[private$..archives[[a]]$archiveName]]$objectName <-
                    private$..archives[[a]]$objectName
                  archives[[private$..archives[[a]]$archiveName]]$archiveName <-
                    private$..archives[[a]]$archiveName
                  archives[[private$..archives[[a]]$archiveName]]$archiveFile <-
                    private$..archives[[a]]$archiveFile
                  archives[[private$..archives[[a]]$archiveName]]$numFiles <-
                    private$..archives[[a]]$numFiles
                  archives[[private$..archives[[a]]$archiveName]]$seqNum <-
                    private$..archives[[a]]$seqNum
                  archives[[private$..archives[[a]]$archiveName]]$created <-
                    private$..archives[[a]]$created
                } else {
                  archives <- list()
                }
              }
            }
            return(archives)
          },

          restore = function(archiveName) {

            # Confirm object name is not missing
            if (missing(archiveName)) {
              v <- Validate0$new()
              v$notify(cls = "SnapSave", method = "restore",
                       fieldName = "objectName", value = "", level = "Error",
                       msg = paste("The object name is a required field",
                                   "See ?SnapSave for further assistance."),
                       expect = NULL)
              stop()
            }

            # Confirm archiveName is a character string
            v <- ValidateClass$new()
            if (v$validate(cls = "SnapSave", method = "restore",
                     fieldName = "archiveName", value = archiveName, level = "Error",
                     msg = paste("The archiveName variable must be a character string.",
                                 "See ?SnapSave for further assistance."),
                     expect = "character") == FALSE) {
              stop()
            }

            # Confirm object has been archived.
            if (!exists(private$..archive[[archiveName]])) {
              v <- Validate0$new()
              v$notify(cls = "SnapSave", method = "restore",
                       fieldName = "archiveName", value = archiveName, level = "Error",
                       msg = paste("The archive", archiveName, "does not exist.",
                                   "See ?SnapSave for further assistance."),
                       expect = NULL)
              stop()
            }

            # Confirm parent exists
            object <- as.environment(as.list(private$..archives[[archiveName]]$object, all.names = TRUE))

            if (class(object)[1] == "Lab") {
              objectData <- object$getLab()
            } else if (class(object)[1] %in% c("Document", "DocumentCollection")) {
              objectData <- objectd$getDocument()
            } else if (class(objectd[1] %in% c("Corpus"))) {
              objectData <- getCorpus()
            }

            v <- ValidateExists$new()
            if (v$validate(cls = "SnapSave", method = "restore",
                           fieldName = "parent", value = objectData$metaData$parentName,
                           level = "Error",
                           msg = paste("Unable to restore object.
                                       Object parent does not exist.",
                                       "Restore", objectData$metaData$parentName,
                                       ", then restore", objectName,
                                       "See ?SnapSave for further assistance."),
                           expect = NULL) == FALSE) {
              stop()
            }

            # Restore files
            unzip(zipfile = objectData$metaData$archiveFile, overwrite = FALSE,
                  exdir = objectData$metaData$path, junkpaths = TRUE,
                  files = NULL)

            # Create object
            assign(objectData$metaData$name, object, envir = .GlobalEnv)

            # Note date modified and store in state.
            private$..modified <- Sys.time()
            nlpStudioState$setState(key = objectData$metaData$name, value = object)
            nlpStudioState$setState(key = private$..name, value = self)

            invisible(object)
          }
        )
      )
