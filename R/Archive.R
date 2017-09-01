#==============================================================================#
#                                   Archive                                    #
#==============================================================================#
#' Archive
#'
#' \code{Archive} Class for archiving and restoring objects in the NLPStudio
#'
#' \strong{Archive Class Overview:}
#'
#' The Archive class manages the process of archiving and restoring objects
#' in the NLPStudio. An object of the Archive class is instantiated the first
#' time the package is loaded. Thereafter, the object is saved and recovered
#' from cache at load time. The Archive object maintains a list of objects that
#' were archived. The object files are compressed and stored in the
#' Archives sub directory.
#'
#' @section Archive Class Collaborators:
#' The collaborators of the Archive class are:
#' \itemize{
#'  \item Lab: Class in which NLP takes place. The class contains objects of the DocumentCollection class.
#'  \item Document0: Strategy class defining and encapsulating interfaces and algorithms for managing members of this composite class.
#'  \item DocumentCollection: Composite class containing objects of the Document class.
#'  \item Document: Leaf class of the Document objects.
#'  }
#'
#' @section Archive Methods:
#' The archive methods are as follows:
#' \itemize{
#'  \item{\code{new()}}{Instantiates an object of the Archive class.  This object is instantiated the first time the package is loaded.}
#'  \item{\code{archive(object)}}{Method that archives an object.  The object is stored in a list within the Archive object. The files are compressed and stored in the Archive subdirectory.}
#'  \item{\code{getArchives(name, type = "list")}}{Method for retrieving the meta data for a specific archive or a list of archives for the object matching the name parameter.}
#'  \item{\code{printArchives(name)}}{Method for printing to console the meta data for a specific archive or a list of archives for the object matching the name parameter.}
#'  \item{\code{restoreArchive(archiveName)}}{Method for restoring an object from archive. It requires the archive name in the name-YYYY-MM-DD-## format, where ## is a sequence number.}
#' }
#'
#'
#' @param archiveName Character string containing the name of the archive for an object in name-YYYY-MM-DD-## format.
#' @param object Object of the "Corpus", "Document", "DocumentCollection", or "Lab" class to be archived.
#' @param objectName String containing the name of the object archived.
#' @param type Character string indicating the format in which the getArchives method returns the archive information. Valid values are c("list" "df", "object"). The default is "list".
#'
#' @field archiveFileName Character string indicating the name of the archive containing the object's files.
#' @field created Date/time object indicating the date and time the archive was created.
#' @field seq Integer containing the sequence number for the archive of an object completed on a single day.
#'
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
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
          ..archives = list(),
          ..created = character(0),
          ..modified = character(0)
        ),

        public = list(

          initialize = function() {

            # Initialize path variable
            private$..created <- Sys.time()
            private$..modified <- Sys.time()

            # Assign name in global environment
            assign(private$..name, self, envir = .GlobalEnv)

            # Cache
            nlpStudioCache$setCache(key = private$..name, value = self)

            invisible(self)
          },

          getInstance = function() {
            invisible(self)
          },

          getArchives = function(objectName = "all", type = "list") {

            searchArchives <- function(objectName) {

              if (objectName == "all") {
                archives <- private$..archives
              } else {
                archives <- list()
                archives <- lapply(private$..archives, function(a) {
                  if (a$objectName == objectName) a
                })
                return(archives)
              }
            }

            # Actually not an object but a list containing the archived object.
            getObject <- function(archives) {
              return(archives)
            }

            getList <- function(archives) {
              archives <- lapply(archives, function(a) {
                archives = list(
                  archiveName = a$archiveName,
                  archiveFile = a$archiveFile,
                  objectName = a$objectName,
                  seqNum = a$seqNum,
                  created = a$created
                )
                archives
              })
              return(archives)
            }

            getDf <- function(archives) {
              archives <- rbindlist(lapply(archives, function(a) {
                archives = list(
                  archiveName = a$archiveName,
                  archiveFile = a$archiveFile,
                  objectName = a$objectName,
                  seqNum = a$seqNum,
                  created = a$created
                )
                archives
              }))
              return(archives)
            }

            # Search for object in archive
            archives <- searchArchives(objectName)

            if (!is.null(archives)) {

              # Format archives
              if (type == "object") {archives <- getObject(archives)}
              else if (type == "list") {archives <- getList(archives)}
              else if (type == "df") {archives <- getDf(archives)}
              else {
                v <- Validate0$new()
                v$notify(cls = "Archive", method = "getArchives",
                         fieldName = "type", value = type, level = "Warn",
                         msg = paste("Invalid type requested.",
                                     "Must be 'object', 'list', or 'df'.",
                                     "Returning Archive in 'list' format.",
                                     "See ?Archive for further assistance."),
                         expect = NULL)
                archives <- getList(archives)
              }
            }
            return(archives)
          },

          printArchives = function(objectName = "all") {

            archives <- self$getArchives(objectName, type = "df")

            if (nrow(archives) == 0) {
              cat("\n\n================================================================================",
                  "\n-------------------------------Archive(s)----------------------------------------")
              cat("\n                              No Archives.")
              cat("\n================================================================================\n")
            } else {

              cat("\n\n================================================================================",
                  "\n-------------------------------Archive(s)----------------------------------------\n")
              print.data.frame(archives)
              cat("\n================================================================================\n")
            }
          },

          archive = function(object) {

            # Get archive directory
            dirs <- nlpStudio$getDirectories()
            private$..path <- dirs$archives

            # Validate parameter
            if (missing(object)) {
              v <- Validate0$new()
              v$notify(cls = "Archive", method = "archive",
                       fieldName = "object", value = "", level = "Error",
                       msg = paste("Object is missing with no default.",
                                   "See ?Archive for further assistance."),
                       expect = NULL)
              stop()
            }

            # Validate class of parameter
            cls <- class(object)[1]
            if (!(cls %in% c("Lab", "Document", "DocumentCollection", "Corpus"))) {
              v <- Validate0$new()
              v$notify(cls = "Archive", method = "archive",
                       fieldName = "object", value = "", level = "Error",
                       msg = paste("Object must be a 'Lab', 'Document',",
                                   "'DocumentCollection', or 'Corpus' object",
                                   "See ?Archive for further assistance."),
                       expect = NULL)
              stop()
            }

            # Get object name
            if (cls == "Lab") {
              o <- object$getLab(type = "list")
            } else {
              o <- object$getDocument(type = "list")
            }
            objectName <- o$metaData$name

            # Format date
            today <-  as.Date(as.POSIXct(Sys.time(), format = "%m/%d/%Y %H:%M:%S", tz = "EDT"))

            # Get sequence number
            seqNum <- 1
            archives <- self$getArchives(objectName, type = "df")
            if (nrow(archives) > 0) {
              archives <- subset(archives, objectName == objectName & as.Date(created) == today)
              if (nrow(archives) > 0) {
                seqNum <- archives[which.max(archives$seqNum), archives$seqNum]
                seqNum <- seqNum + 1
              }
            }

            # Compress and Archive Files
            archiveFile <- file.path(private$..path,
                                     paste0(sub('\\..*', '',cls), "-class-object",
                                            "-archive-", objectName,
                                            format(Sys.time(),'-%Y%m%d-%H%M%S')))

            files <- list.files(path = o$metaData$path, all.files = TRUE, full.names = TRUE,
                                recursive = TRUE, include.dirs = TRUE)
            zip(archiveFile, files)

            # Add to list of archives
            archiveName <- paste0(objectName,"-",today,"-", seqNum)
            archiveFile <- paste0(archiveFile, ".zip")
            private$..archives[[archiveName]]$archiveName <- archiveName
            private$..archives[[archiveName]]$archiveFile <- archiveFile
            private$..archives[[archiveName]]$objectName <- objectName
            private$..archives[[archiveName]]$seqNum <- seqNum
            private$..archives[[archiveName]]$object <- as.environment(as.list(object, all.names = TRUE))
            private$..archives[[archiveName]]$created <- Sys.time()

            # Note date modified and store in cache.
            private$..modified <- Sys.time()
            nlpStudioCache$setCache(key = private$..name, value = self)

            invisible(self)
          },

          restore = function(archiveName) {

            # Confirm object name is not missing
            if (missing(archiveName)) {
              v <- Validate0$new()
              v$notify(cls = "Archive", method = "restore",
                       fieldName = "objectName", value = "", level = "Error",
                       msg = paste("The object name is a required field",
                                   "See ?Archive for further assistance."),
                       expect = NULL)
              stop()
            }

            # Confirm archiveName is a character string
            v <- ValidateClass$new()
            if (v$validate(cls = "Archive", method = "restore",
                     fieldName = "archiveName", value = archiveName, level = "Error",
                     msg = paste("The archiveName variable must be a character string.",
                                 "See ?Archive for further assistance."),
                     expect = "character") == FALSE) {
              stop()
            }

            # Confirm object has been archived.
            if (!exists(private$..archive[[archiveName]])) {
              v <- Validate0$new()
              v$notify(cls = "Archive", method = "restore",
                       fieldName = "archiveName", value = archiveName, level = "Error",
                       msg = paste("The archive", archiveName, "does not exist.",
                                   "See ?Archive for further assistance."),
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
            unzip(zipfile = objectData$metaData$archiveFile, overwrite = FALSE,
                  exdir = objectData$metaData$path, junkpaths = TRUE,
                  files = NULL)

            # Create object
            assign(objectData$metaData$name, object, envir = .GlobalEnv)

            # Note date modified and store in cache.
            private$..modified <- Sys.time()
            nlpStudioCache$setCache(key = objectData$metaData$name, value = object)
            nlpStudioCache$setCache(key = private$..name, value = self)

            invisible(object)
          }
        )
      )
      super$initialize(...)
    }
  )
)
