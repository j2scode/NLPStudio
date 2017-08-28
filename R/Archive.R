## ---- Archive
#==============================================================================#
#                                 Archive                                     #
#==============================================================================#
#' Archive
#'
#' \code{Archive} Abstract class for archiving objects in the NLPStudio
#'
#' This abstract class defines the methods used by the concrete classes for
#' archiving objects in the NLPStudio
#'
#' @docType class
#' @examples
#' \dontrun{
#' a <- archive$new()
#' a$archive0("development")
#' }
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of class Archive}
#'  \item{\code{archive(object)}}{Archives the object in the NLPStudio archive subdirectory }
#'  \item{\code{getArchives()}}{Returns the list of archives in object, list or data frame formats.  }
#'  \item{\code{printArchives()}}{Prints list of archives to console.}
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
          ..archives = list()
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
            private$..archives[[o$metaData$name]] <- list(
              name = o$metaData$name,
              class = class(object)[1],
              path = paste0(archiveFile, ".zip")
            )
            private$..modified <- Sys.time()
          },

          getArchive = function(type = "list") {

            if (type == "object") {
              archive <- self
            } else if (type == "list") {
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
            } else if (type == "df") {
              archive = list(
                metaData = data.frame(name = private$..name,
                                      desc = private$..desc,
                                      path = private$..path,
                                      modified = private$..modified,
                                      created = private$..created,
                                      stringsAsFactors = FALSE),
                archives = self$getArchives(type = "df")
              )
            } else {
              v <- Validate0$new()
              v$notify(cls = "Archive", method = "getArchive",
                       fieldName = "type", value = type, level = "Error",
                       msg = paste("Invalid type requested.",
                                   "Must be 'object', 'list', or 'df'.",
                                   "See ?Archive"),
                       expect = NULL)
              stop()
            }
            return(archive)
          },

          getArchives = function(type = "list") {

            if (type == "object") {
              archives = lapply(private$..archives, function(a) a)
            } else if (type == "list") {
              archives = lapply(private$..archives, function(a) a)
            } else if (type == "df") {
              archives = rbindlist(lapply(private$..archives, function(a) a))
            } else {
              v <- Validate0$new()
              v$notify(cls = "Archive", method = "getArchives",
                       fieldName = "type", value = type, level = "Error",
                       msg = paste("Invalid type requested.",
                                   "Must be 'object', 'list', or 'df'.",
                                   "See ?Archive"),
                       expect = NULL)
              stop()
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
          }
        )
      )
      super$initialize(...)
    }
  )
)
