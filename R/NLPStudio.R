## ---- NLPStudio
#==============================================================================#
#                                 NLPStudio                                    #
#==============================================================================#
#' NLPStudio
#'
#' \code{NLPStudio} Class the creates and manages labs.
#'
#' This class creates and manages data labs  A data lab is essentially a
#' directory in which project data reside.  Multiple data labs can be created
#' to house separate versions of the data for analysis. Note: This class is a
#' singleton pattern. An NLPStudio object called nlpStudio is instantiated at
#' load time.  Any subsequent initializations will return the single nlpStudio
#' instance. There are two sets of methods.  The first set enables clients to
#' retrieve information about the NLPStudio object.  The second set allows
#' clients to add, remove, enter, and leave labs.
#'
#' @section NLPStudio Object Methods:
#' \describe{
#'  \item{\code{new()}}{Initializes the NLPStudio. This is a singleton class in which its only object is created when the package is loaded. The object instantiated at package load time is called "nlpStudio".}
#'  \item{\code{getInstance()}}{Returns the current NLPStudio instance object. This will be the only instantiation called "nlpStudio.}#'
#'  \item{\code{getStudio(type = "list")}}{Returns the current NLPStudio in a variety of formats. Supported formats (types) include c("object", "list", "df"). The default type is "list".}
#'  \item{\code{printStudio()}}{Prints the nlpStudio object and its member objects of the Lab class to the console in data frame format.}
#' }
#'
#' @section Lab Methods:
#' \describe{
#'  \item{\code{getLabs(type = "list")}}{Returns the list of member labs in the nlpStudio object in a variety of formats(types).  Supported formats (types) include c("object", "list", "df"). The default type is "list".}
#'  \item{\code{addLab(lab, enter = FALSE)}}{Adds an existing lab to the NLPStudio object list of labs.  If enter is set to TRUE, the currentLab and currentLabName variables are updated accordingly.}
#'  \item{\code{removeLab(lab, purge = FALSE)}}{Method which archives and removes the lab from the nlpStudio objectd. If purge is set to TRUE, the lab is removed from memory, disk, and cache.}
#'  \item{\code{enterLab(lab)}}{Sets the currentLab and currentLabName to that of the lab object parameter.  The parameter must be an object of the Lab class.}
#'  \item{\code{leaveLab(lab)}}{Sets the currentLab and currentLabName to "None".}
#'  \item{\code{archiveLab(lab)}}{Archives a designated lab.}
#'  \item{\code{restoreLab(lab)}}{Restores a designated lab from archive.}
#'
#'
#' @param desc A character string containing the studio description
#' @param enter A logical indicating whether to enter a lab and to set the lab current.
#' @param lab An object of class 'Lab'.
#' @param labName Character string indicating the name of Lab object.
#' @param name A character string containing the studio name.
#' @param type A character string indicating the format in which the 'get' methods return results.
#'
#' @field studioDirs List containing the directories included in the NLPStudio.
#' @field labs List containing the member lab objects
#' @field currentLab An object of the Lab class. This is the currently active Lab object.
#' @field currentLabName Character string containing the name of the current lab.
#' @field created Datetime object indicating when the nlpStudio object was created.
#' @field modified Datetime object indicating when the nlpStudio object was last modified.
#'
#' @docType class
#' @examples
#' \dontrun{
#' nlpStudio$getStudio() # Returns nlpStudio meta data in list format
#' nlpStudio$addLab(Lisa, enter = FALSE) # Lisa must be an existing lab. See ?Lab.
#' nlpStudio$addLab(Bart, enter = TRUE) # Bart must be an existing lab See ?Lab.
#' nlpStudio$removeLab("Bart") # Fails: Cannot remove a current lab
#' nlpStudio$enterLab(Lisa)
#' nlpStudio$removeLab("Bart") # Success!
#' nlpStudio$getStudio()  # Renders a list of meta data and member labs.
#' nlpStudio$printStudio() # Prints meta data and member lab information in data frame format, to console.
#' }
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
NLPStudio <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "NLPStudio",
        private = list(
          ..name = "nlpStudio",
          ..class = "NLPStudio",
          ..desc = "NLPStudio: Natural Language Processing Studio",
          ..studioDirs = list(
            studio = "./NLPStudio",
            archives = "./NLPStudio/Archives",
            labs = "./NLPStudio/Labs",
            log = "./NLPStudio/Log"
            ),
          ..labs = list(),
          ..currentLab = "None",
          ..currentLabName = "None",
          ..created = "None",
          ..modified = "None"
        ),

        public = list(
          #-------------------------------------------------------------------#
          #                       NLPStudio Methods                           #
          #-------------------------------------------------------------------#

          initialize = function() {

            name <- "nlpStudio"
            desc <- "NLPStudio: Natural Language Processing Studio"

            # Suppress automatically generated error messages
            opt <- options(show.error.messages=FALSE, warn = -1)
            on.exit(options(opt))

            # Create NLPStudio folders
            lapply(private$..studioDirs, function(d) {
              suppressWarnings(dir.create(d))
            })

            # Create single instance of NLPStudio object
            private$..name <- name
            private$..desc <- desc
            private$..currentLab <- "None"
            private$..modified <- Sys.time()
            private$..created <- Sys.time()

            # Instantiate archive object
            nlpArchives <<- Archive$new()$getInstance()

            # Initialize Logger
            if (!dir.exists(private$..studioDirs$log)) {
              dir.create(private$..studioDirs$log)
              futile.logger::flog.threshold(INFO)
              futile.logger::flog.logger("green", INFO, appender=appender.tee('./log/green.log'))
              futile.logger::flog.logger("green", Info, appender=appender.tee('./log/green.log'))
              futile.logger::flog.logger("green", info, appender=appender.tee('./log/green.log'))
              futile.logger::flog.logger("yellow", WARN, appender=appender.tee('./log/yellow.log'))
              futile.logger::flog.logger("yellow", Warn, appender=appender.tee('./log/yellow.log'))
              futile.logger::flog.logger("yellow", warn, appender=appender.tee('./log/yellow.log'))
              futile.logger::flog.logger("red", ERROR, appender=appender.tee('./log/red.log'))
              futile.logger::flog.logger("red", Error, appender=appender.tee('./log/red.log'))
              futile.logger::flog.logger("red", error, appender=appender.tee('./log/red.log'))

              futile.logger::flog.info("Welcome to the NLPStudio package", name = 'green')
            }

            invisible(self)
          },

          getInstance = function() {
            invisible(self)
          },

          getStudio = function(type = "list") {

            getObject <- function() {
              return(self)
            }

            getList <- function() {
              studio = list(
                metaData = list(
                  name = private$..name,
                  class = private$..class,
                  desc = private$..desc,
                  currentLab = private$..currentLabName,
                  modified = private$..modified,
                  created = private$..created
                ),
                labs = self$getLabs(type = 'list')
              )
              return(studio)
            }

            getDf <- function() {
              studio = list(
                metaData = data.frame(name = private$..name,
                                      class = private$..class,
                                      desc = private$..desc,
                                      currentLab = private$..currentLabName,
                                      created = private$..created,
                                      modified = private$..modified,
                                      stringsAsFactors = FALSE),
                labs = self$getLabs(type = "df")
              )
              return(studio)
            }

            if (type == "object") {studio <- getObject() }
            else if (type == "list") {studio <- getList() }
            else if (type == "df") {studio <- getDf() }
            else {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "getLabs",
                       fieldName = "type", value = type, level = "Warn",
                       msg = paste("Type", type, "is not a valid type.",
                                   "Valid types include 'object', 'list', and 'df'.",
                                   "See ?NLPStudio"),
                       expect = NULL)
              studio <- getList()
            }
            return(studio)
          },

          printStudio = function() {

            studio <- nlpStudio$getStudio(type = "df")

            cat("\n\n================================================================================",
                "\n----------------------------------NLPStudio-------------------------------------\n")
            cat("\n                              Name:", studio$metaData$name)
            cat("\n             ",studio$metaData$desc)
            cat("\n                       Current Lab:", studio$metaData$currentLab)
            cat("\n                     Date Modified:", format(studio$metaData$modified))
            cat("\n                      Date Created:", format(studio$metaData$created), "\n")
            cat("\n================================================================================\n")

            if (length(studio$labs) > 0) {
              cat("\n\n================================================================================")
              cat("\n---------------------------------Lab(s)-----------------------------------------\n")
              print.data.frame(studio$labs)
              cat("\n================================================================================\n")
            }

          },

          #-------------------------------------------------------------------#
          #                           Lab Methods                             #
          #-------------------------------------------------------------------#
          getLabs = function(type = "list") {

            getObject <- function() {
              labs = lapply(private$..labs, function(l) l)
              return(labs)
            }

            getList <- function() {
              labs = lapply(private$..labs, function(l) {
                lab <- l$getLab(type = "list")
                lab$metaData

              })
              return(labs)
            }

            getDf <- function() {
              labs = rbindlist(lapply(private$..labs, function(l) {
                lab <- l$getLab(type = "list")
                lab$metaData
              }))
              return(labs)
            }

            if (type == "object") {labs <- getObject()}
            else if (type == "list") {labs <- getList()}
            else if (type == "df") {labs <- getDf()}
            else {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "getLabs",
                       fieldName = "type", value = type, level = "Warn",
                       msg = paste("Type", type, "is not a valid type.",
                                   "Valid types include 'object', 'list', and 'df'.",
                                   "See ?NLPStudio"),
                       expect = NULL)
              labs <- getList()
            }
            return(labs)
          },

          addLab = function(lab, enter = FALSE) {

            # Validation
            if (missing(lab)) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "addLab",
                         fieldName = "lab", value = "", level = "Error",
                         msg = paste("Unable to add lab.",
                                     "Variable lab is missing with no default."),
                         expect = TRUE)
              stop()
            }

            v <- ValidateClass$new()
            if (v$validate(cls = "NLPStudio", method = "addLab",
                           fieldName = "lab", value = lab, level = "Error",
                           msg = paste("Object is not a valid 'Lab' type."),
                           expect = "Lab") == FALSE) {
              stop()
            }

            v <- ValidateLogical$new()
            if (v$validate(cls = "NLPStudio", method = "addLab",
                           fieldName = "enter", value = enter, level = "Error",
                           msg = paste("Invalid logical,", enter, "must be TRUE or FALSE"),
                           expect = TRUE) == FALSE) {
              stop()
            }

            # Add lab to lab list
            l <- lab$getLab(type = "list")
            private$..labs[[l$metaData$name]] <- lab
            private$..modified <- Sys.time()

            # Update current lab
            if (enter == TRUE) {
              private$..currentLab <- lab
              private$..currentLabName <- l$metaData$name
            }

            # Update Cache
            nlpStudioCache$setCache("nlpStudio", self)

            invisible(self)

          },

          removeLab = function(lab, purge = FALSE) {

            # Confirm name parameter is not missing
            if (missing(lab)) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "removeLab",
                       fieldName = "lab", value = "", level = "Error",
                       msg = paste("Lab is missing with no default.",
                                   "See ?NLPStudio for further assistance."),
                       expect = TRUE)
              stop()
            }

            # Confirm lab is a lab
            v <- ValidateClass$new()
            if (v$validate(cls = "NLPStudio", method = "removeLab",
                           fieldName = "lab", value = lab, level = "Error",
                           msg = paste("Object is not a valid 'Lab' type.",
                                       "Encountered object of the",
                                       class(lab), "class.",
                                       "See ?NLPStudio for further assistance."),
                           expect = "Lab") == FALSE) {
              stop()
            }

            # Confirm lab is not current
            if (isTRUE(all.equal(lab, private$..currentLab))) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "removeLab",
                       fieldName = "lab", value = lab, level = "Error",
                       msg = "Unable to remove a current lab.  See ?NLPStudio",
                       expect = NULL)
              stop()
            }

            # Archive lab
            nlpArchives$archive(lab)

            # Obtain lab meta data
            labData <- lab$getLab(type = "list")

            # TODO: Cycle through collections, setting parent to "None"

            # Remove lab from nlpStudio and update the modified time.
            private$..labs[[lab$metaData$name]] <- NULL
            private$..modified <- Sys.time()
            nlpStudioCache$setCache(private$..name, self)

            # Remove from  memory and disc if purge == TRUE
            if (purge == TRUE) {

              base::unlink(labData$metaData$path, recursive = TRUE)

              # Remove from global environment
              rm(list = ls(envir =
                             .GlobalEnv)[grep(labData$metaData$name,
                                              ls(envir = .GlobalEnv))],
                 envir = .GlobalEnv)

              # Remove from cache
              cache <- nlpStudioCache$loadCache()
              cache[[labData$metaData$name]] <- NULL
              nlpStudioCache$replaceCache(cache)
              nlpStudioCache$saveCache()
            }
          },

          enterLab = function(lab) {

            labData <- lab$getLab(type = "list")

            if (isTRUE(all.equal(lab, private$..currentLab))) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "enterLab",
                       fieldName = "lab", value = labData$metaData$name, level = "Info",
                       msg = "Already entered lab.",
                       expect = NULL)
            } else {

              if (!isTRUE(all.equal(private$..currentLab, "None"))) {
                currentLab <- private$..currentLab$getLab("list")
                v <- Validate0$new()
                v$notify(cls = "NLPStudio", method = "enterLab",
                         fieldName = "lab", value = labData$metaData$name, level = "Info",
                         msg = paste("Leaving", currentLab$metaData$name, ". Entering", labData$metaData$name),
                         expect = NULL)
              }
              private$..currentLab <- lab
              private$..currentLabName <- labData$metaData$name
              private$..modified <- Sys.time()
            }


            # Update Cache
            nlpStudioCache$setCache("nlpStudio", self)

            invisible(self)

          },

          leaveLab = function(lab) {

            labData <- lab$getLab(type = "list")
            currentLab <- private$..currentLab$getLab(type = "list")

            if (!isTRUE(all.equal(lab, private$..currentLab))) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "leaveLab",
                       fieldName = "lab", value = labData$metaData$name, level = "Warn",
                       msg = paste("Unable to leave lab", labData$metaData$name,
                                   ". Current lab is", currentLab$metaData$name, "."),
                       expect = NULL)
            } else {
              private$..currentLab <- "None"
              private$..currentLabName <- "None"
              private$..modified <- Sys.time()
            }

            # Update Cache
            nlpStudioCache$setCache("nlpStudio", self)

            invisible(self)

          },

          archiveLab = function(lab) {
            lab$archiveLab()
          },

          restoreLab = function(labName) {
            #TODO: Complete
            a <- Archive$new()

          },

          getDirectories = function() {
            private$..studioDirs
          }
        )
      )
      super$initialize(...)
    }
  )
)#$new()
