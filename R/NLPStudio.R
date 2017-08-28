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
#' instance.
#'
#' @field nlpStudio: The name of the single NLPStudio object created at load time.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Initializes the NLPStudio. This is a singleton class in which its only object is created when the package is loaded.}
#'  \item{\code{getStudio()}}{Retrieves current studio object, returns in list format and prints to console}
#'  \item{\code{addLab(lab, current)}}{Adds an existing lab to the NLPStudio object list of labs.  If current is set to TRUE, it is also assigned to the 'currentLab' variable.}
#'  \item{\code{removeLab()}}{Method which archives the lab and removes it from the global environment and from cache.}
#'  \item{\code{listlabs()}}{Returns a data frame of labs in the NLPStudio and prints them to console}
#' }
#'
#' @section Parameters:
#' @param name A character string containing the studio name
#' @param desc A character string containing the studio description
#' @param current A logical indicating whether to set the lab current.
#' @param lab An object of class 'Lab'
#'
#' @section Active Bindings:
#' \describe{
#'  \item{\code{desc()}}{An active binding that gets and sets the NLPStudio description}
#'  \item{\code{current()}}{An active binding that gets and sets the current lab. With the exception of the cross-lab classes, all objects act on the current lab. }
#' }
#'
#' @docType class
#' @examples
#' \dontrun{
#' nlpStudio$getStudio()
#' nlpStudio$desc <- "Homer's NLPStudio"  # Changes the description of the studio
#' nlpStudio$addLab("Lisa", current = FALSE)
#' nlpStudio$addLab("Bart", current = TRUE)
#' nlpStudio$removeLab("Bart") # Fails: Cannot remove a current lab
#' nlpStudio$currentLab <- "Lisa"
#' nlpStudio$removeLab("Bart") # Success!
#' nlpStudio$listlabs()  # Renders a list of labs to console and to data frame
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
          ..desc = "NLPStudio: Natural Language Processing Studio",
          ..studioDirs = list(
            studio = "./NLPStudio",
            archives = "./NLPStudio/Archives",
            labs = "./NLPStudio/Labs",
            log = "./NLPStudio/Log"
            ),
          ..labs = list(),
          ..currentLab = "None",
          ..created = "None",
          ..modified = "None"
        ),

        public = list(

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

            if ("Lab" %in% class(private$..currentLab)) {
              lab <- private$..currentLab
              lab <- lab$getLab(type = "list")
              labName <- lab$metaData$name
            } else {
              labName <- "None"
            }

            if (type == "object") {
              studio <- self
            } else if (type == "list") {
              studio = list(
                metaData = list(
                  name = private$..name,
                  desc = private$..desc,
                  currentLab = labName,
                  modified = private$..modified,
                  created = private$..created
                ),
                labs = self$getLabs(type = 'list')
              )
            } else if (type == "df") {
              studio = list(
                metaData = data.frame(name = private$..name,
                                      desc = private$..desc,
                                      currentLab = labName,
                                      created = private$..created,
                                      modified = private$..modified,
                                      stringsAsFactors = FALSE),
                labs = self$getLabs(type = "df")
              )
            } else {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "getLabs",
                       fieldName = "type", value = type, level = "Error",
                       msg = paste("Type", type, "is not a valid type.",
                                   "Valid types include 'object', 'list', and 'df'.",
                                   "See ?NLPStudio"),
                       expect = NULL)
              stop()
            }
            return(studio)
          },

          getLabs = function(type = "list") {

            if (type == "object") {
              labs = lapply(private$..labs, function(l) l)
            } else if (type == "list") {
              labs = lapply(private$..labs, function(l) {
                lab <- l$getLab(type = "list")
                lab$metaData

              })
            } else if (type == "df") {
              labs = rbindlist(lapply(private$..labs, function(l) {
                lab <- l$getLab(type = "list")
                lab$metaData
              }))
            } else {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "getLabs",
                       fieldName = "type", value = type, level = "Error",
                       msg = paste("Type", type, "is not a valid type.",
                                   "Valid types include 'object', 'list', and 'df'.",
                                   "See ?NLPStudio"),
                       expect = NULL)
              stop()
            }
            return(labs)
          },

          printStudio = function() {

            studio <- nlpStudio$getStudio(type = "df")

            cat("\n\n================================================================================",
                "\nNLPStudio: ")
            print.data.frame(studio$studioDf)

            cat("\n---------------------------------------------------------------------------------",
                "\nLabs:\n")
            print.data.frame(studio$labsDf)
            cat("\n================================================================================\n")

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

            # Update enter ...
            if (enter == TRUE) private$..currentLab <- lab

            # Update Cache
            nlpStudioCache$setCache("nlpStudio", self)

            invisible(self)

          },

          removeLab = function(name, purge = FALSE) {

            # Confirm name parameter is not missing
            if (missing(name)) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "removeLab",
                       fieldName = "name", value = "", level = "Error",
                       msg = paste("Name of lab is missing with no default.",
                                   "See ?NLPStudio for further assistance."),
                       expect = TRUE)
              stop()
            }

            # Confirm lab exists
            if (!exists(name, envir = .GlobalEnv)) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "removeLab",
                       fieldName = "name", value = name, level = "Error",
                       msg = paste("Lab does not exist.",
                                   "See ?NLPStudio for further assistance."),
                       expect = TRUE)
              stop()
            }

            # Get Lab
            lab <- get(name, envir = .GlobalEnv)

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
                       fieldName = "name", value = name, level = "Error",
                       msg = "Unable to remove a current lab.  See ?NLPStudio",
                       expect = NULL)
              stop()
            }

            # Archive lab
            nlpArchives$archive(lab)

            # Remove lab from nlpStudio
            private$..labs[[name]] <- NULL
            private$..modified <- Sys.time()
            nlpStudioCache$setCache(private$..name, self)

            # Remove from  memory and disc if purge == TRUE
            if (purge == TRUE) {

              # Get document information
              lab <- lab$getLab(type = "list")

              # Remove from disc
              base::unlink(lab$metaData$path, recursive = TRUE)

              # Remove from global environment
              rm(list = ls(envir = .GlobalEnv)[grep(name,ls(envir = .GlobalEnv))], envir = .GlobalEnv)

              # Remove from cache
              cache <- nlpStudioCache$loadCache()
              cache[[name]] <- NULL
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
              private$..modified <- Sys.time()
            }

            # Update Cache
            nlpStudioCache$setCache("nlpStudio", self)

            invisible(self)

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
