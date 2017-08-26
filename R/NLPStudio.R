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
            archives = "./Archives",
            labs = "./Labs"),
          ..labs = list(),
          ..currentLab = "None",
          ..created = "None",
          ..modified = "None"
        ),

        active = list(

          desc = function(value) {
            if (missing(value)) {
              private$..desc
            } else {
            private$..desc <- value
            }
          }
        ),
        public = list(

          initialize = function() {

            name <- "nlpStudio"
            desc <- "NLPStudio: Natural Language Processing Studio"

            # Suppress automatically generated error messages
            opt <- options(show.error.messages=FALSE, warn = -1)
            on.exit(options(opt))

            # Create archive and lab folders
            suppressWarnings(dir.create(private$..studioDirs$archives))
            suppressWarnings(dir.create(private$..studioDirs$labs))

            # Create single instance of NLPStudio object
            private$..name <- name
            private$..desc <- desc
            private$..currentLab <- "None"
            private$..modified <- Sys.time()
            private$..created <- Sys.time()

            invisible(self)
          },

          getInstance = function() {
            invisible(self)
          },

          getStudio = function(format = "object") {

            if ("Lab" %in% class(private$..currentLab)) {
              lab <- private$..currentLab
              lab <- lab$getLab(format = "list")
              labName <- lab$name
            } else {
              labName <- "None"
            }

            if (format == "object") {
              studio <- self
            } else if (format == "list") {
              studio = list(
                name = private$..name,
                desc = private$..desc,
                currentLab = labName,
                labs = self$getLabs(format = 'list'),
                modified = private$..modified,
                created = private$..created
              )
            } else if (format == "df") {
              studio = list(
                studioDf = data.frame(name = private$..name,
                                      desc = private$..desc,
                                      currentLab = labName,
                                      created = private$..created,
                                      modified = private$..modified,
                                      stringsAsFactors = FALSE),
                labsDf = self$getLabs(format = "df")
              )
            } else {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "getLabs",
                       fieldName = "format", value = format, level = "Error",
                       msg = paste("Invalid format requested.",
                                   "Must be 'object', 'list', or 'df'.",
                                   "See ?NLPStudio"),
                       expect = NULL)
            }
            return(studio)
          },

          getLabs = function(format = "list") {

            if (format == "object") {
              labs = lapply(private$..labs, function(l) l)
            } else if (format == "list") {
              labs = lapply(private$..labs, function(l) {
                l$getLab(format = "list")
              })
            } else if (format == "df") {
              labs = rbindlist(lapply(private$..labs, function(l) {
                lab <- l$getLab(format = "list")
                labData = list(
                  name = lab$name,
                  desc = lab$desc,
                  modified = lab$modified,
                  created = lab$created
                )
                labData
              }))
            } else {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "getLabs",
                       fieldName = "format", value = format, level = "Error",
                       msg = paste("Invalid format requested.",
                                   "Must be 'object', 'list', or 'df'.",
                                   "See ?NLPStudio"),
                       expect = NULL)
            }
            return(labs)
          },

          printStudio = function() {

            studio <- nlpStudio$getStudio(format = "df")

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
            v <- ValidateClass$new()
            v$validate(cls = "NLPStudio", method = "addLab",
                       fieldName = "lab", value = lab, level = "Error",
                       msg = paste("Object is not a valid 'Lab' type."),
                       expect = "Lab")

            v <- ValidateLogical$new()
            v$validate(cls = "NLPStudio", method = "addLab",
                       fieldName = "enter", value = enter, level = "Error",
                       msg = paste("Invalid logical,", enter, "must be TRUE or FALSE"),
                       expect = TRUE)

            # Add lab to lab list
            labData <- lab$getLab(format = "list")
            private$..labs[[labData$name]] <- lab
            private$..modified <- Sys.time()

            # Update enter ...
            if (enter == TRUE) private$..currentLab <- lab

            # Update Cache
            nlpStudioCache$setCache("nlpStudio", self)

            invisible(self)

          },

          removeLab = function(lab) {

            # Confirm lab is a lab
            v <- ValidateClass$new()
            v$validate(cls = "NLPStudio", method = "removeLab",
                       fieldName = "lab", value = lab, level = "Error",
                       msg = paste("Object is not a valid 'Lab' type."),
                       expect = "Lab")

            labData <- lab$getLab(format = "list")

            # Confirm lab is not current
            if (isTRUE(all.equal(lab, private$..currentLab))) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "removeLab",
                       fieldName = "lab", value = labData$name, level = "Error",
                       msg = "Unable to remove a current lab.  See ?NLPStudio",
                       expect = "Lab")
            }

            # Archive lab
            lab$archiveLab()

            # Remove lab from nlpStudio
            private$..labs[[labData$name]] <- NULL
            private$..modified <- Sys.time()

            # Remove from global environment
            rm(list = ls(envir = .GlobalEnv)[grep(labData$name, ls(envir = .GlobalEnv))], envir = .GlobalEnv)

            # Remove from cache
            cache <- nlpStudioCache$loadCache()
            cache[[labData$name]] <- NULL
            nlpStudioCache$replaceCache(cache)
            nlpStudioCache$saveCache()
            rm(cache)

            # Update Cache
            nlpStudioCache$setCache("nlpStudio", self)

            invisible(self)

          },

          enterLab = function(lab) {

            labData <- lab$getLab(format = "list")

            if (isTRUE(all.equal(lab, private$..currentLab))) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "enterLab",
                       fieldName = "lab", value = labData$name, level = "Info",
                       msg = "Already entered lab.",
                       expect = NULL)
            } else {

              if (!isTRUE(all.equal(private$..currentLab, "None"))) {
                currentLab <- private$..currentLab$getLab("list")
                v <- Validate0$new()
                v$notify(cls = "NLPStudio", method = "enterLab",
                         fieldName = "lab", value = labData$name, level = "Info",
                         msg = paste("Leaving", currentLab$name, ". Entering", labData$name),
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

            labData <- lab$getLab(format = "list")
            currentLab <- private$..currentLab$getLab(format = "list")

            if (!isTRUE(all.equal(lab, private$..currentLab))) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "leaveLab",
                       fieldName = "lab", value = labData$name, level = "Warn",
                       msg = paste("Unable to leave lab", labData$name,
                                   ". Current lab is", currentLab$name, "."),
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
