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
#'  \item{\code{getInstance()}}{Returns the current NLPStudio instance object. This will be the only instantiation called "nlpStudio.}
#'  \item{\code{getObject()}}{Returns the meta data and current NLPStudio object.}
#' }
#'
#' @section Lab Methods:
#' \describe{
#'  \item{\code{getChildren()}}{Returns the list of member labs in the nlpStudio object.}
#'  \item{\code{addChild(lab, enter = TRUE)}}{Adds an existing lab to the NLPStudio object list of labs.  If enter is set to TRUE, the currentLab and currentLabName variables are updated accordingly.}
#'  \item{\code{removeChild(lab, purge = FALSE)}}{Method which archives and removes the lab from the nlpStudio objectd. If purge is set to TRUE, the lab is removed from memory, disk, and state.}
#' }
#'
#' @param lab An object of class 'Lab'.
#' @param autoSave Logical indicating whether to automatically save the state of an object after any change is made.
#'
#' @docType class
#' @examples
#' \dontrun{
#' nlpStudio$getObject() # Returns nlpStudio object and meta data in list format
#' Lab$new(name = "List", desc = "Lisa Simpon's lab")
#' Lab$new(name = "Bart", desc = "Bart Simpon's lab")
#' nlpStudio$addChild(Lisa, enter = FALSE) # Adds lab without setting it current.
#' nlpStudio$addChild(Bart, enter = TRUE) # Adds lab and sets it current.
#' nlpStudio$removeChild("Bart") # Success!
#' nlpStudio$getChildren()
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
          ..labs = list(),
          ..created = "None",
          ..modified = "None",

          initLog = function() {

            c <- Constants$new()
            logPath <- c$getLogPath()
            if (!dir.exists(logPath)) {
              dir.create(logPath)
              futile.logger::flog.threshold(INFO)
              futile.logger::flog.logger("green", INFO, appender=appender.tee(file.path(logPath, "green.log")))
              futile.logger::flog.logger("green", Info, appender=appender.tee(file.path(logPath, "green.log")))
              futile.logger::flog.logger("green", info, appender=appender.tee(file.path(logPath, "green.log")))
              futile.logger::flog.logger("yellow", WARN, appender=appender.tee(file.path(logPath, "yellow.log")))
              futile.logger::flog.logger("yellow", Warn, appender=appender.tee(file.path(logPath, "yellow.log")))
              futile.logger::flog.logger("yellow", warn, appender=appender.tee(file.path(logPath, "yellow.log")))
              futile.logger::flog.logger("yellow", WARNING, appender=appender.tee(file.path(logPath, "yellow.log")))
              futile.logger::flog.logger("yellow", Warning, appender=appender.tee(file.path(logPath, "yellow.log")))
              futile.logger::flog.logger("yellow", warning, appender=appender.tee(file.path(logPath, "yellow.log")))
              futile.logger::flog.logger("red", ERROR, appender=appender.tee(file.path(logPath, "red.log")))
              futile.logger::flog.logger("red", Error, appender=appender.tee(file.path(logPath, "red.log")))
              futile.logger::flog.logger("red", error, appender=appender.tee(file.path(logPath, "red.log")))

              futile.logger::flog.info("Welcome to the NLPStudio package", name = 'green')
            }
          }
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

            # Initialize System Logger
            private$initLog()

            # Create single instance of NLPStudio object
            private$..name <- name
            private$..desc <- desc
            private$..modified <- Sys.time()
            private$..created <- Sys.time()

            # Log Event
            historian$addEvent(class = "NLPStudio", objectName = "nlpStudio",
                               method = "initializes", event = "nlpStudio Initialized")

            invisible(self)
          },

          getInstance = function()  invisible(self),

          getObject = function() {

            studio = list(
              name = private$..name,
              desc = private$..desc,
              labs = private$..labs,
              modified = private$..modified,
              created = private$..created
              )

            # Update State
            stateManager$saveState(self)

            return(studio)
          },

          #-------------------------------------------------------------------#
          #                           Lab Methods                             #
          #-------------------------------------------------------------------#
          getChildren = function() private$..labs,

          addChild = function(lab) {

            # Validation
            if (missing(lab)) {
              v <- Validate0$new()
              v$notify(class = "NLPStudio", method = "addChild",
                         fieldName = "lab", value = "", level = "Error",
                         msg = paste("Unable to add lab.",
                                     "Variable lab is missing with no default.",
                                     "Please see ?NLPStudio for further assistance."),
                         expect = TRUE)
              stop()
            }

            v <- ValidateClass$new()
            if (v$validate(class = "NLPStudio", method = "addChild",
                           fieldName = "lab", value = lab, level = "Error",
                           msg = paste("Object is not a valid 'Lab' type.",
                                       "Please see ?NLPStudio for further assistance."),
                           expect = "Lab") == FALSE) {
              stop()
            }

            # Add lab to lab list
            l <- lab$getObject()
            private$..labs[[l$name]] <- lab

            # Add parent to lab
            lab$setAncestor(self)

            # Update modified time
            private$..modified <- Sys.time()

            # Update State
            stateNote <- paste("Lab", l$name, "added to nlpStudio.")
            stateManager$saveState(self, stateNote)

            # Log Event
            historian$addEvent(class = "NLPStudio", objectName = "nlpStudio",
                               method = "addChild",
                               event = paste("Added Lab,",
                                             l$name, "to nlpStudio."))

            invisible(self)

          },

          removeChild = function(lab) {

            # Confirm lab parameter is not missing
            if (missing(lab)) {
              v <- Validate0$new()
              v$notify(class = "NLPStudio", method = "removeChild",
                       fieldName = "lab", value = "", level = "Error",
                       msg = paste("Lab is missing with no default.",
                                   "See ?NLPStudio for further assistance."),
                       expect = TRUE)
              stop()
            }

            # Confirm lab is a lab
            v <- ValidateClass$new()
            if (v$validate(class = "NLPStudio", method = "removeChild",
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
              v$notify(class = "NLPStudio", method = "removeChild",
                       fieldName = "lab", value = lab, level = "Error",
                       msg = "Unable to remove a current lab.  See ?NLPStudio",
                       expect = NULL)
              stop()
            }

            # Obtain lab meta data
            l <- lab$getObject()

            # Save State
            stateNote <- paste("Lab", l$name, "pending removal from nlpStudio.")
            stateManager$saveState(self, stateNote)

            # Set child ancester to NULL
            lab$setAncestor(NULL)

            # Delete files and directory
            base::unlink(file.path(self$getPath(), l$name))

            # Remove lab from nlpStudio
            private$..labs[[l$name]] <- NULL

            # Update modified time
            private$..modified <- Sys.time()

            # Save state
            stateNote <- paste("Lab", l$name, "removed from nlpStudio.")
            stateManager$saveState(self, stateNote)

            historian$addEvent(class = "NLPStudio", objectName = "nlpStudio",
                               method = "removeChild",
                               event = paste("Removed Lab,",
                                             l$name, "from nlpStudio."))

          }

        )
      )
      super$initialize(...)
    }
  )
)#$new()
