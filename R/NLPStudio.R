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
#' \strong{NLPStudio Core Methods:}
#' \describe{
#'  \item{\code{new()}}{Initializes the NLPStudio. This is a singleton class in which its only object is created when the package is loaded. The object instantiated at package load time is called "nlpStudio".}
#'  \item{\code{getInstance()}}{Returns the current NLPStudio instance object. This will be the only instantiation called "nlpStudio.}
#'  \item{\code{getObject()}}{Returns the meta data and current NLPStudio object.}
#' }
#'
#' \strong{NLPSTudio Lab Methods:}
#' \describe{
#'  \item{\code{getChildren()}}{Returns the list of member labs in the nlpStudio object.}
#'  \item{\code{addChild(lab, enter = TRUE)}}{Adds an existing lab to the NLPStudio object list of labs.  If enter is set to TRUE, the currentLab and currentLabName variables are updated accordingly.}
#'  \item{\code{removeChild(lab, purge = FALSE)}}{Method which archives and removes the lab from the nlpStudio objectd. If purge is set to TRUE, the lab is removed from memory, disk, and state.}
#' }
#'
#' \strong{NLPSTudio State Methods:}
#' \describe{
#'  \item{\code{saveState()}}{Method for saving the current state of an NLPObject and its descendants.}
#'  \item{\code{restoreState()}}{Method for restoring an NLPStudio object to a prior state as designated by a state id.}
#' }
#'
#' \strong{NLPSTudio Visitor Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Method for accepting the visitor objects. Subclasses override these methods.}
#'   \item{\code{acceptVUpdate(visitor, object)}}{Accepts an object of the VUpdate class.}
#'  }
#'
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
          ..stateId = character(0),
          ..stateDesc = character(0),
          ..created = "None",
          ..modified = "None",

          initLog = function() {

            c <- Constants$new()
            logPath <- c$getLogPath()
            if (!dir.exists(logPath)) {
              dir.create(logPath)
            }
            futile.logger::flog.threshold(INFO)
            futile.logger::flog.logger("green", INFO, appender=appender.tee(file.path(logPath, "green.log")))
            futile.logger::flog.logger("yellow", WARN, appender=appender.tee(file.path(logPath, "yellow.log")))
            futile.logger::flog.logger("red", ERROR, appender=appender.tee(file.path(logPath, "red.log")))

            futile.logger::flog.info("Welcome to the NLPStudio package", name = 'green')
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

            # Create Directories
            c <- Constants$new()
            paths <- c$getPaths()
            lapply(paths, function(p) {
              if (!dir.exists(p))  dir.create(p, recursive = TRUE)
            })

            # Initialize System Logger
            private$initLog()

            # Create single instance of NLPStudio object
            private$..name <- name
            private$..desc <- desc
            private$..stateDesc <- paste("NLPStudio object", name, "instantiated at", Sys.time())
            private$..modified <- Sys.time()
            private$..created <- Sys.time()

            # Log Event
            #historian$addEvent(class = "NLPStudio", objectName = name,
            #                   method = "initialize", event = private$..stateDesc)

            invisible(self)
          },

          getInstance = function()  invisible(self),

          getObject = function() {

            studio = list(
              name = private$..name,
              desc = private$..desc,
              labs = private$..labs,
              stateId = private$..stateId,
              stateDesc = private$..stateDesc,
              modified = private$..modified,
              created = private$..created
              )

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

            # Get lab information
            l <- lab$getObject()

            # Save Memento
             private$..stateDesc <- paste("NLPStudio object", private$..name,
                                          "memento, prior to adding", l$name, "at", Sys.time())
            # private$..saveState()

            # Add lab to lab list
            private$..labs[[l$name]] <- lab

            # Add parent to lab
            lab$setAncestor(self)

            # Update modified time
            private$..modified <- Sys.time()

            # # Update State
             private$..stateDesc <- paste("Lab", l$name, "added to nlpStudio.")
            # private$..saveState()

            # Log Event
            # historian$addEvent(class = "NLPStudio", objectName = "nlpStudio",
            #                    method = "addChild",
            #                    event = private$..stateDesc)

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

            # Obtain lab meta data
            l <- lab$getObject()

            # Save State
            private$..stateDesc <- paste("Lab", l$name, "memento prior to removing", l$name, "from", private$..name)
            # private$..saveState()

            # Set child ancester to NULL
            lab$setAncestor()

            # Remove lab from nlpStudio
            private$..labs[[l$name]] <- NULL

            # Update modified time
            private$..modified <- Sys.time()

            # Save state
            private$..stateDesc <- paste("Lab", l$name, "removed from nlpStudio.")
            # private$..saveState()

            # historian$addEvent(class = "NLPStudio", objectName = "nlpStudio",
            #                    method = "removeChild",
            #                    event = private$..stateDesc)

          },

          #-------------------------------------------------------------------#
          #                           Lab Methods                             #
          #-------------------------------------------------------------------#
          saveState = function() {
            state <- State$new()
            private$..stateId <- state$save(self)
          },

          restoreState = function() {
            private$..stateId <- stateId
            state <- State$new()
            state$restore(self)
            invisible(self)
          }
        )
      )
      super$initialize(...)
    }
  )
)#$new()
