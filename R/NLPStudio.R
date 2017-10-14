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
#'  \item{\code{getInstance()}}{Returns the current NLPStudio instance object. This will be the only instantiation called "nlpStudio.},
#'  \item{\code{getName()}}{Returns the name of the current NLPStudio object.}
#'  \item{\code{exposeObject(requester)}}{Returns the current object elements in list format if invoked by ah authorized requester.}
#'  \item{\code{restore(requester, prior)}}{Restores the object to a prior state of invoked by an authorized requester.}
#' }
#'
#' \strong{NLPSTudio Lab Methods:}
#' \describe{
#'  \item{\code{getChildren()}}{Returns the list of member labs in the nlpStudio object.}
#'  \item{\code{addChild(lab, enter = TRUE)}}{Adds an existing lab to the NLPStudio object list of labs.  If enter is set to TRUE, the currentLab and currentLabName variables are updated accordingly.}
#'  \item{\code{removeChild(lab, purge = FALSE)}}{Method which archives and removes the lab from the nlpStudio objectd. If purge is set to TRUE, the lab is removed from memory, disk, and state.}
#'  \item{\code{parent(value)}}{Getter/setter method for the parent field, implemented as an active binding on the private member.}
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
#'  }
#'
#'
#' @param lab An object of class 'Lab'.
#' @param stateId Character string indicating the stateId which uniquely identifies a serialized object at prior state.
#'
#' @docType class
#' @examples
#' \dontrun{
#' nlpStudio$exposeObject() # Returns nlpStudio object and meta data in list format
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
          ..name = character(0),
          ..desc = character(0),
          ..labs = list(),
          ..stateId = character(0),
          ..stateDesc = character(0),
          ..created = "None",
          ..modified = "None"
        ),

        public = list(
          #-------------------------------------------------------------------#
          #                       NLPStudio Methods                           #
          #-------------------------------------------------------------------#
          initialize = function() {

            name <- "nlpStudio"
            desc <- "NLPStudio: Natural Language Processing Environment"

            # Suppress automatically generated error messages
            opt <- options(show.error.messages=FALSE, warn = -1)
            on.exit(options(opt))

            # Create Directories
            c <- Constants$new()
            paths <- c$getPaths()
            lapply(paths, function(p) {
              if (!dir.exists(p))  dir.create(p, recursive = TRUE)
            })

            # Create single instance of NLPStudio object
            private$..name <- name
            private$..desc <- desc
            private$..stateDesc <- paste("NLPStudio object", name, "instantiated at", Sys.time())
            private$..modified <- Sys.time()
            private$..created <- Sys.time()

            # Assign its name in the global environment
            assign(name, self, envir = .GlobalEnv)

            # Log Event
            historian$addEvent(className = "NLPStudio", objectName = name,
                              method = "initialize", event = private$..stateDesc)

            invisible(self)
          },

          getInstance = function()  {
            invisible(self)
          },

          getName = function() {
            private$..name
          },

          exposeObject = function() {

            # TODO: Uncomment after testing
            # v <- Validator$new()
            # if (v$exposeObject(object = self,
            #                 requester = requester) == FALSE) stop()

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

          restore = function(requester, prior) {

            v <- Validator$new()
            if (v$restore(object = self,
                          requester = requester,
                          prior = prior) == FALSE) stop()

            r <- prior$exposeObject()
            private$..desc <- r$desc
            private$..labs <- r$labs
            private$..stateId <- r$stateId
            private$..stateDesc <- paste("NLPStudio object prior to prior",
                                         "state, designated by state identifier:",
                                         r$stateId,"at", Sys.time())
            private$..modified <- Sys.time()
            private$..created <- r$created

            # Log event
            # historian$addEvent(className = class(self)[1], objectName = name,
            #                    method = "restore",
            #                    event = private$..stateDesc)

            invisible(self)
          },

          #-------------------------------------------------------------------#
          #                           Lab Methods                             #
          #-------------------------------------------------------------------#
          getChildren = function() private$..labs,

          addChild = function(child) {

            # Perform validation
            v <- Validator$new()
            if (v$addChild(self, child) == FALSE) stop()

            # Get kids name
            kidsName <- child$getName()

            # Save Memento
            private$..stateDesc <- paste("NLPStudio object", private$..name,
                                          "memento, prior to adding", kidsName, "at", Sys.time())
            # self$saveState()

            # Add lab to lab list
            private$..labs[[kidsName]] <- child

            # Add parent to lab
            child$parent <- self

            # Update modified time
            private$..modified <- Sys.time()

            # # Update State
            private$..stateDesc <- paste("Lab", kidsName, "added to nlpStudio.")
            # self$saveState()

            # Log Event
            historian$addEvent(className = "NLPStudio", objectName = "nlpStudio",
                               method = "addChild",
                               event = private$..stateDesc)

            invisible(self)

          },

          removeChild = function(child) {

            # Perform validation
            v <- Validator$new()
            if (v$removeChild(self, child) == FALSE) stop()

            # Obtain kids name
            kidsName <- child$getName()

            # Save State
            private$..stateDesc <- paste("Lab", kidsName,
                                         "memento prior to removing",
                                         kidsName, "from the", class(self)[1],
                                         "object,", private$..name)
            # self$saveState()

            # Set child ancester to NULL
            child$parent <- NULL

            # Remove lab from nlpStudio
            private$..labs[[kidsName]] <- NULL

            # Update modified time
            private$..modified <- Sys.time()

            # Save state
            private$..stateDesc <- paste("Lab", kidsName, "removed from nlpStudio.")
            # self$saveState()

            historian$addEvent(className = "NLPStudio", objectName = "nlpStudio",
                               method = "removeChild",
                               event = private$..stateDesc)

          },

          #-------------------------------------------------------------------#
          #                           State Methods                           #
          #-------------------------------------------------------------------#
          saveState = function() {
            state <- State$new()
            private$..stateId <- state$save(self)
          },

          restoreState = function(stateId) {
            private$..stateId <- stateId
            state <- State$new()
            state$restore(self)
            invisible(self)
          },

          #-------------------------------------------------------------------------#
          #                           Visitor Methods                               #
          #-------------------------------------------------------------------------#
          accept = function(visitor)  {
            visitor$nlpStudio(self)
          }
        )
      )
      super$initialize(...)
    }
  )
)#$new()
