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
#'  \item{\code{enterLab(lab)}}{Sets the currentLab and currentLabName to that of the lab object parameter.  The parameter must be an object of the Lab class.}
#'  \item{\code{leaveLab(lab)}}{Sets the currentLab and currentLabName to "None".}
#' }
#'
#' @param enter A logical indicating whether to enter a lab and to set the lab current.
#' @param lab An object of class 'Lab'.
#'
#' @docType class
#' @examples
#' \dontrun{
#' nlpStudio$getObject() # Returns nlpStudio object and meta data in list format
#' Lab$new(name = "List", desc = "Lisa Simpon's lab")
#' Lab$new(name = "Bart", desc = "Bart Simpon's lab")
#' nlpStudio$addChild(Lisa, enter = FALSE) # Adds lab without setting it current.
#' nlpStudio$addChild(Bart, enter = TRUE) # Adds lab and sets it current.
#' nlpStudio$removeChild("Bart") # Fails: Cannot remove a current lab
#' nlpStudio$enterLab(Lisa)
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
          ..class = "NLPStudio",
          ..desc = "NLPStudio: Natural Language Processing Studio",
          ..studioDirs = list(
            studio = "./NLPStudio",
            archives = "./NLPStudio/Archives",
            archivesLabs = "./NLPStudio/Archives/Labs",
            archivesCollections = "./NLPStudio/Archives/Collections",
            archivesDocuments = "./NLPStudio/Archives/Documents",
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
              if (!dir.exists(d)) {
                suppressWarnings(dir.create(d))
              }
            })

            # Create single instance of NLPStudio object
            private$..name <- name
            private$..desc <- desc
            private$..currentLab <- "None"
            private$..modified <- Sys.time()
            private$..created <- Sys.time()

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

          getObject = function() {

            studio = list(
              name = private$..name,
              class = private$..class,
              desc = private$..desc,
              labs = private$..labs,
              currentLab = private$..currentLab,
              currentLabName = private$..currentLabName,
              modified = private$..modified,
              created = private$..created
              )

            # Update State
            nlpStudioState$saveState(private$..name, self)

            return(studio)
          },

          #-------------------------------------------------------------------#
          #                           Lab Methods                             #
          #-------------------------------------------------------------------#
          getChildren = function() {

            labs = lapply(private$..labs, function(l) {
              lab <- l$getObject()
            })
            return(labs)
          },

          addChild = function(lab, enter = FALSE) {

            # Validation
            if (missing(lab)) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "addChild",
                         fieldName = "lab", value = "", level = "Error",
                         msg = paste("Unable to add lab.",
                                     "Variable lab is missing with no default.",
                                     "Please see ?NLPStudio for further assistance."),
                         expect = TRUE)
              stop()
            }

            v <- ValidateClass$new()
            if (v$validate(cls = "NLPStudio", method = "addChild",
                           fieldName = "lab", value = lab, level = "Error",
                           msg = paste("Object is not a valid 'Lab' type.",
                                       "Please see ?NLPStudio for further assistance."),
                           expect = "Lab") == FALSE) {
              stop()
            }

            v <- ValidateLogical$new()
            if (v$validate(cls = "NLPStudio", method = "addChild",
                           fieldName = "enter", value = enter, level = "Error",
                           msg = paste("Invalid logical,", enter, "must be TRUE or FALSE",
                                       "Please see ?NLPStudio for further assistance."),
                           expect = TRUE) == FALSE) {
              stop()
            }

            # Add lab to lab list
            l <- lab$getObject()
            private$..labs[[l$name]] <- lab

            # Add parent to lab
            lab$setAncestor(self)

            # Update current lab
            if (enter == TRUE) {
              private$..currentLab <- lab
              private$..currentLabName <- l$name
            }

            # Update modified time
            private$..modified <- Sys.time()

            # Update State
            nlpStudioState$saveState(l$name, lab)

            invisible(self)

          },

          removeChild = function(lab) {

            # Confirm lab parameter is not missing
            if (missing(lab)) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "removeChild",
                       fieldName = "lab", value = "", level = "Error",
                       msg = paste("Lab is missing with no default.",
                                   "See ?NLPStudio for further assistance."),
                       expect = TRUE)
              stop()
            }

            # Confirm lab is a lab
            v <- ValidateClass$new()
            if (v$validate(cls = "NLPStudio", method = "removeChild",
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
              v$notify(cls = "NLPStudio", method = "removeChild",
                       fieldName = "lab", value = lab, level = "Error",
                       msg = "Unable to remove a current lab.  See ?NLPStudio",
                       expect = NULL)
              stop()
            }

            # Obtain lab meta data
            l <- lab$getObject()

            # Snapshot self and lab
            Snap0$save(self)
            Snap0$save(lab)

            # Delete files and directory
            base::unlink(l$path)

            # Remove lab from nlpStudio
            private$..labs[[l$name]] <- NULL

            # Remove lab from global environment.
            rm(list = ls(envir = .GlobalEnv)[grep(l$name,
                                                  ls(envir = .GlobalEnv))])

            # Update modified time
            private$..modified <- Sys.time()

            # Save state
            nlpStudioState$saveState(private$..name, self)

          },

          enterLab = function(lab) {

            l <- lab$getObject()

            if (isTRUE(all.equal(lab, private$..currentLab))) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "enterLab",
                       fieldName = "lab", value = l$name, level = "Info",
                       msg = "Already entered lab.",
                       expect = NULL)
            } else {

              if (!isTRUE(all.equal(private$..currentLab, "None"))) {
                currentLab <- private$..currentLab$getObject()
                v <- Validate0$new()
                v$notify(cls = "NLPStudio", method = "enterLab",
                         fieldName = "lab", value = l$name, level = "Info",
                         msg = paste("Leaving", currentLab$name, ". Entering", l$name),
                         expect = NULL)
              }
              private$..currentLab <- lab
              private$..currentLabName <- l$name
              private$..modified <- Sys.time()
            }


            # Update State
            nlpStudioState$saveState(l$name, lab)

            invisible(self)

          },

          leaveLab = function(lab) {

            l <- lab$getObject()
            currentLab <- private$..currentLab$getObject()

            if (!isTRUE(all.equal(lab, private$..currentLab))) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "leaveLab",
                       fieldName = "lab", value = l$name, level = "Warn",
                       msg = paste("Unable to leave lab", l$name,
                                   ". Current lab is", currentLab$name, "."),
                       expect = NULL)
            } else {
              private$..currentLab <- "None"
              private$..currentLabName <- "None"
              private$..modified <- Sys.time()
            }

            # Save state
            nlpStudioState$saveState(l$name, lab)

            invisible(self)

          },

          getDirectories = function() {
            private$..studioDirs
          },

          #-------------------------------------------------------------------#
          #                           Visitor Methods                         #
          #-------------------------------------------------------------------#
          accept = function(visitor) {
            visitor$visitNLPStudio(self)
          },

          acceptArchive = function(visitor, stateId) {
            visitor$visitNLPStudio(stateId, self)
          },

          acceptRestore = function(visitor, stateId) {
            visitor$visitNLPStudio(stateId, self)
          }

        )
      )
      super$initialize(...)
    }
  )
)#$new()
