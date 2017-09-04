#==============================================================================#
#                                 StateManager                                 #
#==============================================================================#
#' StateManager
#'
#' \code{StateManager} Class for storing the current state of all objects in
#' NLPStudio.
#'
#' NLPStudio objects may be inadvertaetly removed from the global environment.
#' This class allows clients to restore the objects to its current state in
#' the global environment. The current state of all objects is stored to
#' file for persistence from session to session.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{initialize()}}{Creates the state environment.}
#'  \item{\code{getState(key)}}{Retrieves object from state.}
#'  \item{\code{setState(key, value)}}{Save a key value pair to the state environment.}
#'  \item{\code{loadState()}}{Loads the state from file.}
#'  \item{\code{saveState()}}{Restores items in state to the global environment.}
#'  \item{\code{restoreState()}}{Purges the entire state.}
#'  \item{\code{printState}}{Lists the names of the items in the state environment.}
#'  \item{\code{purgeState}}{Lists the names of the items in the state environment.}
#'  \item{\code{printState()}}{Prints the contents of the state environment.}
#'  \item{\code{getInstance()}}{Returns the instance of the singleton StateManager class.}
#' }
#'
#' @param key Character string containing the name of the object to retrieved or stored to state
#' @param value The object to be stored to state
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
StateManager <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "StateManager",
        private = list(
          ..state = character(0),
          ..stateFile = "./NLPStudio/.State.Rdata"
        ),
        public = list(
          initialize = function() private$..state <- new.env(parent = emptyenv()),
          getState = function(key) unlist(private$..state[[key]]),

          setState = function(key, value) {
            private$..state[[key]] <- value
            self$saveState()
          },

          loadState = function() {
            load(file = private$..stateFile)
            private$..state <- nlpStudioState
            return(nlpStudioState)
          },

          saveState = function() {
            nlpStudioState <- lapply(private$..state, function(c) c)
            save(nlpStudioState, file = private$..stateFile)
          },

          restoreState = function() {
            objNames <- names(private$..state)
            lapply(seq_along(private$..state), function(c) {
              objName <- objNames[c]
              assign(objName, private$..state[[c]], .GlobalEnv)
            })
          },

          purgeState = function() {
            private$..state <- new.env(parent = emptyenv())
          },

          printState = function() {
            cat("\n\n############################### State #########################################\n")
            print(lsState())
            cat("\n\n############################### State #########################################\n")
          },

          lsState = function() names(private$..state),

          getInstance = function() invisible(self)
        )
      )
      super$initialize(...)
    }
  ),lock_objects = FALSE
)#$new()
