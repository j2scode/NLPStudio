#==============================================================================#
#                                 Lab                                          #
#==============================================================================#
#' Lab
#'
#' \code{Lab} Class that contains document collections and the environment in which NLP happens.
#'
#' The environment in which NLP happens. There are two groups of methods. The
#' first group allows clients to instantiate, retrieve, print, enter, leave,
#' and archive a Lab object.  The second set of methods allow clients to retrieve
#' the contained documents, add a document, and remove a document.
#'
#' \strong{Lab Core Methods:}
#'  \describe{
#'   \item{\code{new(name, desc = NULL)}}{Creates an object of Lab Class}
#'   \item{\code{desc}}{A getter/setter method allowing clients to retrieve and set the Lab description variable.}
#'   \item{\code{getObject()}}{Retrieves the meta data for the Lab object.}
#'  }
#'
#' \strong{Lab Aggregate Methods:}
#'  \describe{
#'   \item{\code{getChildren()}}{Retrieves a list containing meta data for child objects of the DocumentCollection class.}
#'   \item{\code{addChild(document)}}{Adds a child document, an object of the DocumentCollection class, to the Lab object.}
#'   \item{\code{removeChild(document)}}{Removes a child document, an object of the DocumentCollection class, from the Lab object.}
#' }
#'
#' \strong{Lab State Methods:}
#'  \describe{
#'   \item{\code{saveState()}}{Method that initiates the process of saving the current state of an object.}
#'   \item{\code{restoreState(stateId)}}{Method that initiates the process of restoring an object to a prior state.}
#' }
#'
#'
#' \strong{Lab Visitor Methods:}
#'  \describe{
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#' }
#'
#' @param name A character string containing the name of the Lab object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Lab
#' @param document An object of the DocumentCollection class to be added to the Lab object's list of document collections.
#' @param visitor An object of one of the visitor classes.
#' @param stateId Character string identifying a prior state for a Lab object.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Lab <- R6::R6Class(
  classname = "Lab",
  lock_objects = FALSE,
  lock_class = FALSE,
  private = list(
    ..name = character(0),
    ..desc = character(0),
    ..parent = character(0),
    ..collections = list(),
    ..state = character(0),
    ..stateId = character(0),
    ..modified = "None",
    ..created = "None"
  ),

  active = list(

    desc = function(value) {
      if (missing(value)) {
        private$..desc
      } else {
        private$..desc <- value
      }
      #TODO; Add call to statebuilder
      print("State saved after update of description")
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                         Lab Core Methods                                #
    #-------------------------------------------------------------------------#
    initialize = function(name, desc = NULL) {

      # Validate Name
      v <- ValidateName$new()
      if (v$validate(class = "NLPStudio", method = "initialize", value = name,
                     expect = FALSE) == FALSE) {
        stop()
      }

      # Confirm lab does not already exist
      v <- ValidateExists$new()
      if (v$validate(class = "Lab", method = "initialize",
                 fieldName = "name", value = name, level = "Error",
                 msg = paste("Cannot create lab as", name,
                             "already exists.",
                             "See ?Lab"),
                 expect = FALSE) == FALSE) {
        stop()
      }

      # Instantiate variables
      private$..name <- name
      if (is.null(desc)) { desc <- paste(name, "Lab") }
      private$..desc <- desc
      private$..parent <- nlpStudio
      private$..state <+ paste("Lab", name, "instantiated at", Sys.time())
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      # Log Event
      historian$addEvent(class = "Lab", objectName = name,
                         method = "initialize",
                         event = paste("Instantiated", name, "Lab,"))

      invisible(self)
    },

    getObject = function() {

      lab = list(
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        documents = private$..collections,
        state = private$..state,
        stateId = private$..stateId,
        modified = private$..modified,
        created = private$..created
      )

      return(lab)
    },


    #-------------------------------------------------------------------------#
    #                         Lab Aggregate Methods                           #
    #-------------------------------------------------------------------------#
    getChildren = function() { private$..collections },

    addChild = function(collection) {

      # Validation
      if (missing(collection)) {
        v <- Validate0$new()
        v$notify(class = "Lab", method = "addChild",
                 fieldName = "collection", value = "", level = "Error",
                 msg = paste("Unable to add collection.",
                             "Document parameter is missing with no default.",
                             "Please see ?Lab for further assistance."),
                 expect = TRUE)
        stop()
      }

      # Validate
      v <- ValidateClass$new()
      if (v$validate(class = "Lab", level = "Error", method = "addChild",
                   fieldName = "collection", value = collection,
                   msg = paste("Invalid class. DocumentCollection class expected",
                               class(collection), "encountered.",
                               "Please see ?Lab for further assistance."),
                   expect = "DocumentCollection") == FALSE) {
        stop()
      }

      # Add collection to list of collections
      c <- collection$getObject()
      private$..collections[[c$name]] <- collection

      # Add parent to document
      collection$setAncestor(self)

      # Update modified time
      private$..modified <- Sys.time()

      # Update State
      private$..state <- paste("Collection", c$name, "added to Lab", private$..name, "at", Sys.time())
      self$saveState()

      # Log Event
      historian$addEvent(class = "Lab", objectName = private$..name,
                         method = "addChild",
                         event = private$..state)


      invisible(self)

    },

    removeChild = function(collection) {

      # Confirm collection parameter is not missing
      if (missing(collection)) {
        v <- Validate0$new()
        v$notify(class = "Lab", method = "removeChild",
                 fieldName = "collection", value = "", level = "Error",
                 msg = paste("collection is missing with no default.",
                             "See ?Lab for further assistance."),
                 expect = TRUE)
        stop()
      }

      # Obtain collection meta data
      c <- collection$getObject()

      # Remove collection from lab and update modified time
      private$..collections[[c$name]] <- NULL
      private$..modified <- Sys.time()

      # Update State
      private$..state <- paste("Collection", c$name, "removed from Lab", private$..name, "at", Sys.time())
      self$saveState()

      # Log Event
      historian$addEvent(class = "Lab", objectName = private$..name,
                         method = "removeChild",
                         event = private$..state)

      invisible(self)

    },

    #-------------------------------------------------------------------------#
    #                           Lab State Methods                             #
    #-------------------------------------------------------------------------#
    saveState = function() {
      state <- State$new()
      state$save(self)
    },

    restoreState = function(stateId) {
      private$..stateId <- stateId
      state <- State$new()
      state$restore(self)
    }
  )
)
