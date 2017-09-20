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
#'   \item{\code{setObject(object)}}{Sets an object to a previous stateDesc as per the object parameter.}
#'  }
#'
#' \strong{Lab Aggregate Methods:}
#'  \describe{
#'   \item{\code{getChildren()}}{Retrieves a list containing meta data for child objects of the DocumentCollection class.}
#'   \item{\code{addChild(document)}}{Adds a child document, an object of the DocumentCollection class, to the Lab object.}
#'   \item{\code{removeChild(document)}}{Removes a child document, an object of the DocumentCollection class, from the Lab object.}
#'   \item{\code{setAncestor(document)}}{Removes a child document, an object of the DocumentCollection class, from the Lab object.}
#'   \item{\code{getAncestor()}}{Removes a child document, an object of the DocumentCollection class, from the Lab object.}
#' }
#'
#'
#' \strong{Lab Visitor Methods:}
#'  \describe{
#'   \item{\code{accept(visitor)}}{Accepts an object of the Visitor family of classes.}
#'   \item{\code{acceptUpdate(visitor, object)}}{Accepts an object of the VUpdate class.}
#'   \item{\code{acceptAdd(visitor, object)}}{Accepts an object of the VAddChild class.}
#'   \item{\code{acceptRemove(visitor, object)}}{Accepts an object of the VRemoveChild class.}
#'   \item{\code{acceptAssociate(visitor, object)}}{Accepts an object of the VAssociate class.}
#' }
#'
#' @param name A character string containing the name of the Lab object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Lab
#' @param document An object of the DocumentCollection class to be added to the Lab object's list of document collections.
#' @param visitor An object of one of the visitor classes.
#' @param stateId Character string identifying a prior stateDesc for a Lab object.
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
    ..stateId = character(0),
    ..stateDesc = character(0),
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
      # # Save State
      private$..stateDesc <- paste(private$..name, "Lab description changed at", Sys.time())
      # state <- State$new()
      # private$..stateId <- state$saveState(self)
    },

    saveState = function() {
      state <- State$new()
      private$..stateId <- state$saveState(self)
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
      private$..parent <- nlpStudio$getInstance()
      private$..stateDesc <- paste("Lab", name, "instantiated at", Sys.time())
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      # Log Event
      # historian$addEvent(class = "Lab", objectName = name,
      #                    method = "initialize",
      #                    event = private$..stateDesc)

      invisible(self)
    },

    getObject = function() {

      lab = list(
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        documents = private$..collections,
        stateDesc = private$..stateDesc,
        stateId = private$..stateId,
        modified = private$..modified,
        created = private$..created
      )

      return(lab)
    },

    setObject = function(object) {
      o <- object$getObject()
      private$..desc <- o$desc
      private$..parent <- nlpStudio
      private$..stateDesc <- o$stateDesc
      private$..stateId <- o$stateId
      private$..created <- o$created
      private$..modified <- o$modified
      invisible(self)
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

      # Get collection information
      c <- collection$getObject()

      # Save state as memento
      private$..stateDesc <- paste("Memento of", private$..name, "before adding ", c$name, "at", Sys.time())
      # private$saveState(self)

      # Add collection to lab's list of collections
      private$..collections[[c$name]] <- collection

      # Set parent to document collection object
      collection$setAncestor(self)

      # Update modified time
      private$..modified <- Sys.time()

      # Save State
      private$..stateDesc <- paste("Collection", c$name, "added to Lab", private$..name, "at", Sys.time())
      # private$saveState(self)

      # Log Event
      # historian$addEvent(class = "Lab", objectName = private$..name,
      #                    method = "addChild",
      #                    event = private$..stateDesc)

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

      # Obtain collection information
      c <- collection$getObject()

      # Save state as memento
      private$..stateDesc <- paste("Memento of", private$..name, "before removing ", c$name, "at", Sys.time())
      # private$saveState(self)


      # Remove collection from lab and update modified time
      private$..collections[[c$name]] <- NULL
      private$..modified <- Sys.time()

      # Update State
      private$..stateDesc <- paste("Collection", c$name, "removed from Lab", private$..name, "at", Sys.time())
      # private$saveState(self)

      # Log Event
      # historian$addEvent(class = "Lab", objectName = private$..name,
      #                    method = "removeChild",
      #                    event = private$..stateDesc)

      invisible(self)

    },

    getAncestor = function() {

      p <- private$..parent

      return(p)
    },

    setAncestor = function(parent = NULL) {

      v <- ValidateClass$new()
      if (v$validate(class = "Lab", method = "setAncestor", fieldName = "class(parent)",
                     level = "Error", value = parent,
                     msg = paste("Unable to set parent.  Parent must be a",
                                 "NLPStudio class object.",
                                 "See ?Lab for assistance."),
                     expect = "NLPStudio") == FALSE) {
        stop()
      }

      # Get parent information
      p <- parent$getObject()

      # Update State
      private$..stateDesc <- paste("Memento of Lab object", private$..name, "prior to setting ancestor to", p$name, "at", Sys.time())
      # private$saveState(self)

      # Set parent and obtain parent information.
      private$..parent <- parent
      private$..modified <- Sys.time()

      # Update State
      private$..stateDesc <- paste("Set parent of DocumentCollection object,",
                                private$..name, "to", p$name, "at",
                                Sys.time())
      # private$saveState(self)

      # Log Event
      # historian$addEvent(class = "DocumentCollection", objectName = private$..name,
      #                    method = "removeChild",
      #                    event = private$..stateDesc)

    },

    #-------------------------------------------------------------------------#
    #                           Visitor Methods                               #
    #-------------------------------------------------------------------------#
    accept = function(visitor)  {
      visitor$lab(self)
    },
    acceptVUpdate = function(visitor, priorObject)  {
      visitor$lab(self, priorObject)
    },
    acceptVAddChild = function(visitor, child)  {
      visitor$lab(self, child)
    },
    acceptVRemoveChild = function(visitor, child)  {
      visitor$lab(self, child)
    }
  )
)
