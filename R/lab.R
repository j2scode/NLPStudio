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
#' @section Lab Methods:
#' \describe{
#'  \item{\code{new(name, desc = NULL)}}{Creates an object of Lab Class}
#'  \item{\code{desc}}{A getter/setter method allowing clients to retrieve and set the Lab description variable.}
#'  \item{\code{getObject()}}{Retrieves the meta data for the Lab object.}
#'  \item{\code{getAncestor()}}{Retrieves the parent object for the Lab object.}
#'  \item{\code{setAncestor()}}{Sets the parent object for the Lab object.}
#'  \item{\code{accept(visitor)}}{Accepts a visitor and responds by dispatching the appropriate method, sending itself as a parameter.}
#' }
#'
#'@section Document collection (child) Methods:
#' \describe{
#'  \item{\code{getChildren()}}{Retrieves a list containing meta data for child objects of the DocumentCollection class.}
#'  \item{\code{addChild(document)}}{Adds a child document, an object of the DocumentCollection class, to the Lab object.}
#'  \item{\code{removeChild(document)}}{Removes a child document, an object of the DocumentCollection class, from the Lab object.}
#' }
#'
#' @param name A character string containing the name of the Lab object. This variable is used in the instantiation and remove methods.
#' @param desc A chararacter string containing the description of the Lab
#' @param document An object of the DocumentCollection class to be added to the Lab object's list of document collections.
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
    ..parentName = "nlpStudio",
    ..path = character(0),
    ..collections = list(),
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
      stateManager$saveState(self)
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                              Lab Methods                                #
    #-------------------------------------------------------------------------#

    initialize = function(name, desc = NULL) {

      # Load directories
      dirs <- nlpStudio$getPaths()

      # Validate Name
      v <- ValidateName$new()
      if (v$validate(cls = "NLPStudio", method = "initialize", value = name,
                     expect = FALSE) == FALSE) {
        stop()
      }

      # Confirm lab does not already exist
      v <- ValidateExists$new()
      if (v$validate(cls = "Lab", method = "initialize",
                 fieldName = "name", value = name, level = "Error",
                 msg = paste("Cannot create lab because", name,
                             "already exists.",
                             "See ?Lab"),
                 expect = FALSE) == FALSE) {
        stop()
      }

      # Confirm directory does not exist
      v <- ValidatePath$new()
      if (v$validate(cls = "Lab", method = "initialize",
                 fieldName = "name",
                 value = file.path(dirs$labs, name),
                 level = "Error",
                 msg = paste("Cannot create lab because", name,
                             "directory already exists.",
                             "See ?Lab"),
                 expect = FALSE) == FALSE) {
        stop()
      }

      # Instantiate variables
      private$..name <- name
      if (is.null(desc)) { desc <- paste(name, "Lab") }
      private$..desc <- desc
      private$..parent <- nlpStudio
      private$..parentName <- "nlpStudio"
      private$..path <- file.path(dirs$labs, name)
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Create lab directory
      dir.create(file.path(dirs$labs, name))

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      invisible(self)
    },

    getAncestor = function() {
      return(private$..parent)
    },

    setAncestor = function(parent) {
      o <- parent$getObject()
      private$..parentName <- o$name
      private$..parent <- parent
      return(private$..parentName)
    },

    getObject = function() {

      lab = list(
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        parentName = private$..parentName,
        path = private$..path,
        documents = private$..collections,
        modified = private$..modified,
        created = private$..created
      )

      return(lab)
    },

    #-------------------------------------------------------------------------#
    #                         Document Methods                                #
    #-------------------------------------------------------------------------#

    getChildren = function() {

      documents = lapply(private$..collections, function(d) {
        d$getObject()
      })

      return(documents)
    },

    addChild = function(document) {

      # Validation
      if (missing(document)) {
        v <- Validate0$new()
        v$notify(cls = "Lab", method = "addChild",
                 fieldName = "document", value = "", level = "Error",
                 msg = paste("Unable to add document.",
                             "Document parameter is missing with no default.",
                             "Please see ?Lab for further assistance."),
                 expect = TRUE)
        stop()
      }

      # Validate
      v <- ValidateClass$new()
      if (v$validate(cls = "Lab", level = "Error", method = "addChild",
                   fieldName = "document", value = document,
                   msg = paste("Invalid class. DocumentCollection class expected",
                               class(document), "encountered.",
                               "Please see ?Lab for further assistance."),
                   expect = "DocumentCollection") == FALSE) {
        stop()
      }

      # Add collection to list of collections
      d <- document$getObject()
      private$..collections[[d$name]] <- document

      # Add parent to document
      document$setAncestor(self)

      # Update modified time
      private$..modified <- Sys.time()

      # Update State
      stateManager$saveState(self)
      stateManager$saveState(document)

      invisible(self)

    },

    removeChild = function(document) {

      # Confirm document parameter is not missing
      if (missing(document)) {
        v <- Validate0$new()
        v$notify(cls = "Lab", method = "removeDocument",
                 fieldName = "document", value = "", level = "Error",
                 msg = paste("Document is missing with no default.",
                             "See ?Lab for further assistance."),
                 expect = TRUE)
        stop()
      }

      # Confirm document class
      v <- ValidateClass$new()
      if (v$validate(cls = "Lab", method = "removeChild",
                     fieldName = "document", value = document, level = "Error",
                     msg = paste("Lab method is unable to remove child document",
                                 "of the", class(document)[1], "class.",
                                 "Lab methods can only remove DocumentCollection",
                                 "objects.", "See ?Lab for further assistance."),
                     expect = "DocumentCollection") == FALSE) {
        stop()
      }

      # Obtain document meta data
      d <- document$getDocument()

      # Confirm object is not self
      if (private$..name == d$name) {
        v <- Validate0$new()
        v$notify(cls = "Lab", method = "removeDocument",
                 fieldName = "document", value = d$name,
                 level = "Error",
                 msg = paste("Object unable to remove itself. Removal must",
                             "be invoked from the parent object.",
                             "See ?Lab for further assistance"),
                 expect = NULL)
          stop()
      }

      # Archive before removing document
      stateManager$saveState(self)
      stateManager$saveState(document)

      # Move files to orphan directory
      File$orphan(document)

      # Remove collection from lab and update modified time
      private$..collections[[d$name]] <- NULL
      private$..modified <- Sys.time()

      # Update State
      stateManager$saveState(self)

      invisible(self)

    },

    #-------------------------------------------------------------------#
    #                           Visitor Methods                         #
    #-------------------------------------------------------------------#
    accept = function(visitor) {
      visitor$visitLab(self)
    },

    acceptArchive = function(visitor, stateId) {
      visitor$visitLab(stateId, self)
    },

    acceptRestore = function(visitor, stateId) {
      visitor$visitLab(stateId, self)
    }
  )
)
