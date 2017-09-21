#==============================================================================#
#                                   Document                                   #
#==============================================================================#
#' Document
#'
#' \code{Document} An abstract class that defines the interfaces and primary
#' methods for the DocumentText, DocumentRdata, DocumentCsv, and DocumentXlsx
#' sub classes.
#'
#' \strong{Document Family of Classes Overview:}
#'
#' The Document family of classes is an implementation of the composite
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows composite
#' and individual objects to be treated uniformly.
#'
#' The following sections include:
#' \itemize{
#'  \item Class Participants: Classes the comprise this composite pattern.
#'  \item Class Collaborators: Classes which interact with the Document0 class.
#'  \item Class Methods: Methods included in the interface.
#'  }
#'
#' \strong{Document Family of Classes Participants:}
#' The participants of the Document0 class are:
#' \itemize{
#'  \item Document0: This component class specifies an abstract interface
#'  for all leaf and composite document classes.
#'  \item DocumentCollection: The composite class that maintains the
#'  hierarchical structure of document collections (composites) and individual
#'  documents (leafs).
#'  \item Document: This "abstract leaf" class defines the interface for
#'  DocumentText, DocumentRdata, DocumentCsv, and DocumentXlsx sub-classes.
#'  \item DocumentText: This "concrete leaf" class for text documents.
#'  \item DocumentCsv: This "concrete leaf" class for csv documents.
#'  \item DocumentRdata: This "concrete leaf" class for RData documents.
#'  \item DocumentXlsx: This "concrete leaf" class for excel documents.
#'  }
#'
#' \strong{Document Class Collaborators:}
#' The collaborators of the Document family  are:
#'  \itemize{
#'   \item DocumentCollection: Class resonsible for containing the composite hierarchy of documents.
#'   \item Reader: Class responsible for initiating the document read operation.
#'   \item Writer: Class responsible for initiating the document write operation.
#'   \item VReader: Visitor class responsible for performing read operations through the Document hierarchy.
#'   \item VWriter: Visitor class responsible for performing write operations through the Document hierarchy.
#'  }
#'
#' \strong{Document Methods:}
#' There are six types of methods within the Document class and they are:
#' \itemize{
#'  \item{Core Methods: Core methods shared by both Document and
#'  DocumentCollection objects.}
#'  \item{Getter/Setter Methods: Active binding methods for getting and setting
#'  selected private members.}
#'  \item{Composite Methods: Methods implemented by the DocumentCollection
#'  class to maintain the document heirarchy.}
#'  \item{State Methods: Methods for saving current and restoring to prior object states.}
#'  \item{Visitor Methods: Methods for implementation of and messaging
#'  with objects of the visitor classes.}
#' }
#'
#' \strong{Document Core Methods:}
#'  \itemize{
#'   \item{\code{new(name, desc)}}{Method for instantiating a document}
#'   \item{\code{getObject()}}{Method for obtaining the document data in a list format.}
#'   \item{\code{setObject(object)}}{Method for restoring an object to a prior state, as per the object parameter.}
#'   \item{\code{addContent(content)}}{Method for adding content to the document object. This method is invoked by the read visitor.}
#'  }
#'
#' \strong{Document Field Getter/Setter Active Binding Methods:}
#'  \itemize{
#'   \item{\code{desc()}}{Method used to get / set the description variable.
#'   Implemented as an active binding and so the field may be updated
#'   by assignment. This method is inherited from the Document0 class.}
#'   \item{\code{fileName()}}{Method used to get / set the file name variable.
#'   Implemented as an active binding and so the field may be updated
#'   by assignment.}
#' }
#'
#' \strong{Document Composite Methods:}
#'  \itemize{
#'   \item{\code{addChild(document)}}{Not implemented for this class.}
#'   \item{\code{getChildren()}}{Returns NULL.}
#'   \item{\code{removeChild(document)}}{Not implemented for this class.}
#'   \item{\code{getAncestor()}}{Returns the parent object for the Document object.}
#'   \item{\code{setAncestor(parent)}}{Sets the parent object for the Document object.}
#' }
#'
#'
#' \strong{Document State Methods:}
#'  \itemize{
#'   \item{\code{saveState()}}{Method that initiates the process of saving the current state of the object. This method is inherited from the Document0 class.}
#'   \item{\code{restoreState()}}{Method that initiates the process of restoring an object to a prior state. This method is inherited from the Document0 class.}
#'  }
#'
#' \strong{Document Visitor Methods:}
#'  \itemize{
#'   \item{\code{accept(visitor)}}{Method for accepting the visitor objects.}
#'   \item{\code{acceptUpdate(visitor, object)}}{Accepts an object of the VUpdate class.}
#'  }
#'
#' @param name Character string indicating the name of the document or file. Required for all objects.
#' @param desc Character string containing the description of the document.
#' @param content Nested list of content to be written to files.
#' @param fileName Character string indicating File object's file name.
#' @param parent An object of the Lab or DocumentCollection class that represents
#' the parent object.
#' @param visitor An object of one of the visitor classes.
#' @param stateId Character string that uniquely identifies an object and its
#' state at a specific point in time.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Document classes
#' @export
Document <- R6::R6Class(
  classname = "Document",
  inherit = Document0,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(
    ..fileName = character(0),
    ..content = character(0)
  ),

  active = list(
    fileName = function(value) {
      if (missing(value)) private$..fileName
      else private$..fileName <- value
    }
  ),

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, fileName, desc = NULL) {

      # Confirm required parameters are not missing.
      if (missing(name)) {
        v <- Validate0$new()
        v$notify(class = class(self)[1], method = "initialize", fieldName = "name",
                 value = "", level = "Error",
                 msg = paste0("Name parameter is missing with no default. ",
                             "See ?", class(self)[1], " for further assistance."),
                 expect = NULL)
        stop()
      }

      # Validate name
      v <- ValidateName$new()
      if (v$validate(class = class(self)[1], method = "initialize",
                     value = name, expect = FALSE) == FALSE) {
        stop()
      }

      if (missing(fileName)) {
        v <- Validate0$new()
        v$notify(class = class(self)[1], method = "initialize", fieldName = "fileName",
                 value = "", level = "Error",
                 msg = paste0("File name parameter is missing with no default. ",
                             "See ?", class(self)[1], " for further assistance."),
                 expect = NULL)
        stop()
      }

      # Instantiate variables
      private$..name <- name
      private$..desc <- ifelse(is.null(desc), paste(name, "Document"), desc)
      private$..fileName <- fileName
      private$..stateDesc <- paste("Document", name, "instantiated at", Sys.time())
      private$..created <- Sys.time()
      private$..modified <- Sys.time()

      # Assign to object to name  in global environment
      assign(name, self, envir = .GlobalEnv)

      # # Log event
      # historian$addEvent(class = class(self)[1], objectName = name,
      #                    method = "initialize",
      #                    event = private$..stateDesc)

      invisible(self)
    },

    getObject = function() {

      document <- list (
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        fileName = private$..fileName,
        content = private$..content,
        stateId = private$..stateId,
        stateDesc = private$..stateDesc,
        created = private$..created,
        modified = private$..modified
      )

      return(document)
    },

    setObject = function(visitor, restored) {

      v <- ValidateClass$new()
      if (v$validate(class = class(self)[1], level = "Error", method = "setObject",
                     fieldName = "visitor", value = visitor,
                     msg = paste0("Class not authorized to invoke this method. ",
                                 "See ?", class(self)[1], " for further assistance."),
                     expect = "VUpdate") == FALSE) {
        stop()
      }

      if (v$validate(class = class(self)[1], level = "Error", method = "setObject",
                     fieldName = "restored", value = restored,
                     msg = paste0("Unable to restore ", private$..name, ", ",
                                  "an object of class ", class(self)[1], "to state ",
                                  "of an object of class ", class(restored)[1], ". ",
                                  "See ?", class(self)[1], " for further assistance."),
                     expect = "Document") == FALSE) {
        stop()
      }
      r <- restored$getObject()
      private$..desc <- r$desc
      private$..parent <- r$parent
      private$..fileName <- r$fileName
      private$..content <- r$content
      private$..stateDesc <- paste("Document object", private$..name,
                                   "restored to prior state designated by",
                                   "state identifier:",
                                   r$stateId,"at", Sys.time())
      private$..stateId <- r$stateId
      private$..created <- r$created
      private$..modified <- Sys.time()

      # Log event
      # historian$addEvent(class = class(self)[1], objectName = name,
      #                    method = "setObject",
      #                    event = private$..stateDesc)
      invisible(self)
    },

    addContent = function(visitor, content) {

      v <- ValidateClass$new()
      if (v$validate(class = class(self)[1], level = "Error", method = "addContent",
                     fieldName = "visitor", value = visitor,
                     msg = paste0("Class not authorized to invoke this method. ",
                                  "See ?", class(self)[1], " for further assistance."),
                     expect = "VReader") == FALSE) {
        stop()
      }
      private$..content <- content

      # # Log event
      # private$..stateDesc <- paste("Added content to", class(self)[1],
      #                              "class object,", private$..name)
      # historian$addEvent(class = class(self)[1], objectName = name,
      #                    method = "addContent",
      #                    event = private$..stateDesc)
    },

    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#
    addChild = function(document) { stop("This method not implemented for this class")},
    getChildren = function() { return(NULL) },
    removeChild = function(name, purge = FALSE) { stop("This method not implemented for this class")},

    getAncestor = function() private$..parent,

    setAncestor = function(parent) {

      if (!is.null(parent)) {

        # Obtain parent information
        p <<- parent$getObject()

        v <- ValidateClass$new()
        if (v$validate(class = class(self)[1], method = "setAncestor", fieldName = "class(parent)",
                       level = "Error", value = class(parent)[1],
                       msg = paste0("Unable to set parent.  Parent must be a ",
                                   "Document or Lab object. ",
                                   "See ?", class(self)[1], " for further assistance."),
                       expect = c("Document", "Lab")) == FALSE) {
          stop()
        }

        # Save Memento
        private$..state <- paste("Saving Memento of Document object",
                                 private$..name, "before setting parent to",
                                 p$name, "at", Sys.time())
        # self$saveState(self)

        # Set parent and state description
        private$..parent <- parent
        private$..stateDesc <- paste("Set parent of Document object,",
                                     private$..name, "to", p$name, "at",
                                     Sys.time())
      } else {
        # Save Memento
        private$..stateDesc <- paste("Saving Memento of Document object",
                                     private$..name, "before setting parent to NULL",
                                     "at", Sys.time())
        # self$saveState(self)

        # Set parent to null and update object state"
        private$..parent <- NULL
        private$..stateDesc <- paste("Set parent of Document object",
                                     private$..name, "to NULL at",
                                     Sys.time())
      }

      private$..modified <- Sys.time()
      # self$saveState(self)

      # Log Event
      # historian$addEvent(class = class(self)[1], objectName = private$..name,
      #                    method = "removeChild",
      #                    event = private$..stateDesc)
    },


    #-------------------------------------------------------------------#
    #                           State Method                            #
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
      visitor$document(self)
    },
    acceptVUpdate = function(visitor, priorObject)  {
      visitor$document(self, priorObject)
    }
  )
)
