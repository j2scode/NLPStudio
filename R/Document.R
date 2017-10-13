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
#'   \item State: Class responsible for saving current and restoring prior states of objects.
#'   \item Curator: Class responsible for maintaining the object hierarchy.
#'   \item Historian: Class responsible for maintaining the history of events on objects.
#'   \item Reader: Class responsible for initiating the document read operation.
#'   \item Writer: Class responsible for initiating the document write operation.
#'   \item VReader: Visitor class responsible for performing read operations through the Document hierarchy.
#'   \item VWriter: Visitor class responsible for performing write operations through the Document hierarchy.
#'   \item VCurator: Visitor class that fulfills commands from the Curator class.
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
#'   \item{\code{getName()}}{Method for obtaining the document name.}
#'   \item{\code{getObject(requester)}}{Method for obtaining the document data in a list format if invoked by authorized method.}
#'   \item{\code{restore(requester, prior)}}{Method for restoring an object to a prior state, as per the object parameter.}
#'   \item{\code{addContent(content)}}{Method for adding content to the document object. This method is invoked by the read visitor.}
#'  }
#'
#' \strong{Document Field Getter/Setter Active Binding Methods:}
#'  \itemize{
#'   \item{\code{desc()}}{Method used to get / set the description variable.
#'   Implemented as an active binding and so the field may be updated
#'   by assignment. This method is inherited from the Document0 class.}
#' }
#'
#' \strong{Document Composite Methods:}
#'  \itemize{
#'   \item{\code{addChild(document)}}{Not implemented for this class.}
#'   \item{\code{getChildren()}}{Returns NULL.}
#'   \item{\code{removeChild(document)}}{Not implemented for this class.}
#'   \item{\code{parent()}}{Getter/Setter method for retrieving and changing the parent of a Document class object.}
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

  public = list(

    #-------------------------------------------------------------------------#
    #                           Core Methods                                  #
    #-------------------------------------------------------------------------#
    initialize = function(name, fileName, desc = NULL) {stop("This method is not implemented for the Document class.")},

    getObject = function(requester) {

      v <- Validator()
      if (v$getObject(object = self,
                      requester = requester) == FALSE) stop()

      document = list(
        name = private$..name,
        desc = private$..desc,
        parent = private$..parent,
        fileName = private$..fileName,
        content = private$..content,
        stateId = private$..stateId,
        stateDesc = private$..stateDesc,
        created = private$..created,
        modified =private$..modified

      )
      return(document)
    },

    restore = function(requester, prior) {

      v <- Validator$new()
      if (v$restore(object = self,
                    requester = requester, prior = prior) == FALSE) stop()

      private$..desc <- prior$desc
      private$..parent <- prior$parent
      private$..fileName <- prior$fileName
      private$..content <- prior$content
      private$..stateDesc <- paste("Document object", private$..name,
                                   "restored to prior state designated by",
                                   "state identifier:",
                                   prior$stateId,"at", Sys.time())
      private$..stateId <- prior$stateId
      private$..created <- prior$created
      private$..modified <- Sys.time()

      # Save state
      # self$savestate()

      # Log event
      # historian$addEvent(cls = class(self)[1], objectName = name,
      #                    method = "restore",
      #                    event = private$..stateDesc)
      invisible(self)
    },

    addContent = function(requester, content) {

      v <- Validator()
      if (v$addContent(object = self,
                      requester = requester) == FALSE) stop()

      private$..content <- content

      # Save State
      # self$saveState()

      # # Log event
      # private$..stateDesc <- paste("Added content to", class(self)[1],
      #                              "class object,", private$..name)
      # historian$addEvent(cls = class(self)[1], objectName = name,
      #                    method = "addContent",
      #                    event = private$..stateDesc)
    },

    #-------------------------------------------------------------------------#
    #                          Composite Methods                              #
    #-------------------------------------------------------------------------#
    addChild = function(document) { stop("This method not implemented for this class")},
    getChildren = function() { return(NULL) },
    removeChild = function(name, purge = FALSE) { stop("This method not implemented for this class")},


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
    }
  )
)
