#==============================================================================#
#                             VisitorArchive                                   #
#==============================================================================#
#' VisitorArchive
#'
#' \code{VisitorArchive} Class responsible for archiving files associated with
#' objects in the NLPStudio package. Files are archived to folders designated
#' by the class of the object to which the files are associated.
#'
#' \strong{Visitor Class Family  Overview:}
#'
#' The Visitor family of classes is an implementation of the visitor
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows operations
#' to be defined without changing the clases of the elemnts upon which
#' it operates.
#'
#' \strong{Visitor Class Family Participants:}
#' The participants of the Visitor class family are:
#' \itemize{
#'  \item Visitor0: This class which specifies an abstract interface for all concrete visitors.
#'  \item VisitorArchive This class archives the files associated with a designated object.
#'  \item VisitorRestore This class restores files associated with from archives the files associated with a designated object.
#'  \item VisitorOrphan: Visitor class responsible for moving an object's files to the orphan directory when the object has been removed from a lab or a collection.
#'  }
#'
#' \strong{Visitor Class Family Collaborators:}
#' The collaborators of the Visitor0 class are:
#' \itemize{
#'  \item Lab: Class for creating and managing documents and workflows.
#'  \item DocumentCollection: A composite class participant of the Document0 composite class which creates and manages collections of documents
#'  \item Document: A leaf class participant of the Document0 composite class. Elements include individual documents.
#'  }
#'
#' The methods are as follows:
#' \itemize{
#'  \item{\code{visitLab(lab)}}{Base method for visiting a object of the Lab class.}
#'  \item{\code{visitDocumentCollection(collection)}}{Base method visiting a object of the DocumentCollection class.}
#'  \item{\code{visitDocument(document)}}{Base method visiting a object of the Document class.}
#' }
#'
#' @param lab An object of the Lab class.
#' @param collection An object of the DocumentCollection class.
#' @param document An object of the Document class.
#' @param stateId A unique identifier given to each object state which is saved.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Visitor Classes
#' @export
VisitorArchive <- R6::R6Class(
  classname = "VisitorArchive",
  inherit = Visitor0,
  lock_objects = FALSE,
  lock_class = FALSE,

  public = list(

    visitLab = function(stateId, lab) {
      dirs <- nlpStudio$getDirectories()

    },
    visitDocumentCollection = function(collection) {},
    visitDocument = function(document) {}

  )
)
