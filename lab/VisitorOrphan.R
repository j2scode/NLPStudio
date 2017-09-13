#==============================================================================#
#                                   VisitorOrphan                              #
#==============================================================================#
#' VisitorOrphan
#'
#' \code{VisitorOrphan} Interface class which defines a visit operation for each class
#' of concrete elements within NLPStudio
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
#'
#' \strong{Visitor Class Family Collaborators:}
#' The collaborators of the VisitorOrphan class are:
#' \itemize{
#'  \item Lab: Class for creating and managing documents and workflows.
#'  \item DocumentCollection: A composite class participant of the Document0 composite class which creates and manages collections of documents
#'  \item Document: A leaf class participant of the Document0 composite class. Elements include individual documents.
#'  }
#'
#' The methods are as follows:
#' \itemize{
#'  \item{\code{visitLab(lab)}}{Base method for visiting a object of the Lab class.}
#'  \item{\code{visitCollection(collection)}}{Base method visiting a object of the DocumentCollection class.}
#'  \item{\code{visitDocument(document)}}{Base method visiting a object of the Document class.}
#' }
#'
#' @param lab An object of the Lab class.
#' @param collection An object of the DocumentCollection class.
#' @param document An object of the Document class.
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Visitor Classes
#' @export
VisitorOrphan <- R6::R6Class(
  classname = "VisitorOrphan",
  inherit = Visitor0,
  lock_objects = FALSE,
  lock_class = FALSE,

  public = list(

    orphan = function(object) {
      object$accept(self)
    },

    visitDocumentCollection = function(collection) {

      # Create orphan directory if it doesn't already exist
      dirs <- nlpStudio$getPaths()
      if (!dir.exists(dirs$OrphanCollections)) {
        dir.create(dirs$OrphanCollections)
      }

      # Move files to orphan directory and delete old delete.
      c <- collection$getObject
      file.copy(c$path, dirs$OrphanCollections)
      base::unlink(c$path)

    },

    visitDocument = function(document) {

      # Create orphan directory if it doesn't already exist
      dirs <- nlpStudio$getPaths()
      if (!dir.exists(dirs$orphanCollection)) {
        dir.create(dirs$orphanCollection)
      }

      # Move files to orphan directory and delete old directory
      c <- collection$getObject
      file.copy(c$path, dirs$orphanCollection)
      base::unlink(c$path)

    }
  )
)
