#==============================================================================#
#                                   Visitor0                                   #
#==============================================================================#
#' Visitor0
#'
#' \code{Visitor0} Interface class which defines a visit operation for each class
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
#'  \item VisitorGetObject: Visitor class responsible for retrieving objects and meta data on the objects from concrete elements.
#'  \item VisitorPrintObject: Visitor class responsible for printing object meta data to the console.
#'  \item VisitorSave: Visitor class responsible for saving snapshots of objects within an object hierarchy.
#'  \item VisitorRestore: Visitor class responsible for restoring snapshots of objects within an object hierarchy.
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
#' @family Visitor classes
#' @export
Visitor0 <- R6::R6Class(
  classname = "Visitor0",
  lock_objects = FALSE,
  lock_class = FALSE,

  public = list(

    visitLab = function(lab) stop("Method is not available from Visitor0, an interface class!"),
    visitCollection = function(collection) stop("Method is not available from Visitor0, an interface class!"),
    visitDocument = function(document) stop("Method is not available from Visitor0, an interface class!")

  )
)
