#==============================================================================#
#                               VisitorGetObject                               #
#==============================================================================#
#' VisitorGetObject
#'
#' \code{VisitorGetObject} Class for obtaining an object and its meta data.
#'
#' \strong{Visitor Family of Classes Overview:}
#'
#' The Visitor family of classes is an implementation of the visitor
#' pattern documented in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This pattern allows operations
#' to be defined without changing the clases of the elemnts upon which
#' it operates.
#'
#' \strong{Visitor Family of Classes Participants:}
#' \itemize{
#'  \item Visitor0: This component class specifies an abstract interface
#'  for all concrete visitors
#'  \item VisitorGetObject: This visitor class responsible for retrieving objects and meta data on the objects from concrete elements.
#'  \item VisitorPrintObject: Visitor class responsible for printing object meta data to the console.
#'  }
#'
#' \strong{VisitorGetObject Class Collaborators:}
#' The collaborators of the VisitorGetObject class are:
#' \itemize{
#'  \item Lab: Class responsible for managing collections of documents and analysis and processing workflows.
#'  \item DocumentCollection: Composite class containing documents.
#'  \item Documents: Class where each object represents a single document.
#'  }
#'
#' \strong{VisitorGetObject Class Methods:}
#' The methods areas follows:
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
#' @return object A list containing the object meta data as well as the object itself. The format is as follows:
#' \describe{
#'  \item{metaData}{The meta data for the object in list and data frame format.}
#'   \describe{
#'    \item{metaDataList}{Meta data in list format.}
#'    \item{metaDataDf}{Meta data in data frame format.}
#'    }
#'  \item{object}{The actual object }
#' }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @family Visitor classes
#' @export
VisitorGetObject <- R6::R6Class(
  classname = "VisitorGetObject",
  lock_objects = FALSE,
  lock_class = FALSE,

  public = list(

    visitLab = function(lab) {

      getList <- function() {
        lab = list(
          metaData = list(
            name = lab$private$..name,
            class = lab$private$..class,
            desc = lab$private$..desc,
            parentName = lab$private$..parentName,
            path = lab$private$..path,
            modified = lab$private$..modified,
            created = lab$private$..created
          ),
          documents = self$getDocuments(type = "list")
        )
        return(lab)
      }

      getDf <- function() {
        lab = list(
          metaData = data.frame(name = lab$private$..name,
                                class = lab$private$..class,
                                desc = lab$private$..desc,
                                parentName = lab$private$..parentName,
                                path = lab$private$..path,
                                modified = lab$private$..modified,
                                created = lab$private$..created,
                                stringsAsFactors = FALSE),
          documents = self$getDocuments(type = "df")
        )
        return(lab)
      }

      if (type == "object") {lab <- getObject()}
      else if (type == "list") {lab <- getList()}
      else if (type == "df") {lab <- getDf()}
      else {
        v <- Validate0$new()
        v$notify(cls = "Lab", method = "getLab",
                 fieldName = "type", value = type, level = "Warn",
                 msg = paste("Invalid format requested.",
                             "Must be 'object', 'list', or 'df'.",
                             "Lab is returned in 'list' format",
                             "See ?Lab"),
                 expect = NULL)
        lab <- getList()
      }

      return(lab)
    },
    visitCollection = function(collection) stop("Method is not available from Visitor0, an interface class!"),
    visitDocument = function(document) stop("Method is not available from Visitor0, an interface class!")

  )
)
