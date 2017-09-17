#==============================================================================#
#                                   Writer                                     #
#==============================================================================#
#' Writer
#'
#'
#' \code{Writer} Class responsible for performing write functionality through
#' the document and document collection composite hierarchy. This class has
#' a single method that, upon instantiation, initiates a VWriter object,
#' passes this visitor object to the document's accept method. The document
#' accepts the VWriter visitor and the VWriter processes the request or
#' dispatches another write request on a composite object.
#'
#' \strong{Writer Class Overview:}
#' The Writer class is a participant in the VWriter visitor pattern, a design
#' pattern described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This visitor pattern allows
#' new operations to be defined without changing the classes upon which
#' the visitor method operates.
#'
#'
#' \strong{Writer Methods:}
#' The Writer class has a single method as follows:
#'  \itemize{
#'   \item{\code{writeData(object)}}{Method responsible for performing write functionality within the Document composite hierarchy.}
#' }
#'
#' @param object This is an object of the DocumentCollection, Document or File class
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @export
Writer <- R6::R6Class(
  classname = "Writer",
  public = list(

    writeData = function(object) {

      vWriter <- VWriter$new()
      object$acceptVWriter(vWriter)

    }
  )
)
