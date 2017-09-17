#==============================================================================#
#                                   Reader                                     #
#==============================================================================#
#' Reader
#'
#'
#' \code{Reader} Class responsible for performing read functionality through
#' the document and document collection composite hierarchy. This class has
#' a single method that, upon instantiation, initiates a VReader object,
#' passes this visitor object to the document's accept method. The document
#' accepts the VReader visitor and the VReader processes the request or
#' dispatches another read request on a composite object.
#'
#' \strong{Reader Class Overview:}
#' The Reader class is a participant in the VReader visitor pattern, a design
#' pattern described in the book "Design Patterns: Elements of Reusable
#' Object-Oriented Software" by Erich Gamma, Richard Helm, Ralph Johnson
#' and John Vlissides (hence Gang of Four). This visitor pattern allows
#' new operations to be defined without changing the classes upon which
#' the visitor method operates.
#'
#'
#' \strong{Reader Methods:}
#' The Reader class has a single method as follows:
#'  \itemize{
#'   \item{\code{readData(object)}}{Method responsible for performing read functionality within the Document composite hierarchy.}
#' }
#'
#' @param object This is an object of the DocumentCollection, Document or File class
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @export
Reader <- R6::R6Class(
  classname = "Reader",
  public = list(

    readData = function(object) {

      vReader <- VReader$new()
      object$acceptVReader(vReader)

    }
  )
)
