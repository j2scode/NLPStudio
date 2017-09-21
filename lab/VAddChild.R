#==============================================================================#
#                                   VAddChild                                       #
#==============================================================================#
#' VAddChild
#'
#'
#' \code{VAddChild} Visitor class responsible for adding a child object to a parent
#'
#' \strong{VAddChild Methods:}
#' The VAddChild methods are as follows:
#'  \itemize{
#'   \item{\code{lab(object)}}{Method for adding a DocumentCollection object to a Lab object}
#'   \item{\code{documentCollection(object)}}{Method for adding a Document or DocumentCollection object to a DocumentCollection object}
#'   \item{\code{document(object)}}{Not implemented as a Document class object cannot have child objects.}
#' }
#'
#' @param parent The parent object
#' @param child The child object
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family State Classes
#' @export
VAddChild <- R6::R6Class(
  classname = "VAddChild",

  public = list(

    lab = function(parent, child)  {
      parent$addChild(child)
    },
    documentCollection = function(parent, child)  {
      parent$addChild(child)
    },
    document = function(parent, child)  {
      v <- Validate0$new()
      v$notify(class = "VAddChild", method = "document",
               fieldName = "parent", value = class(parent)[1], level = "Error",
               msg = paste("Unable to add child to a document class object."),
               expect = NULL)
      stop()
    }
  )
)
