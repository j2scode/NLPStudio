#==============================================================================#
#                                   VRemoveChild                                    #
#==============================================================================#
#' VRemoveChild
#'
#'
#' \code{VRemoveChild} Visitor class responsible for removing a child object from a parent
#'
#' \strong{VRemoveChild Methods:}
#' The VRemoveChild methods are as follows:
#'  \itemize{
#'   \item{\code{lab(object)}}{Method for removing a DocumentCollection class object from a Lab class object.}
#'   \item{\code{documentCollection(object)}}{Method for removing a Document or DocumentCollection class object from a DocumentCollection class object.}
#'   \item{\code{document(object)}}{Not implemented as Document class objects cannot have child objects.}
#' }
#'
#' @param parent The parent object
#' @param child The child object
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family State Classes
#' @export
VRemoveChild <- R6::R6Class(
  classname = "VRemoveChild",

  public = list(

    lab = function(parent, child)  {
      parent$addChild(child)
    },
    documentCollection = function(parent, child)  {
      parent$addChild(child)
    },
    document = function(parent, child)  {
      v <- Validator0$new()
      v$notify(class = "VRemoveChild", method = "document",
               fieldName = "parent", value = class(parent)[1], level = "Error",
               msg = paste("Unable to add child to a document class object."),
               expect = NULL)
      stop()
    }
  )
)
