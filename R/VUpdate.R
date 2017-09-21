#==============================================================================#
#                                   VUpdate                                    #
#==============================================================================#
#' VUpdate
#'
#'
#' \code{VUpdate} Visitor class responsible for updating an object to a prior stete
#'
#' \strong{VUpdate Methods:}
#' The VUpdate methods are as follows:
#'  \itemize{
#'   \item{\code{lab(object)}}{Method for updating a Lab object to its current state.}
#'   \item{\code{documentCollection(object)}}{Method for updating a DocumentCollection object to its current state.}
#'   \item{\code{document(object)}}{Method for updating a Document object to its current state.}
#' }
#'
#' @param current The object in its current state
#' @param restored The object restored from a prior state.
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family State Classes
#' @export
VUpdate <- R6::R6Class(
  classname = "VUpdate",
  private = list(

    validateObject = function(current, restored) {

      v <- ValidateClass$new()
      if (v$validate(class = "VUpdate", method = method, fieldName = "class(restored)",
                     level = "Error", value = restored,
                     msg = paste0("Unable to restore object of class ",
                                 class(current)[1], " to state of ",
                                 "object class ", class(restored)[1], ". ",
                                 "See ?VUpdate for assistance."),
                     expect = class(current)[1]) == FALSE) {
        stop()
      }
    }
  ),
  public = list(

    lab = function(current, restored)  {
      private$..validateObject(current, restored)
      current$setObject(self, restored)
    },
    documentCollection = function(current, restored)  {
      private$..validateObject(current, restored)
      current$setObject(self, restored)
    },
    document = function(current, restored)  {
      private$..validateObject(current, restored)
      current$setObject(self, restored)
    }
  )
)
