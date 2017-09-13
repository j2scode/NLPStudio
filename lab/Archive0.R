## ---- Singleton
#==============================================================================#
#                                 Singleton                                    #
#==============================================================================#
#' Archive0
#'
#'
#' \code{Archive0} Singleton class for Archive subclass.
#'
#' This class ...
#'
#' @docType class
#' @section Methods:
#' \describe{
#'  \item{\code{initialize(...)}}{Initializes a Singleton object}
#'  \item{\code{getInstance(...)}}{Gets instance of singleton object }
#' }
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Archive0 <- R6::R6Class(
  "Archive0",
  portable = FALSE,
  private = list(
    Class = "ANY",
    instance = "ANY"
    ),
  public = list(
    initialize = function(...) {
      private$instance <<- NULL
    },
    getInstance = function(...) {
      if(is.null(private$instance)) private$instance <<- Class$new(...)
      return(private$instance)
    }
  )
)
