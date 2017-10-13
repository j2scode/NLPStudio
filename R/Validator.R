#==============================================================================#
#                                 Validator                                    #
#==============================================================================#
#' Validator
#'
#' @description
#' \code{Validator} Class responsible for validation of objects and parameters.
#'
#' This class contains the parameter validation methods for objects. Methods
#' provide validation of parameters for initialization, composite, state,
#' and read/write operations.
#'
#'
#' \strong{Validator Methods:}
#' \describe{
#'  \item{\code{new()}}{Creates an object of Validator class}
#'  \item{\code{init(object)}}{Dispatches the initialization validation visitor, via the accept method of the originator.}
#'  \item{\code{getObject(object)}}{Dispatches the getObject validation visitor, via the accept method of the originator.}
#'  \item{\code{addChild(object)}}{Dispatches the addChild validation visitor, via the accept method of the originator.}
#'  \item{\code{removeChild(object)}}{Dispatches the removeChild validation visitor, via the accept method of the originator.}
#'  \item{\code{restore(object)}}{Dispatches the restore validation visitor, via the accept method of the originator.}
#'  \item{\code{readState(object)}}{Dispatches the readState validation visitor, via the accept method of the originator.}
#'  \item{\code{writeState(object)}}{Dispatches the writeState validation visitor, via the accept method of the originator.}
#'  \item{\code{read(object)}}{Dispatches the read validation visitor, via the accept method of the originator.}
#'  \item{\code{write(object)}}{Dispatches the write validation visitor, via the accept method of the originator.}
#' }
#'
#' \strong{Validator Parameters:}
#' @param object Originator object
#' @param ... Parameters being validated
#'
#' @return A logical, TRUE if validation criteria are met, FALSE, otherwise.
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @docType class
#' @family Validation Classes
#' @export
Validator <- R6::R6Class(
  "Validator",
  lock_class = FALSE,
  lock_objects = FALSE,

  public = list(

    init = function(object) {
      visitor <- VValidatorInit$new()
      object$accept(visitor)
    },
    getObject = function(object) {
      visitor <- VValidatorGetObject$new()
      object$accept(visitor)
    },
    addChild = function(object, child) {
      visitor <- VValidatorAddChild$new(object, child)
      object$accept(visitor)
    },
    removeChild = function(object, child) {
      visitor <- VValidatorRemoveChild$new(object, child)
      object$accept(visitor)
    },
    setParent = function(object, parent) {
      visitor <- VValidatorSetParent$new(object, parent)
      object$accept(visitor)
    },
    restore = function(object, state) {
      visitor <- VValidatorRestore$new(object, state)
      object$accept(visitor)
    },
    read = function(object) {
      visitor <- VValidatorRead$new(object)
      object$accept(visitor)
    },
    write = function(object, content) {
      visitor <- VValidatorWrite$new(object, content)
      object$accept(visitor)
    }
  )
)
