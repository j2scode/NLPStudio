#==============================================================================#
#                                 Validator                                    #
#==============================================================================#
#' Validator
#'
#' @description
#' \code{Validator} Class responsible for validation of requests pertaining to:
#' \itemize{
#'  \item Object Create and Read: Requests to create and read object internal representation.
#'  \item Composition and Aggregation: Requests to manipulate aggregate and composite objects
#'  \item History: Requests to read historical records for an object or objects.#'
#'  \item State: Requests to read, save, and restore object states
#'  \item Document I/O: Requests to read and write documents.
#' }
#'
#' @section Validator methods:
#' This section summarizes the methods in the Validator class.
#'
#' \strong{Object Create and Read Methods:}
#' \describe{
#'  \item{\code{new()}}{Creates an object of Validator class}
#'  \item{\code{init(object)}}{Dispatches the initialization validation visitor, via the accept method of object.}
#'  \item{\code{exposeObject(object)}}{Dispatches the exposeObject validation visitor, via the accept method of object.}
#'  }
#'
#' \strong{Composition and Aggregation Methods:}
#' \describe{
#'  \item{\code{addChild(object)}}{Dispatches the addChild validation visitor, via the accept method of object.}
#'  \item{\code{removeChild(object)}}{Dispatches the removeChild validation visitor, via the accept method of object.}
#'  \item{\code{setParent(object, parent)}}{Dispatches the setParent validation visitor, via the accept method of object.}
#'  }
#'
#' \strong{Curator Methods:}
#' \describe{
#'  \item{\code{setQueryTarget(object, target)}}{Dispatches the setQueryTarget validation visitor, via the accept method of object.}
#'  \item{\code{submitQuery(object)}}{Dispatches the removeChild validation visitor, via the accept method of object.}
#'  }
#'
#' \strong{History Methods:}
#' \describe{
#'  \item{\code{readHistory(object)}}{Dispatches the readHistory validation visitor, via the accept method of object.}
#'  }
#'
#' \strong{State Methods:}
#' \describe{
#'  \item{\code{readStates(object)}}{Dispatches the readStates validation visitor, via the accept method of object.}
#'  \item{\code{saveState(object)}}{Dispatches the saveState validation visitor, via the accept method of object.}
#'  \item{\code{restoreState(object)}}{Dispatches the restoreState validation visitor, via the accept method of object.}
#'  }
#'
#' \strong{Document I/O Methods:}
#' \describe{
#'  \item{\code{read(object)}}{Dispatches the read validation visitor, via the accept method of object.}
#'  \item{\code{write(object)}}{Dispatches the write validation visitor, via the accept method of object.}
#' }
#'
#' @section Validation Class Parameters
#' @param object Originator object
#' @param target Object which is the target of a query. Used by the Curator methods
#' @param content Character vector containing content to be written by a write originator.
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

    #-------------------------------------------------------------------------#
    #                      Object Creation and Read                           #
    #-------------------------------------------------------------------------#
    init = function(object) {
      visitor <- VValidatorInit$new()
      object$accept(visitor)
    },
    exposeObject = function(object) {
      visitor <- VValidatorexposeObject$new()
      object$accept(visitor)
    },

    #-------------------------------------------------------------------------#
    #                   Composition and Aggregation                           #
    #-------------------------------------------------------------------------#
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

    #-------------------------------------------------------------------------#
    #                          Curator (Query)                                #
    #-------------------------------------------------------------------------#
    setQueryTarget= function(object, target) {
      visitor <- VValidatorSetQueryTarget$new(object, target)
      object$accept(visitor)
    },
    submitQuery = function(object) {
      visitor <- VValidatorSubmitQuery$new(object)
      object$accept(visitor)
    },

    #-------------------------------------------------------------------------#
    #                                History                                  #
    #-------------------------------------------------------------------------#
    readHistory = function(object) {
      visitor <- VValidatorReadHistory$new(object)
      object$accept(visitor)
    },

    #-------------------------------------------------------------------------#
    #                                State                                    #
    #-------------------------------------------------------------------------#
    readStates = function(object) {
      visitor <- VValidatorReadStates$new(object)
      object$accept(visitor)
    },

    saveState = function(object) {
      visitor <- VValidatorSaveState$new(object)
      object$accept(visitor)
    },

    restoreState = function(object) {
      visitor <- VValidatorRestoreState$new(object)
      object$accept(visitor)
    },

    #-------------------------------------------------------------------------#
    #                              Document I/O                               #
    #-------------------------------------------------------------------------#
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
