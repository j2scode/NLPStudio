#==============================================================================#
#                                   VValidatorInit                             #
#==============================================================================#
#' VValidatorInit
#'
#'
#' \code{VValidatorInit} Visitor class responsible for validating the initialization objects of all classes
#'
#' \strong{VValidatorInit Methods:}
#' The VValidatorInit methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object, ...)}}{Method for validating the instantiation of the NLPStudio object}
#'   \item{\code{lab(object, ...)}}{Method for validating the instantiation of the Lab object}
#'   \item{\code{documentCollection(object, ...)}}{Method for validating the instantiation of the DocumentCollection object.}
#'   \item{\code{documentText(object, ...)}}{Method for validating the instantiation of the DocumentText object.}
#'   \item{\code{documentCsv(object, ...)}}{Method for validating the instantiation of the DocumentCsv object.}
#'   \item{\code{documentRdata(object, ...)}}{Method for validating the instantiation of the DocumentRdata object.}
#'   \item{\code{documentXlsx(object, ...)}}{Method for validating the instantiation of the DocumentXlsx object.}
#' }
#'
#' @param object The object in its current state
#' @param ... Parameters
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Classes
#' @export
VValidatorInit <- R6::R6Class(
  classname = "VValidatorInit",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    validate = function(object, ...) {
      if (private$validateName(object, ...) == TRUE) {
        return(private$validateFileName(object, ...))
      } else(
        return(FALSE)
      )
    },

    validateName = function(object, name) {

      # Confirm required parameters are not missing.
      if (missing(name)) {
        v <- Validator0$new()
        v$notify(class = class(object)[1], method = "initialize", fieldName = "name",
                 value = "", level = "Error",
                 msg = paste0("Name parameter is missing with no default. ",
                              "See ?", class(object)[1], " for further assistance."),
                 expect = NULL)
        return(FALSE)
      }

      # Confirm object doesn't already exist.
      v <- ValidatorExists$new()
      if (v$validate(class = class(object)[1], method = "initialize",
                     fieldName = "name", value = name, level = "Error",
                     msg = paste0("Cannot create ", class(object)[1],
                                 " object. ", name, " already exists. ",
                                 "See ?", class(object)[1],
                                 " for further assistance"),
                     expect = FALSE) == FALSE) {
        return(FALSE)
      }

      # Validate name
      v <- ValidatorName$new()
      if (v$validate(class = class(object)[1], method = "initialize",
                     value = name, expect = FALSE) == FALSE) {
        return(FALSE)
      }

      return(TRUE)
    },

    validateFileName = function(object, ...) {

      if (missing(fileName)) {
        v <- Validator0$new()
        v$notify(class = class(object)[1], method = "initialize", fieldName = "fileName",
                 value = "", level = "Error",
                 msg = paste0("File name parameter is missing with no default. ",
                              "See ?", class(object)[1], " for further assistance."),
                 expect = NULL)
        return(FALSE)
      }
      return(TRUE)
    }
  ),

  public = list(

    nlpStudio = function(object,...) {
      return(TRUE)
    },

    lab = function(object,...) {
      return(private$validateName(object, ...))
    },

    documentCollection = function(object,...) {
      return(private$validateName(object, ...))
    },

    documentText = function(object,...) {
      return(private$validate(object, ...))
    },

    documentCsv = function(object,...) {
      return(private$validate(object, ...))
    },

    documentRdata = function(object,...) {
      return(private$validate(object, ...))
    },

    documentXlsx = function(object,...) {
      return(private$validate(object, ...))
    }
  )
)
