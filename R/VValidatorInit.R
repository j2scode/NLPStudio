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
#'   \item{\code{nlpStudio(object)}}{Method for validating the instantiation of the NLPStudio object}
#'   \item{\code{lab(object)}}{Method for validating the instantiation of the Lab object}
#'   \item{\code{documentCollection(object)}}{Method for validating the instantiation of the DocumentCollection object.}
#'   \item{\code{documentText(object)}}{Method for validating the instantiation of the DocumentText object.}
#'   \item{\code{documentCsv(object)}}{Method for validating the instantiation of the DocumentCsv object.}
#'   \item{\code{documentRdata(object)}}{Method for validating the instantiation of the DocumentRdata object.}
#'   \item{\code{documentXlsx(object)}}{Method for validating the instantiation of the DocumentXlsx object.}
#' }
#'
#' @param object The object in its current state
#' @param ... Parameters
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorInit <- R6::R6Class(
  classname = "VValidatorInit",
  inherit = VValidator,
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    validateName = function(object) {

      name <- object$getName()

      # Confirm not missing
      if (is.null(name) | is.na(name)) {
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

      # Validate name is well-formed
      v <- ValidatorName$new()
      if (v$validate(class = class(object)[1], method = "initialize",
                     value = name, expect = FALSE) == FALSE) {
        return(FALSE)
      }
      return(TRUE)
    },

    validateParent = function(object, parentClasses) {

      parent <- object$parent()

      v <- ValidatorClass$new()
      if (v$validate(class = class(object)[1], method = "initialize",
                     fieldName = "parent", value = object, level = "Error",
                     msg = paste0("Cannot create ", class(object)[1],
                                  " object, ", name, ". Object of class ",
                                  class(object)[1], "can not have an ",
                                  "object of class ", class(parent)[1],
                                  " as a parent. ",
                                  "See ?", class(object)[1],
                                  " for further assistance"),
                     expect = parentClasses) == FALSE) {
        return(FALSE)
      }
      return(TRUE)
    },

    validateState = function(object) {
      o <- object$getObject(self)

      if (is.null(o$stateDesc) | is.na(o$stateDesc) | length(o$stateDesc) == 0) {
        v <- Validator0$new()
        v$notify(class = class(object)[1], method = "initialize", fieldName = "stateDesc",
                 value = "", level = "Error",
                 msg = paste0("State element is missing with no default. ",
                              "See ?", class(object)[1], " for further assistance."),
                 expect = NULL)
        return(FALSE)
      }
    },


    validateFileName = function(object, ext) {

      o <- object$getObject(self)

      if (is.null(o$fileName) | is.na(o$fileName) | length(o$fileName) == 0) {
        v <- Validator0$new()
        v$notify(class = class(object)[1], method = "initialize", fieldName = "fileName",
                 value = "", level = "Error",
                 msg = paste0("File name parameter is missing with no default. ",
                              "See ?", class(object)[1], " for further assistance."),
                 expect = NULL)
        return(FALSE)
      }

      if (!(file_ext(o$fileName) %in% ext)) {
        v <- Validator0$new()
        v$notify(class = class(object)[1], method = "initialize", fieldName = "fileName",
                 value = fileName, level = "Error",
                 msg = paste0("File type must be ", ext,
                              "See ?", class(object)[1], " for further assistance."),
                 expect = NULL)
        return(FALSE)
      }
      return(TRUE)
    }
  ),

  public = list(

    nlpStudio = function(object) {
      return(TRUE)
    },

    lab = function(object) {
      print("********************************")
      print(" visiting validatorinit visitor lab")
      return(private$validateName(object) &
               private$validateParent(object, "NLPStudio") &
               private$validateState(object)
             )
    },

    documentCollection = function(object) {
      return(private$validateName(object) &
               private$validateParent(object, c("Lab", "DocumentCollection")) &
               private$validateState(object)
      )
    },

    documentText = function(object) {
      return(private$validateName(object) &
               private$validateParent(object, "DocumentCollection") &
               private$validateState(object) &
               private$validateFileName(object, "txt")
      )
    },

    documentCsv = function(object) {
      return(private$validateName(object) &
               private$validateParent(object, "DocumentCollection") &
               private$validateState(object) &
               private$validateFileName(object, "csv")
      )
    },

    documentRdata = function(object) {
      return(private$validateName(object) &
               private$validateParent(object, "DocumentCollection") &
               private$validateState(object) &
               private$validateFileName(object, c("Rdata", "RData", "Rda"))
      )
    },

    documentXlsx = function(object) {
      return(private$validateName(object) &
               private$validateParent(object, "DocumentCollection") &
               private$validateState(object) &
               private$validateFileName(object, c("xlsx", "xls"))
      )
    }
  )
)
