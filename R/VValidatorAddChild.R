#==============================================================================#
#                                   VValidatorAddChild                             #
#==============================================================================#
#' VValidatorAddChild
#'
#'
#' \code{VValidatorAddChild} Visitor class responsible for validating the parameters for the addChild methods
#' of aggregate and composite classes.
#'
#' \strong{VValidatorAddChild Methods:}
#' The VValidatorAddChild methods are as follows:
#'  \itemize{
#'   \item{\code{nlpStudio(object, ...)}}{Method for validating the addChild method parameters of the NLPStudio object}
#'   \item{\code{lab(object, ...)}}{Method for validating the addChild method parameters of the Lab object}
#'   \item{\code{documentCollection(object, ...)}}{Method for validating the addChild method parameters of the DocumentCollection object.}
#'   \item{\code{documentText(object, ...)}}{Method for validating the addChild method parameters of the DocumentText object.}
#'   \item{\code{documentCsv(object, ...)}}{Method for validating the addChild method parameters of the DocumentCsv object.}
#'   \item{\code{documentRdata(object, ...)}}{Method for validating the addChild method parameters of the DocumentRdata object.}
#'   \item{\code{documentXlsx(object, ...)}}{Method for validating the addChild method parameters of the DocumentXlsx object.}
#' }
#'
#' @param object The object in its current state
#' @param ... Parameters
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Classes
#' @export
VValidatorAddChild <- R6::R6Class(
  classname = "VValidatorAddChild",
  lock_objects = FALSE,
  lock_class = FALSE,

  private = list(

    validate = function(classes, object, child) {

      # Confirm required parameters are not missing.
      if (missing(child)) {
        v <- Validator0$new()
        v$notify(class = class(object)[1], method = "addChild", fieldName = "child",
                 value = "", level = "Error",
                 msg = paste0("Child parameter is missing with no default. ",
                              "See ?", class(object)[1], " for further assistance."),
                 expect = NULL)
        return(FALSE)
      }

      # Confirm class of child
      name <- child$getName()
      v <- ValidatorClass$new()
      if (v$validate(class = class(object)[1], method = "addChild",
                     fieldName = "child", value = child, level = "Error",
                     msg = paste0("Unable to add ", class(child)[1],
                                  " class object ", name,
                                  ". to an object of class ",
                                  class(object)[1], ".",
                                 "See ?", class(object)[1],
                                 " for further assistance."),
                     expect = classes) == FALSE) {
        return(FALSE)
      }
    },

    validateDocument = function(object, ...) {
      v <- Validator0$new()
      v$notify(class = class(object)[1], method = "addChild", fieldName = "child",
               value = "", level = "Error",
               msg = paste0("Unable to add children to objects of the ",
                            class(object)[1], " class. ",
                            "See ?", class(object)[1], " for further assistance."),
               expect = NULL)
      return(FALSE)
    }
  ),

  public = list(

    nlpStudio = function(object, child) {
      classes <- "Lab"
      return(private$validate(classes = classes, object = object, child))
    },

    lab = function(object,...) {
      classes <- "DocumentCollection"
      return(private$validate(classes = classes, object = object, child))
    },

    documentCollection = function(object,...) {
      classes <- c("DocumentCollection", "DocumentText", "DocumentCsv",
                   "DocumentRdata", "DocumentXlsx")
      return(private$validate(classes = classes, object = object, child))
    },

    documentText = function(object,...) {
      return(private$validateDocument(object, ...))
    },

    documentCsv = function(object,...) {
      return(private$validateDocument(object, ...))
    },

    documentRdata = function(object,...) {
      return(private$validateDocument(object, ...))
    },

    documentXlsx = function(object,...) {
      return(private$validateDocument(object, ...))
    }
  )
)
