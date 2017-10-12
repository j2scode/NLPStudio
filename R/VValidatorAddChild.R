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
#'   \item{\code{nlpStudio(object, child)}}{Method for validating the addChild method parameters of the NLPStudio object}
#'   \item{\code{lab(object, child)}}{Method for validating the addChild method parameters of the Lab object}
#'   \item{\code{documentCollection(object, child)}}{Method for validating the addChild method parameters of the DocumentCollection object.}
#'   \item{\code{documentText(object, child)}}{Method for validating the addChild method parameters of the DocumentText object.}
#'   \item{\code{documentCsv(object, child)}}{Method for validating the addChild method parameters of the DocumentCsv object.}
#'   \item{\code{documentRdata(object, child)}}{Method for validating the addChild method parameters of the DocumentRdata object.}
#'   \item{\code{documentXlsx(object, child)}}{Method for validating the addChild method parameters of the DocumentXlsx object.}
#' }
#'
#' @param object The parent object
#' @param child  The child object
#'
#' @docType class
#' @author John James, \email{jjames@@DataScienceSalon.org}
#' @family Validation Visitor Classes
#' @export
VValidatorAddChild <- R6::R6Class(
  classname = "VValidatorAddChild",
  inherit = VValidator,
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

    validateDocument = function(object, child) {
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
      return(private$validate(object, child))
    },

    lab = function(object,child) {
      classes <- "DocumentCollection"
      return(private$validate(object, child))
    },

    documentCollection = function(object,child) {
      classes <- c("DocumentCollection", "DocumentText", "DocumentCsv",
                   "DocumentRdata", "DocumentXlsx")
      return(private$validate(object, child))
    },

    documentText = function(object,child) {
      return(private$validateDocument(object, child))
    },

    documentCsv = function(object,child) {
      return(private$validateDocument(object, child))
    },

    documentRdata = function(object,child) {
      return(private$validateDocument(object, child))
    },

    documentXlsx = function(object,child) {
      return(private$validateDocument(object, child))
    }
  )
)
