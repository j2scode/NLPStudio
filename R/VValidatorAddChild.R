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

    ..parent = character(0),
    ..child = character(0),

    validate = function(classes, object) {

      # Confirm object and parent are a match
      if (private$..parent$getName() != object$getName()) {
        v <- Validator0$new()
        v$notify(class = class(object)[1], method = "addchild", fieldName = "parent",
                 value = "", level = "Error",
                 msg = paste0("Parent and visitor acceptor mismatch. ",
                              "See ?", class(self)[1], " for further assistance."),
                 expect = NULL)
        return(FALSE)
      }

      # Confirm class of child
      v <- ValidatorClass$new()
      if (v$validate(class = class(private$..parent)[1], method = "addChild",
                     fieldName = "child", value = private$..child, level = "Error",
                     msg = paste0("Unable to add ", class(private$..child)[1],
                                  " class object to an object of class ",
                                  class(private$..parent)[1], ".",
                                 "See ?", class(private$..parent)[1],
                                 " for further assistance."),
                     expect = classes) == FALSE) {
        return(FALSE)
      }
      return(TRUE)
    },

    validateDocument = function(object) {

      v <- Validator0$new()
      v$notify(class = class(private$..parent)[1], method = "addchild", fieldName = "child",
               value = "", level = "Error",
               msg = paste0("Unable to add children to objects of the ",
                            class(private$..parent)[1], " class. ",
                            "See ?", class(private$..parent)[1], " for further assistance."),
               expect = NULL)
      return(FALSE)
    }
  ),

  public = list(

    initialize = function(parent, child) {
      if(missing(parent)) {
        v <- Validator0$new()
        v$notify(class = class(self)[1], method = "addChild", fieldName = "parent",
                 value = "", level = "Error",
                 msg = paste0("Parent parameter missing with no default. "),
                 expect = NULL)
        return(FALSE)
      }
      if(missing(child)) {
        v <- Validator0$new()
        v$notify(class = class(self)[1], method = "addChild", fieldName = "child",
                 value = "", level = "Error",
                 msg = paste0("Child parameter missing with no default. "),
                 expect = NULL)
        return(FALSE)
      }

      private$..parent <- parent
      private$..child <- child

      invisible(self)
    },

    nlpStudio = function(object) {
      classes <- "Lab"
      return(private$validate(classes, object))
    },

    lab = function(object) {
      classes <- "DocumentCollection"
      return(private$validate(classes, object))
    },

    documentCollection = function(object) {
      classes <- c("DocumentCollection", "DocumentText", "DocumentCsv",
                   "DocumentRdata", "DocumentXlsx")
      return(private$validate(classes, object))
    },

    documentText = function(object) {
      return(private$validateDocument(object))
    },

    documentCsv = function(object) {
      return(private$validateDocument(object))
    },

    documentRdata = function(object) {
      return(private$validateDocument(object))
    },

    documentXlsx = function(object) {
      return(private$validateDocument(object))
    }
  )
)
