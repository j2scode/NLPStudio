## ---- NLPStudio
#==============================================================================#
#                                 NLPStudio                                    #
#==============================================================================#
#' NLPStudio
#'
#' \code{NLPStudio} Class the creates and manages labs.
#'
#' This class creates and manages data labs  A data lab is
#' essentially a directory in which project data reside.  Multiple data
#' labs can be created to house separate versions of the data
#' for analysis and management.
#'
#' Note: This class is a singleton pattern. An NLPStudio object called
#'       nlpStudio is  instantiated at load time.  Any subsequent
#'       initializations will return the nlpStudio instance.
#'
#' @docType class
#' @examples
#' \dontrun{
#' nlpStudio$getStudio()
#' nlpStudio$archiveLab("Main")
#' nlpStudio$removeLab("Main")
#' nlpStudio$searchStudio("Main")
#' nlpStudio$listlabs()
#' nlpStudio$addLab("Alpha", "Alpha Test Lab", FALSE)
#' nlpStudio$currentLab <- "Alpha"
#' nlpStudio$listlabs()
#' }
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Initializes the NLPStudio. Note, an instance called nlpStudio is created at load time}
#'  \item{\code{getLab()}}{Retrieves current studio object, returns in list format and prints to console}
#'  \item{\code{searchLab(name)}}{Searches the studio for a named lab}
#'  \item{\code{archiveLab(name)}}{Archives the labs in the studio}
#'  \item{\code{addLab(name, desc, current)}}{Creates an lab and sets its current status to TRUE or FALSE}
#'  \item{\code{listlabs()}}{Returns a data frame of labs in the NLPStudio and prints them to console}
#'  \item{\code{removeLab(name)}}{Removes an lab (after archiving it)}
#' }
#'
#' @section Parameters:
#' @param name A character string containing the studio name
#' @param desc A character string containing the studio description
#'
#' @section Active Bindings:
#' \describe{
#'  \item{\code{currentLab}}{Gets and sets the current lab}
#' }
#'
#'
#' @author John James, \email{j2sdatalab@@gmail.com}
#' @export
NLPStudio <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "NLPStudio",
        private = list(
          ..cls = "NLPStudio",
          ..name = character(0),
          ..studioDirs = list(
            archive = "./Archive",
            labs = "./Labs"),
          ..labList = list(),
          ..currentLab = "None",
          ..created = "None",
          ..modified = "None"
        ),

        active = list(

          currentLab = function(lab) {

            if (missing(lab)) {
              private$..currentLab

            } else {

            method <- "setLabCurrent"
            v <- ValidateExists$new()
            v$validate(cls = cls, method = method,
                       fieldName = "lab", value = lab, level = "Error",
                       msg = paste("Invalid lab,", lab, "does not exist."),
                       expect = TRUE)

            private$..currentLab <- lab
            private$..modified <- Sys.time()

            }
          }
        ),

        public = list(

          desc = character(0),

          initialize = function() {

            name <- "nlpStudio"
            desc <- "NLPStudio: Natural Language Processing Studio"

            # Suppress automatically generated error messages
            opt <- options(show.error.messages=FALSE, warn = -1)
            on.exit(options(opt))

            # Create archive and lab folders
            suppressWarnings(dir.create(private$..studioDirs$archive))
            suppressWarnings(dir.create(private$..studioDirs$labs))

            # Create single instance of NLPStudio object
            private$..name <- name
            self$desc <- desc
            private$..currentLab <- "None"
            private$..modified <- Sys.time()
            private$..created <- Sys.time()

            invisible(self)
          },

          getInstance = function() {
            invisible(self)
          },

          getStudio = function(verbose = FALSE) {
            studio = list(
              name = private$..name,
              desc = self$desc,
              labList = self$listlabs(),
              currentLab = private$..currentLab,
              modified = private$..modified,
              created = private$..created
            )

            if (verbose == TRUE) {
              cat("\n\n================================================================================",
                  "\nNLPStudio: ", studio$name, " ", studio$desc, " ",
                  "\nCurrent Lab: ",studio$currentLab, " Created: ",
                  format(studio$created), "Modified:", format(studio$modified))

              cat("\n---------------------------------------------------------------------------------",
                  "\nLabs:\n")
              print.data.frame(studio$labs)
              cat("\n================================================================================\n")
            }

            return(studio)
          },

          addLab = function(lab, current = FALSE) {

            labData <- lab$getLab()

            # Validation
            v <- ValidateExists$new()
            v$validate(cls = "NLPStudio", method = "addLab",
                       fieldName = "lab", value = labData$name, level = "Error",
                       msg = paste("Invalid lab,", lab, "does not exist."),
                       expect = TRUE)

            v <- ValidateLogical$new()
            v$validate(cls = "NLPStudio", method = "addLab",
                       fieldName = "current", value = current, level = "Error",
                       msg = paste("Invalid logical,", current, "must be TRUE or FALSE"),
                       expect = TRUE)

            # Add lab to lab list
            if (length(private$..labsList) == 0) {
              private$..labList <- list(lab)
            } else {
              private$..labList <- list(private$..labList, list(lab))
            }

            private$..modified <- Sys.time()

            invisible(self)

          },

          removeLab = function(lab) {

            # Validate
            e <- ValidateExists$new()
            e$validate(cls = "NLPStudio", method = "removeLab",
                       fieldName = "lab", value = lab, level = "Error",
                       msg = paste("Invalid lab,", lab, "does not exist."),
                       expect = TRUE)

            #lab$archive()

            #rm(lab, envir = .GlobalEnv)

            # private$..modified <- Sys.time()

            # TODO: Confirm lab is removed from list of labs during testing
          },

          listlabs = function(verbose = FALSE) {

            labs = rbindlist(lapply(private$..labList, function(e) {
              lab <- list(
                name = e$private$..name,
                desc = e$private$..desc,
                current = e$private$..currentLab,
                created = e$private$..created
              )
              lab
            }))

            if (verbose == TRUE) {
              print.data.frame(labs)
            }
            return(labs)
          }
        )
      )
      super$initialize(...)
    }
  )
)#$new()
