## ---- NLPStudio
#==============================================================================#
#                                 NLPStudio                                    #
#==============================================================================#
#' NLPStudio
#'
#' \code{NLPStudio} Class the creates and manages labs.
#'
#' This class creates and manages data labs  A data lab is essentially a
#' directory in which project data reside.  Multiple data labs can be created
#' to house separate versions of the data for analysis. Note: This class is a
#' singleton pattern. An NLPStudio object called nlpStudio is instantiated at
#' load time.  Any subsequent initializations will return the single nlpStudio
#' instance.
#'
#' @field nlpStudio: The name of the single NLPStudio object created at load time.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Initializes the NLPStudio. This is a singleton class in which its only object is created when the package is loaded.}
#'  \item{\code{getStudio()}}{Retrieves current studio object, returns in list format and prints to console}
#'  \item{\code{addLab(lab, current)}}{Adds an existing lab to the NLPStudio object list of labs.  If current is set to TRUE, it is also assigned to the 'currentLab' variable.}
#'  \item{\code{removeLab()}}{Method which archives the lab and removes it from the global environment and from cache.}
#'  \item{\code{listlabs()}}{Returns a data frame of labs in the NLPStudio and prints them to console}
#' }
#'
#' @section Parameters:
#' @param name A character string containing the studio name
#' @param desc A character string containing the studio description
#' @param current A logical indicating whether to set the lab current.
#' @param lab An object of class 'Lab'
#'
#' @section Active Bindings:
#' \describe{
#'  \item{\code{desc()}}{An active binding that gets and sets the NLPStudio description}
#'  \item{\code{current()}}{An active binding that gets and sets the current lab. With the exception of the cross-lab classes, all objects act on the current lab. }
#' }
#'
#' @docType class
#' @examples
#' \dontrun{
#' nlpStudio$getStudio()
#' nlpStudio$desc <- "Homer's NLPStudio"  # Changes the description of the studio
#' nlpStudio$addLab("Lisa", current = FALSE)
#' nlpStudio$addLab("Bart", current = TRUE)
#' nlpStudio$removeLab("Bart") # Fails: Cannot remove a current lab
#' nlpStudio$currentLab <- "Lisa"
#' nlpStudio$removeLab("Bart") # Success!
#' nlpStudio$listlabs()  # Renders a list of labs to console and to data frame
#' }
#'
#' @author John James, \email{jjames@@datasciencesalon.org}
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
          ..name = "nlpStudio",
          ..desc = "NLPStudio: Natural Language Processing Studio",
          ..studioDirs = list(
            archive = "./Archive",
            labs = "./Labs"),
          ..labList = list(),
          ..currentLab = "None",
          ..created = "None",
          ..modified = "None"
        ),

        active = list(

          desc = function(value) {
            if (missing(value)) {
              private$..desc
            } else {
            private$..desc <- value
            }
          },

          currentLab = function(lab) {

            if (missing(lab)) {
              private$..currentLab

            } else {

              v <- ValidateClass$new()
              v$validate(cls = "NLPStudio", method = "currentLab",
                         fieldName = "lab", value = lab, level = "Error",
                         msg = "Object is not of type 'Lab' ",
                         expect = "Lab")

              labData <- lab$getLab(verbose = FALSE)

              if (identical(private$..currentLab, lab)) {
                v <- Validate0$new()
                v$notify(cls = "NLPStudio", method = "currentLab",
                         fieldName = "lab", value = labData$name, level = "Warn",
                         msg = paste(labData$name, "is already current."),
                         expect = "Lab")
              } else {
                private$..currentLab <- lab
                private$..modified <- Sys.time()

                # Update Cache
                nlpStudioCache$setCache("nlpStudio", self)

              }
            }
          }
        ),

        public = list(

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
            private$..desc <- desc
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
              desc = private$..desc,
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

            # Wait one second (for asynchronous calls)
            Sys.sleep(1)

            # Validation
            v <- ValidateClass$new()
            v$validate(cls = "NLPStudio", method = "addLab",
                       fieldName = "lab", value = lab, level = "Error",
                       msg = paste("Object is not a valid 'Lab' type."),
                       expect = "Lab")

            v <- ValidateLogical$new()
            v$validate(cls = "NLPStudio", method = "addLab",
                       fieldName = "current", value = current, level = "Error",
                       msg = paste("Invalid logical,", current, "must be TRUE or FALSE"),
                       expect = TRUE)

            # Add lab to lab list
            labData <- lab$getLab(verbose = FALSE)
            private$..labList[[labData$name]] <- lab
            private$..modified <- Sys.time()

            # Update Current
            if (current == TRUE) private$..currentLab <- lab

            # Update Cache
            nlpStudioCache$setCache("nlpStudio", self)

            invisible(self)

          },

          removeLab = function(lab) {

            if (class(lab) == "character") { labObject <- get(lab, envir = .GlobalEnv)}

            # Confirm lab is a lab
            v <- ValidateClass$new()
            v$validate(cls = "NLPStudio", method = "removeLab",
                       fieldName = "lab", value = labObject, level = "Error",
                       msg = paste("Object is not a valid 'lab' type."),
                       expect = "Lab")

            labData <- labObject$getLab(verbose = FALSE)

            # Confirm lab is not current
            if (identical(lab,private$..currentLab)) {
              v <- Validate0$new()
              v$notify(cls = "NLPStudio", method = "removeLab",
                       fieldName = "lab", value = labData$name, level = "Error",
                       msg = "Unable to remove a current lab.  See ?NLPStudio",
                       expect = "Lab")
            }

            # Archive, and remove lab, and update modified date
            rm(list = ls(envir = .GlobalEnv)[grep(lab, ls(envir = .GlobalEnv))], envir = .GlobalEnv)
            cache <- nlpStudioCache$loadCache()
            cache[[lab]] <- NULL
            nlpStudioCache$replaceCache(cache)
            nlpStudioCache$saveCache()
            rm(cache)
            private$..labList[[lab]] <- NULL
            private$..modified <- Sys.time()

            # Update Cache
            nlpStudioCache$setCache("nlpStudio", self)

            invisible(self)

          },

          listlabs = function(verbose = FALSE) {

            labs = rbindlist(lapply(private$..labList, function(e) {
              labData <- e$getLab(verbose = FALSE)
              l <- list(
                name = labData$name,
                desc = labData$desc,
                created = labData$created,
                modified = labData$modified
              )
              l
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
