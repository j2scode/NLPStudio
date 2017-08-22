## ---- Lab
#==============================================================================#
#                                 Lab                                          #
#==============================================================================#
#' Lab
#'
#' \code{Lab} Class for managing collections of documents.
#'
#' The primary function of this class is to maintain collections of documents.
#' Methods have been developed to add, and remove document collections.
#' Additionally, functionality is included to get/print and archive the lab.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates an object of Lab Class}
#'  \item{\code{getLab(verbose = FALSE)}}{Retrieves the meta data for the Lab, as well as a list of its document collections. If verbose is false, a data frame is returned.  If verbose is true, data frame is returned and the lab and its colllections are printed to console.}
#'   \item{\code{archiveLab()}}{Compresses and stores the lab in the archive directory. This method is also called prior to removing a lab from the Studio. }
#'   \item{\code{addCollection(name, desc)}}{Adds a document collection to the list of collections in the lab}
#'   \item{\code{removeCollection(name, purge = FALSE)}}{Removes a document collection from the list and removes the collection from memory of purge = TRUE}
#' }
#'
#' @section Private Fields:
#' @param name A character string containing the name of the Lab
#' @param desc A chararacter string containing the description of the Lab
#' @param collections A list containing collection objects
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
Lab <- R6::R6Class(
  classname = "Lab",
  private = list(
    ..name = character(0),
    ..desc = character(0),
    ..collections = list(),
    ..modified = "None",
    ..created = "None"
  ),

  active = list(

    desc = function(value) {
      if (missing(value)) {
        private$..desc
      } else {
        private$..desc <- value
      }
      nlpStudioCache$setCache("nlpStudio")
      nlpStudioCache$setCache(private$..name, self)
    }
  ),

  public = list(

    initialize = function(name, desc = NULL) {

      # Validation
      v <- ValidationManager$new()
      v$validateName(cls = "NLPStudio", method = "initialize", name = name,
                     expect = FALSE)

      # Instantiate variables
      private$..name <- name
      if (is.null(desc)) { desc <- paste(name, "Lab") }
      private$..desc <- desc
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      # Update Cache
      nlpStudioCache$setCache(name, self)

      invisible(self)
    },

    getLab = function(verbose = TRUE) {
      lab = list(
        name = private$..name,
        desc = private$..desc,
        collections = self$listCollections(verbose = FALSE),
        modified = private$..modified,
        created = private$..created
      )

      if (verbose == TRUE) {
        cat("\n\n================================================================================",
            "\nLab: ", lab$name, " ", lab$desc, " ", " Created: ", format(lab$created), "Modified:", format(lab$modified))
        cat("\n---------------------------------------------------------------------------------",
            "\nCollections:\n")
        print.data.frame(lab$collections)
        cat("\n================================================================================\n")
      }
      return(lab)
    },

    archiveLab = function() {
      return("archived")

      # TODO: Finish archive class
      # TODO: Implement INFO logs in log directory established for the lab

      # a <- Archive$new()
      # object = list(
      #   name = private$..name,
      #   desc = paste0(private$..name, "-archived-Lab")
      # )
      # a$archive(object)
      # private$..modified <- Sys.time()

      # Update Cache
      # nlpStudioCache$setCache("nlpStudio")
      # nlpStudioCache$setCache(private$..name, self)
    },

    addCollection = function(collection) {

      # Validate
      v <- ValidateClass$new()
      v$validation(cls = "Lab", level = "Error", method = "addCollection",
                   fieldName = "collection", value = collection,
                   msg = paste("Invalid class. Collection class expected",
                               class(collection), "encountered."),
                   expect = "Collection")

      # Add collection to list of collections
      if (length(private$..collections) == 0) {
        private$..collections <- list(collection)
      } else {
        private$..collections <- list(private$..collections, list(collection))
      }

      # Update Cache
      nlpStudioCache$setCache("nlpStudio")
      nlpStudioCache$setCache(private$..name)

      invisible(self)

    },

    removeCollection = function(name, purge = FALSE) {

      private$..collections[[name]] <- NULL

      # Update Cache
      nlpStudioCache$setCache(private$..name)
      nlpStudioCache$setCache(private$..name)
    },

    listCollections = function(verbose = FALSE) {

      collections = rbindlist(lapply(private$..collections, function(c) {
        col <- list(
          name = c$private$..name,
          desc = c$private$..desc,
          modified = c$private$..modified,
          created = c$private$..created
        )
        col
      }))

      if (verbose == TRUE) {
        print.data.frame(collections)
      }
      return(collections)
    }
  ), lock_objects = FALSE
)
