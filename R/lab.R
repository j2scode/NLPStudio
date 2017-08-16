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
#'  \item{\code{getLab(verbose = FALSE)}}{Retrieves the meta data for the Lab, as well as a list of its document collections. If verbose is false, a list of two data frames is returned.  If verbose is true, the data frames are additionally printed to console.
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

  public = list(

    initialize = function(name, desc = NULL, current = FALSE) {

      # Validation
      v <- ValidateNoSpaces$new()
      v$validate(cls = "NLPStudio", level = "Error", method = "initializes",
                 fieldName = "name", value = name,
                 msg = "Lab names must contain alphanumeric characters and no spaces.",
                 expect = TRUE)

      v <- ValidateExists$new()
      v$validate(cls = "NLPStudio", level = "Error",
                 method = "initialize", fieldName = "name",
                 value = name, msg = paste(
                   "Lab name", name, "already exists."),
                 expect = FALSE)

      v <- ValidateLogical$new()
      v$validate(cls = "NLPStudio", level = "Error",
                 method = "initialize", fieldName = "current",
                 value = current, msg = paste(
                   "Current must be a valid logical"),
                 expect = TRUE)

      private$..name <- name
      if (is.null(desc)) { desc <- paste(name, "Lab") }
      private$..desc <- desc
      private$..modified <- Sys.time()
      private$..created = Sys.time()

      # Bind Lab in global environment
      assign(name, self, envir = .GlobalEnv)

      # Add Lab to NLPStudio
      nlpStudio$addLab(self, current)

      invisible(self)
    },

    getLab = function(verbose = FALSE) {
      Lab = data.frame(
        parent = private$..parent,
        name = self$name,
        desc = self$desc,
        path = private$..path,
        created = private$..created,
        archived = private$..archived,
        stringsAsFactors = FALSE
      )

      if (verbose == TRUE) {
        print.data.frame(Lab)
      }

      return(Lab)
    },

    archiveLab = function() {

      a <- Archive0$new()
      object = list(
        name = self$name,
        desc = paste0(self$name, "-archived-Lab"),
        path = self$path)
      a$archive(object)
      private$..archived <- Sys.time()
    },

    addCollection = function(collection) {

      private$validate(what = "class", cls = "Lab",
                       level = "Error", method = "addCollection",
                       fieldName = "collection", value = collection,
                       expect = "Collection")

      # Update collection path
      collection$parent <- self$name
      collection$path <- file.path(
        private$..path, gsub(" ", "-", collection$name))

      # Add collection to list of collections
      if (length(private$..collections) == 0) {
        private$..collections <- list(collection)
      } else {
        private$..collections <- list(private$..collections, list(collection))
      }

      # Create Directory
      dir.create(collection$path)

    },

    searchCollections = function(name) {
      if (length(private$..collections) > 0) {
        for (i in 1:length(private$..collections)) {
          if (private$..collections[[i]]$name == name) { return(i) }
        }
        return(FALSE)
      } else {
        return(FALSE)
      }
    },

    removeCollection = function(name) {

      self$archiveLab()
      collectionIdx <- searchStudio(name)
      private$..collections[[collectionIdx]] <- NULL
      rm(name, Labir = .GlobalEnv)
    },

    listCollections = function(verbose = FALSE) {

      collections = rbindlist(lapply(private$..collections, function(c) {
        col <- list(
          name = c[[1]]$name,
          description = c[[1]]$desc,
          path = c[[1]]$path,
          created = c[[1]]$created
        )
        col
      }))

      if (verbose == TRUE) {
        print.data.frame(collections)
      }
      return(collections)
    }
  )
)
