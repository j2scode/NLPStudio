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
#'   \item{\code{addDocument(name, desc)}}{Adds a document collection to the list of collections in the lab}
#'   \item{\code{removeDocument(name, purge = FALSE)}}{Removes a document collection from the list and removes the collection from memory of purge = TRUE}
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
    ..documents = list(),
    ..modified = "None",
    ..created = "None",
    ..directories = list()
  ),

  active = list(

    desc = function(value) {
      if (missing(value)) {
        private$..desc
      } else {
        private$..desc <- value
      }
      nlpStudioCache$setCache("nlpStudio", nlpStudio)
      nlpStudioCache$setCache(private$..name, self)
    }
  ),

  public = list(

    initialize = function(name, desc = NULL) {

      # Load directories
      private$..directories <- nlpStudio$getDirectories()

      # Validate Name
      v <- ValidationManager$new()
      v$validateName(cls = "NLPStudio", method = "initialize", name = name,
                     expect = FALSE)

      # Confirm lab does not already exist
      v <- ValidateExists$new()
      v$validate(cls = "Lab", method = "initialize",
                 fieldName = "name", value = name, level = "Error",
                 msg = paste("Cannot create lab because", name,
                             "already exists.",
                             "See ?Lab"),
                 expect = FALSE)

      # Confirm directory does not exist
      v <- ValidatePath$new()
      v$validate(cls = "Lab", method = "initialize",
                 fieldName = "name",
                 value = file.path(private$..directories$labs, name),
                 level = "Error",
                 msg = paste("Cannot create lab because", name,
                             "directory already exists.",
                             "See ?Lab"),
                 expect = FALSE)

      # Instantiate variables
      private$..name <- name
      if (is.null(desc)) { desc <- paste(name, "Lab") }
      private$..desc <- desc
      private$..modified <- Sys.time()
      private$..created <- Sys.time()

      # Create lab directory
      dir.create(file.path(private$..directories$labs, name))

      # Assign its name in the global environment
      assign(name, self, envir = .GlobalEnv)

      # Update Cache
      nlpStudioCache$setCache(name, self)
      nlpStudioCache$setCache("nlpStudio", nlpStudio)

      invisible(self)
    },

    getLab = function(format = "object") {

      if (format == "object") {
        lab <- self
      } else if (format == "list") {
        lab = list(
          name = private$..name,
          desc = private$..desc,
          collections = self$getDocuments(format = "list"),
          modified = private$..modified,
          created = private$..created
        )
      } else if (format == "df") {
        lab = list(
          labDf = data.frame(name = private$..name,
                             desc = private$..desc,
                             modified = private$..modified,
                             created = private$..created,
                             stringsAsFactors = FALSE),
          collectionsDf = self$getDocuments(format = "df")
          )
      } else {
        v <- Validate0$new()
        v$notify(cls = "Lab", method = "getLab",
                 fieldName = "format", value = format, level = "Error",
                 msg = paste("Invalid format requested.",
                             "Must be 'object', 'list', or 'df'.",
                             "See ?Lab"),
                 expect = NULL)
      }

      return(lab)
    },

    enterLab = function() {
      nlpStudio$enterLab(self)
    },

    leaveLab = function() {
      nlpStudio$leaveLab(self)
    },

    printLab = function() {

      lab <- self$getLab(format = "df")

      cat("\n\n================================================================================",
          "\nLab:")
      print.data.frame(lab$labDf)
      cat("\n---------------------------------------------------------------------------------",
          "\nCollections:\n")
      print.data.frame(lab$collectionsDf)
      cat("\n================================================================================\n")
    },

    archiveLab = function() {

      filePath <- file.path(private$..directories$labs, private$..name)
      if(length(list.files(path = filePath, all.files = TRUE,
                          recursive = TRUE, include.dirs = TRUE)) > 0) {

        a <- Archive0$new()
        a$archive(fileName = private$..name,
                  filePath = file.path(private$..directories$archives,
                                       private$..name),
                  self)
      }
    },


    getDocuments = function(format = "object") {

      if (format == "object") {
        collections = lapply(private$..documents, function(c) c)
      } else if (format == "list") {
        collections = lapply(private$..documents, function(c) {
          c$getDocument(format = "list")
        })
      } else if (format == "df") {
        collections = rbindlist(lapply(private$..documents, function(c) {
          collections <- c$getDocument(format = "list")
          collectionsData = list(
            name = collections$name,
            desc = collections$desc,
            path = collections$path,
            created = collections$created,
            modified = collections$modified
          )
        }))
      } else {
        v <- Validate0$new()
        v$notify(cls = "Lab", method = "getDocuments",
                 fieldName = "format", value = format, level = "Error",
                 msg = paste("Invalid format requested.",
                             "Must be 'object', 'list', or 'df'.",
                             "See ?Lab"),
                 expect = NULL)
      }
      return(collections)
    },

    addDocument = function(document) {

      # Validate
      documentData <- document$getDocument(format = "list")
      v <- ValidateClass$new()
      v$validate(cls = "Lab", level = "Error", method = "addDocument",
                   fieldName = "document", value = document,
                   msg = paste("Invalid class. DocumentCollection class expected",
                               class(document), "encountered."),
                   expect = "DocumentCollection")

      # Add collection to list of collections
      private$..documents[[documentData$name]] <- document
      private$..modified <- Sys.time()

      # Update Cache
      nlpStudioCache$setCache("nlpStudio", nlpStudio)
      nlpStudioCache$setCache(private$..name, self)

      invisible(self)

    },

    removeDocument = function(document, purge = FALSE) {

      # Confirm parameter is a collection
      v <- ValidateClass$new()
      v$validate(cls = "Lab", method = "removeDocument",
                 fieldName = "document", value = document, level = "Error",
                 msg = paste("Object is not a valid DocumentCollection type."),
                 expect = "DocumentCollection")

      # Archive Lab
      self$archiveLab()

      # Remove collection from lab
      documentData <- document$getDocument(format = "list")
      private$..documents[[documentData$name]] <- NULL
      private$..modified <- Sys.time()

      if (purge == TRUE) {
        # Remove from global environment
        rm(list = ls(envir = .GlobalEnv)[grep(documentData$name, ls(envir = .GlobalEnv))], envir = .GlobalEnv)

        # Remove from cache
        cache <- nlpStudioCache$loadCache()
        cache[[documentData$name]] <- NULL
        nlpStudioCache$replaceCache(cache)
        nlpStudioCache$saveCache()
      }

      # Update Cache
      nlpStudioCache$setCache(private$..name, self)
      nlpStudioCache$setCache("nlpStudio", nlpStudio)

      invisible(self)

    }
  ), lock_objects = FALSE
)
