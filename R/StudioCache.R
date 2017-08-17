## ---- Lab
#==============================================================================#
#                                 Cache                                        #
#==============================================================================#
#' Cache
#'
#' \code{Cache} Class for managing the cache for all NLP objects.
#'
#' Class and methods to create a cache environment, retrieve objects from
#' the cache, and write objects to the cache.  There are also methods for
#' obtaininig cached objects from a file and generating a list of the contents
#' of the cache environment.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{new()}}{Creates the cache environment.}
#'  \item{\code{get(key)}}{Retrieves object from cache.}
#'  \item{\code{set(key, value)}}{Save a key value pair to the cache environment.}
#'  \item{\code{load(filePath)}}{Loads data from file into the cache environment.}
#'  \item{\code{ls}}{Lists the items in the cache environment.}
#' }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
StudioCache <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "StudioCache",
        private = list(
          ..cache = character(0),
          ..cacheFile = "./.StudioCache.Rdata"
        ),
        public = list(
          initialize = function() private$..cache <- list(),
          getCache = function(key) private$..cache[[key]],
          setCache = function(key, value) private$..cache[[key]] <- value,
          purgeCache = function() private$..cache <- list(),
          loadCache = function() {
            name <- assign("c", load(file = private$..cacheFile))
            private$..cache <- get(c)
          },
          saveCache = function() {
            cache <- list()
            for (i in 1:length(private$..cache)) {
              cache[[names(private$..cache[i])]] <- private$..cache[i]
            }
            save(cache, file = private$..cacheFile)
          },
          lsCache = function() names(private$..cache),
          printCache = function() {
            cat("\n\n################################ CACHE #########################################\r")
            lapply(private$..cache, function(c) ({ print(c) }))
            cat("\n\n################################ CACHE #########################################\n")
          },
          globalize = function() {
            lapply(seq_along(private$..cache), function(c) {
              objName <- name(private$..cache[[c]])
              if (!exists(objName, envir = .GlobalEnv)) {
                assign(objName, value = private$..cache[[c]], envir = .GlobalEnv)
              }
            })
          },
          getInstance = function() invisible(self)
        )
      )
      super$initialize(...)
    }
  ),lock_objects = FALSE
)#$new()
