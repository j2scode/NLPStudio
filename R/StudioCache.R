#==============================================================================#
#                                 StudioCache                                  #
#==============================================================================#
#' StudioCache
#'
#' \code{Cache} Class for managing the cache for all NLP objects.
#'
#' Class and methods to create a cache environment, retrieve objects from
#' the cache, and write objects to the cache.  There are also methods for
#' obtaining cached objects from a file and generating a list of the contents
#' of the cache environment.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{initialize()}}{Creates the cache environment.}
#'  \item{\code{getCache(key)}}{Retrieves object from cache.}
#'  \item{\code{setCache(key, value)}}{Save a key value pair to the cache environment.}
#'  \item{\code{purgeCache()}}{Purges the entire cache.}
#'  \item{\code{loadCache()}}{Loads the cache from file.}
#'  \item{\code{saveCache()}}{Restores items in cache to the global environment.}
#'  \item{\code{restoreCache()}}{Purges the entire cache.}
#'  \item{\code{lsCache}}{Lists the names of the items in the cache environment.}
#'  \item{\code{printCache()}}{Prints the contents of the cache environment.}
#'  \item{\code{getAllCache()}}{Returns the entire cache environment.}
#'  \item{\code{getInstance()}}{Returns the instance of the singleton StudioCache class.}
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
          initialize = function() private$..cache <- new.env(parent = emptyenv()),
          getCache = function(key) unlist(private$..cache[[key]]),

          setCache = function(key, value) {
            private$..cache[[key]] <- value
            self$saveCache()
          },

          purgeCache = function() {
            private$..cache <- new.env(parent = emptyenv())
          },
          loadCache = function() {
            load(file = private$..cacheFile)
            private$..cache <- nlpStudioCache
            return(nlpStudioCache)
          },
          saveCache = function() {
            nlpStudioCache <- lapply(private$..cache, function(c) c)
            save(nlpStudioCache, file = private$..cacheFile)
          },
          replaceCache = function(newCache) {
            private$..cache <- newCache
          },
          restoreCache = function() {
            objNames <- names(private$..cache)
            lapply(seq_along(private$..cache), function(c) {
              objName <- objNames[c]
              assign(objName, private$..cache[[c]], .GlobalEnv)
            })
          },
          lsCache = function() names(private$..cache),
          printCache = function() {
            cat("\n\n################################ CACHE #########################################\r")
            print(self$getAllCache())
            cat("\n\n################################ CACHE #########################################\n")
          },
          getAllCache = function() {
            return(unlist(lapply(private$..cache, function(c) ({c}))))
          },
          getInstance = function() invisible(self)
        )
      )
      super$initialize(...)
    }
  ),lock_objects = FALSE
)#$new()
