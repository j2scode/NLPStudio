#==============================================================================#
#                             StudioCacheManager                               #
#==============================================================================#
#' StudioCacheManager
#'
#' \code{Cache} Class for interacts with the StudioCache class to ensure that the cache is saved to disk whenever the cache has changed
#'
#' Class and methods to read, write, and save objects to, from and of the StudioCache.
#'
#' @section Methods:
#' \describe{
#'  \item{\code{initialize()}}{Creates the cache environment.}
#'  \item{\code{getCache(key)}}{Retrieves object from cache.}
#'  \item{\code{setCache(key, value)}}{Save a key value pair to the cache environment.}
#'  \item{\code{getInstance()}}{Returns the instance of the singleton StudioCache class.}
#' }
#'
#' @docType class
#' @author John James, \email{jjames@@datasciencesalon.org}
#' @export
StudioCacheManager <- R6::R6Class(
  "SingletonContainer",
  portable = FALSE,
  inherit = Singleton,
  public = list(
    initialize = function(...) {
      Class <<- R6::R6Class(
        classname = "StudioCache",
        private = list(
          ..cacheManager = character(0)
        ),
        public = list(
          initialize = function() private$..cacheManager <- new.env(parent = emptyenv()),

          getCache = function(key) {
            return(studioCache$getCache(key))
          },

          setCache = function(key, value) {
            studioCache$setCache(key, value)
            studioCache$saveCache()
          },
          getInstance = function() invisible(self)
        )
      )
      super$initialize(...)
    }
  ),lock_objects = FALSE
)#$new()
