checkCache <- function(name) {

  object <<- get(name, envir = .GlobalEnv)
  cache <<- nlpStudioCache$getCache(name)
  if (!all.equal(cache, object)) return(FALSE)

  load("./.StudioCache.Rdata")
  if (isTRUE(all.equal(nlpStudioCache[[name]], object)) != TRUE) return(FALSE)

  return(TRUE)
}

