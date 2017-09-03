checkState <- function(name) {

  object <<- get(name, envir = .GlobalEnv)
  state <<- nlpStudioState$getState(name)
  if (!all.equal(state, object)) return(FALSE)

  load("./NLPStudio/.State.Rdata")
  if (isTRUE(all.equal(nlpStudioState[[name]], object)) != TRUE) return(FALSE)

  return(TRUE)
}

