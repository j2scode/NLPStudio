checkState <- function(name) {

  object <<- get(name, envir = .GlobalEnv)
  state <<- nlpStudioState$restoreState(name)
  if (!all.equal(state, object)) return(FALSE)
}

