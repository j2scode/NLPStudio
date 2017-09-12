checkState <- function(name) {

  object <<- get(name, envir = .GlobalEnv)
  state <<- stateManager$restoreState(name)
  if (!all.equal(state, object)) return(FALSE)
}

