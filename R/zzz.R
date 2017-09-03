.onLoad <- function(libname, pkgname) {

  # Instantiate Singleton Classes
  if (file.exists("./NLPStudio/.State.Rdata")) {
    packageStartupMessage("\nWelcome back to the NLPStudio (Beta)!\n\n")
    nlpStudioState <<- StateManager$new()$getInstance()
    nlpStudio <<- NLPStudio$new()$getInstance()
    nlpStudioState$loadState()
  } else {
    packageStartupMessage("\nWelcome to the NLPStudio (Beta)!")
    nlpStudioState <<- StateManager$new()$getInstance()
    nlpStudio <<- NLPStudio$new()$getInstance()
    nlpStudioState$setState("nlpStudio", nlpStudio)
  }
}
