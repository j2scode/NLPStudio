testInit <- function() {

  # Remove directories
  base::unlink("./NLPStudio", recursive = TRUE)

  # Clear global environment
  rm(list = ls())

  # Load Package
  devtools::load_all()

  # Source Test Functions
  source("./tests/testFunctions/logTests.r")
  source("./tests/testFunctions/copyFiles.r")

}
testInit()
