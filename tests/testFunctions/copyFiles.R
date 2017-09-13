copyFiles <- function(dir) {
  fromDir <- "./tests/testFiles"
  listOfFiles <- list.files(fromDir,full.names = TRUE, recursive = TRUE)
  file.copy(listOfFiles, dir)
}
