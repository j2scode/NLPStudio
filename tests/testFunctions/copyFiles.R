copyFiles <- function(dir) {
  fromDir <- "./tests/testFiles"
  listOfFiles <- list.files(fromDir,full.names = TRUE)
  file.copy(listOfFiles, dir)
}
