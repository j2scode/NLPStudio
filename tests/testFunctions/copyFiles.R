copyFiles <- function(from, to) {
  # From should be a directory relative to ./tests/testFiles
  from <- file.path("./tests/testFiles", from)
  listOfFiles <- list.files(from, full.names = TRUE, recursive = TRUE)
  file.copy(listOfFiles, to)
}
