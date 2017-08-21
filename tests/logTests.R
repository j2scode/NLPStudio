logTests <- function(cls, mthd, note) {

  newPost <- data.frame(date = Sys.time(),
                        class = cls,
                        method = mthd,
                        note = note)
  if (!exists("testLog")) {
    testLog <<- newPost
  } else {
    testLog <<- rbind(testLog, newPost)
  }
}
